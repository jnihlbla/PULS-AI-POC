000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W6011510.                                                
000400*AUTHOR.         LARS THELL.                                              
000500*DATE-WRITTEN.   92/04/01.                                                
000600*                                                                         
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        AKTIVERAR FÖLJESEDLAR VID MOTTAGNING                             
001100*                                                                         
001200*        PROGRAMMET          LÄSER      WLARTC (WDK6)                     
001300*        PROGRAMMET          LÄSER      WLARTS (WDK7)                     
001400*        PROGRAMMET          LÄSER      WLINLB (WDD9)                     
001500*        PROGRAMMET          LÄSER      W6PLAA (W6G1)                     
001600*        PROGRAMMET          LÄSER      WDJ1                              
001700*        PROGRAMMET          LÄSER      WDK9                              
001800*        PROGRAMMET          LÄSER      WDL2                              
001900*        PROGRAMMET          LÄSER      WDD9                              
002000*        PROGRAMMET          UPPDATERAR W6INLA (W6D1)                     
002100*        PROGRAMMET          UPPDATERAR W6LASA (W6G2)                     
002200*    SUB PROGRAMMET W006KOM  UPPDATERAR WLKOMA (WDP8)                     
002300*    SUB PROGRAMMET W611STYR LÄSER      W6HANB (W6G1)                     
002400*                                       W6PLAA (W6G1)                     
002500*    SUB PROGRAMMET W611PRIO UPPDATERAR W6INLA (W6D1)                     
002600*                            LÄSER      WLARTC (WDK6)                     
002700*                            LÄSER      WLARTM (WDK9)                     
002800*                            LÄSER      WLORDQ (WDA5A)                    
002900*    SUB PROGRAMMET W426KNTR LÄSER      WLARTC (WDK6)                     
003000*                            LÄSER      W6KVAE (W6H7)                     
003100*                            LÄSER      W6PROA (W6G1)                     
003200*                            LÄSER      W6LEVA (W6F1)                     
003300*                            UPPDATERAR W6KVAH (W6D2)                     
003400*                            UPPDATERAR W6UPFA (W6L1)                     
003500*                            UPPDATERAR WLXXLA (WDR1)                     
003600*                            UPPDATERAR W6KODA (W6G2)                     
003700*                                                                         
003800*    INDATA.                                                              
003900*        TRANSAKTION: W6T115                                              
004000*        MID:         W6I11501                                            
004100*                                                                         
004200*    UTDATA.                                                              
004300*        MOD:         W6O11501                                            
004400                                                                          
004500     SKIP3                                                                
004600 ENVIRONMENT DIVISION.                                                    
004700     EJECT                                                                
004800 DATA DIVISION.                                                           
004900 WORKING-STORAGE SECTION.                                                 
005000                                                                          
005100*    -- CHECKED BY WY2000                                                 
005200 77  IDPGM                       PIC X(08)   VALUE 'W6011510'.            
005300                                                                          
005400*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
005500 77  FILLER                      PIC X(8)  VALUE 'ERRORTEX'.              
005600 77  ERRORTEXT                   PIC X(80) VALUE SPACE.                   
005700                                                                          
005800 77  FILLER                      PIC X(8)  VALUE 'CURRENT:'.              
005900 77  FILLER                      PIC X(8)  VALUE 'IMS-SEC'.               
006000                                                                          
006100 77  JA                          PIC X       VALUE 'J'.                   
006200 77  YES                         PIC X       VALUE 'Y'.                   
006300 77  NEJ                         PIC X       VALUE 'N'.                   
006400 77  SEP-TPO-SDC-NDC             PIC X(2)    VALUE '21'.                  
006500 77  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
006600 77  DAGENS-TID                  PIC 9(8)    VALUE ZERO.                  
006700 01  WS-IDDC-LOCAL.                                                       
006800     03  FILLER                  PIC X(5)   VALUE 'WIDDC'.                
006900     03  WS-IDDC-LOCAL-DATE      PIC X(2).                                
007000     03  FILLER                  PIC X(1)   VALUE SPACE.                  
007100 01  WS-DAT-TIVVD                PIC 9(3)   VALUE ZERO.                   
007200 01  WS-DAT-TID                  PIC 9      VALUE ZERO.                   
007300 01  WS-DAT-TIAAVV-GRP.                                                   
007400     03  WS-DAT-TIAAVV           PIC 9(4)   VALUE ZERO.                   
007500 01  WS-TIORDTIME.                                                        
007600     05 ORDDATE  PIC 9(6)             VALUE ZERO.                         
007700     05 ORDTIME  PIC 9(6)             VALUE ZERO.                         
007800                                                                          
007900*    --- INDEX FÖR BLÄDDRINGSRADER                                        
008000 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
008100 77  MAX-INDX                    PIC S9(4)  VALUE +10   COMP SYNC.        
008200 77  IX                          PIC  9(1)  VALUE ZERO.                   
008300 77  6191-IX                     PIC S9(4)  VALUE +0    COMP SYNC.        
008400 77  MAX-6191-IX                 PIC S9(4)  VALUE +15   COMP SYNC.        
008500 77  6192-IX                     PIC S9(4)  VALUE +0    COMP SYNC.        
008600 77  6196-IX                     PIC S9(4)  VALUE +0    COMP SYNC.        
008700 77  MAX-6196-IX                 PIC S9(4)  VALUE +15   COMP SYNC.        
008800 77  6197-IX                     PIC S9(4)  VALUE +0    COMP SYNC.        
008900 77  MAX-6197-IX                 PIC S9(4)  VALUE +15   COMP SYNC.        
009000 77  6199-IX                     PIC S9(4)  VALUE +0    COMP SYNC.        
009100 77  6199-MAX-IX                 PIC S9(4)  VALUE +12   COMP SYNC.        
009200 77  RAD-IX                      PIC S9(4)  VALUE +0    COMP SYNC.        
009300 77  RAD-MAX-IX                  PIC S9(4)  VALUE +12   COMP SYNC.        
009400 77  MAX-ADINLOMR-IX             PIC S9(4)  VALUE +50   COMP SYNC.        
009500 77  MAX-LPL-IX                  PIC S9(4)  VALUE +10   COMP SYNC.        
009600 77  MAX-RESP-LPL-IX             PIC S9(4)  VALUE +5    COMP SYNC.        
009700 77  RESP-LPL-IX                 PIC S9(4)  VALUE +0    COMP SYNC.        
009800 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
009900 77  MAX-MOD-LAENGD              PIC S9(4)  VALUE +927  COMP SYNC.        
010000 77  LNG-P-TO-P-PREFIX           PIC S9(4)  VALUE +17   COMP SYNC.        
010100 77  WS-FLWEBDC                  PIC X(1)   VALUE SPACE.                  
010200                                                                          
010300 77  I-TAB-IX                    PIC S9(4)  VALUE +0    COMP SYNC.        
010400 77  I-TAB-IX-MAX                PIC S9(4)  VALUE +0    COMP SYNC.        
010500 77  I-TAB-MAX-IX                PIC S9(4)  VALUE +150  COMP SYNC.        
010600                                                                          
010700*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
010800 77  WS-IDLEVNR                  PIC X(5)    VALUE SPACE.                 
010900 77  WS-IDFS                     PIC X(8)    VALUE SPACE.                 
011000 77  WS-TIAVIDAT                 PIC X(6)    VALUE SPACE.                 
011100 77  WS-IDLBBET                  PIC X(12)   VALUE SPACE.                 
011200 77  WS-ADINLOMR-PRT             PIC X(4)    VALUE SPACE.                 
011300 77  WS-KDMFSFOR                 PIC X(1)    VALUE SPACE.                 
011400                                                                          
011500 77  WS-IDLEVNR-2                PIC X(5)    VALUE SPACE.                 
011600 77  W-TIAVIDAT                  PIC S9(7)   VALUE ZERO COMP-3.           
011700 77  W-IDFS                      PIC  X(8)   VALUE SPACE.                 
011800 77  W-KVPARTI                   PIC S9(7)   VALUE ZERO COMP-3.           
011900 77  W-TIAVIDAT-RAD              PIC 9(7)    VALUE ZERO.                  
012000 77  W-KDVVKL                    PIC S9(1)   VALUE ZERO COMP-3.           
012100 77  W-KVPB-SEP-CL               PIC S9(6)V9 VALUE ZERO COMP-3.           
012200 77  W-KVPB-SATS-CL              PIC S9(6)V9 VALUE ZERO COMP-3.           
012300 77  W-KVPB-TPO-CL               PIC S9(6)V9 VALUE ZERO COMP-3.           
012400 77  W-FLFSP-CL                  PIC X(1)    VALUE 'N'.                   
012500 77  W-FLEJBUFF                  PIC X(1)    VALUE 'N'.                   
012600 77  W-ADINLOMR-BOA              PIC X(4)    VALUE SPACE.                 
012700 77  FLSLUT                      PIC X(1)    VALUE 'N'.                   
012800 77  HELP-FLSLUT                 PIC X(1)    VALUE 'N'.                   
012900 77  W-OFOERAEDLAT               PIC S9(7)   VALUE ZERO COMP-3.           
013000 77  WS-TPOBEHOV                 PIC S9(7)   VALUE ZERO COMP-3.           
013100 77  WS-SATSBEHOV                PIC S9(7)   VALUE ZERO COMP-3.           
013200 77  WS-SATSBEHOV-ROS            PIC S9(7)   VALUE ZERO COMP-3.           
013300 77  WS-SATSBEHOV-SAVE           PIC S9(7)   VALUE ZERO COMP-3.           
013400 77  WS-KVBUFF-OF                PIC S9(7)   VALUE ZERO COMP-3.           
013500 77  WS-KVAR                     PIC S9(7)   VALUE ZERO COMP-3.           
013600 77  WS-KVROS-RES                PIC S9(7)   VALUE ZERO COMP-3.           
013700 77  WS-TPO-BEHOV                PIC S9(7)   VALUE ZERO COMP-3.           
013800 77  W-SUMMA-PRIO-OFR            PIC S9(7)   VALUE ZERO COMP-3.           
013900 77  W-PROCENT-SATS              PIC S9(1)V9(2) VALUE ZERO COMP-3.        
014000 77  W-MAX-PROCENT-LPL           PIC S9(1)V9(2) VALUE ZERO COMP-3.        
014100 77  W-TOT-KVPB-CL               PIC S9(6)V9 VALUE ZERO COMP-3.           
014200 77  W-KVKVAR-ATT-STYRA          PIC S9(7)   VALUE ZERO COMP-3.           
014300 77  W-KVAVIS-FB                 PIC S9(7)   VALUE ZERO COMP-3.           
014400 77  W-KVAVIS-FP                 PIC S9(7)   VALUE ZERO COMP-3.           
014500 77  W-KVAVIS-LO                 PIC S9(7)   VALUE ZERO COMP-3.           
014600 77  W-KVAVIS-BO                 PIC S9(7)   VALUE ZERO COMP-3.           
014700 77  W-KVAVIS-OVR                PIC S9(7)   VALUE ZERO COMP-3.           
014800 77  W-KVAVIS-PRIO-EGET          PIC S9(7)   VALUE ZERO COMP-3.           
014900 77  W-KVAVIS-KVAR               PIC S9(7)   VALUE ZERO COMP-3.           
015000 77  W-KVAVROP-SUM               PIC S9(7)   VALUE ZERO COMP-3.           
015100 77  WS-KVANTAL-CD               PIC S9(7)   VALUE ZERO COMP-3.           
015200 77  W-LPL-FB                    PIC  X(4)   VALUE SPACE.                 
015300 77  W-LPL-FP                    PIC  X(4)   VALUE SPACE.                 
015400 77  W-LPL-LO                    PIC  X(4)   VALUE SPACE.                 
015500 77  W-LPL-BO                    PIC  X(4)   VALUE SPACE.                 
015600 77  W-ADINLOMR-BO               PIC  X(4)   VALUE SPACE.                 
015700 77  W-ADINLOMR-OVR              PIC  X(4)   VALUE SPACE.                 
015800 77  W-TOT-VLARTNTO              PIC S9(9)V9(1) COMP-3 VALUE ZERO.        
015900 77  W-BIL-VLARTNTO              PIC S9(9)V9(1) COMP-3 VALUE ZERO.        
016000 77  W-SPAR-LPL                  PIC  X(4)   VALUE SPACE.                 
016100 77  W-SPAR-ADLAGOMR             PIC 9(2)     VALUE ZEROS.                
016200 77  W-JFR-ADLAGOMR              PIC S9(3)    VALUE ZEROS COMP-3.         
016300 77  W-ADLAGOMR                  PIC 9(2)    VALUE ZERO.                  
016400 77  W-IDILIST                   PIC 9(5)     VALUE ZEROS.                
016500 77  W-KVRADER                   PIC 9(5)     VALUE ZEROS.                
016600 77  W-SPAR-TORG                 PIC X(4)     VALUE SPACE.                
016700 77  W-KVPOST-I-LISTA            PIC 9(7)     VALUE ZERO.                 
016800 77  W-IDPRTLST                  PIC X(8)     VALUE SPACE.                
016900 77  W-IDPRTLST-VAL              PIC X(2)     VALUE SPACE.                
017000 77  W-KVUTSKR-AR                PIC S9(3)    VALUE ZERO.                 
017100 77  DATUM-PASSERAT              PIC X        VALUE 'N'.                  
017200 77  X-DAGAR                     PIC S9(2)    VALUE 30.                   
017300 77  WS-REDAN-UTTAGET            PIC S9(7)    VALUE ZERO COMP-3.          
017400 77  FL-UTTAG                    PIC X(1)    VALUE 'J'.                   
017500 77  FL-BUFFOMR                  PIC X(1)    VALUE 'J'.                   
017600 77  WS-KVBEHOV-CD               PIC S9(7)V9(2) VALUE ZERO COMP-3.        
017700 77  WS-IDARTNR-DISP             PIC 9(9)     VALUE ZERO.                 
017800 01  WS-CD.                                                               
017900    03  WS-AK-CD                 OCCURS 4 TIMES                           
018000                                 PIC S9(7)   VALUE ZERO COMP-3.           
018100                                                                          
018200 01  SPAR-6110-KVAVIS            PIC S9(7)    VALUE +0 COMP-3.            
018300 01  SPAR-6110-KVAVIS-PRIO       PIC S9(7)    VALUE +0 COMP-3.            
018400 01  SPAR-6110-KVAVIS-KIT        PIC S9(7)    VALUE +0 COMP-3.            
018500 01  SPAR-IDRADNR-INL            PIC S9(5)    VALUE ZERO COMP-3.          
018600 01  HELP-IDRADNR                PIC S9(5)    VALUE ZERO COMP-3.          
018700 01  HELP-IDRADNR-NY             PIC S9(5)    VALUE ZERO COMP-3.          
018800 01  HELP-KVINLART               PIC S9(7)    VALUE ZERO COMP-3.          
018900                                                                          
019000 01  W-IDLOPNRM                  PIC 9(9)    VALUE ZERO.                  
019100                                                                          
019200 01  WS-TILLDATUM                PIC 9(4).                                
019300                                                                          
019400 01  WS-TILLDATUM-GRP REDEFINES WS-TILLDATUM.                             
019500  03 WS-TILLD-AA                 PIC 9(2).                                
019600  03 WS-TILLD-VV                 PIC 9(2).                                
019700                                                                          
019800 01  W-IDLOPNRM-LOPA             PIC 9(9)    VALUE ZERO.                  
019900                                                                          
020000 01  W-0VVDLLLLK  REDEFINES W-IDLOPNRM-LOPA.                              
020100  03 FILLER                      PIC 9(1).                                
020200  03 W-VVD                       PIC 9(3).                                
020300  03 W-LLLL                      PIC 9(4).                                
020400  03 W-K                         PIC 9(1).                                
020500                                                                          
020600 01  FLT-FOR-BER-AV-IDLOPNRM.                                             
020700     03  FLT-LGD                 PIC S9  COMP SYNC   VALUE +7.            
020800     03  VAEGNINGSTAL            PIC 9(7)        VALUE 2121212.           
020900     03  VAEGNTAL-LGD            PIC S9  COMP SYNC   VALUE +7.            
021000     03  MODUL-10-11             PIC 9(2)            VALUE 10.            
021100     03  ALT-A-B                 PIC X(1)            VALUE 'B'.           
021200                                                                          
021300*    --- TABELL FÖR ADINLOMR                                              
021400 01  FILLER.                                                              
021500  03 W-ADINLOMR-TAB     OCCURS 50 INDEXED BY ADINLOMR-IX.                 
021600     05  W-ADINLOMR              PIC X(4)         VALUE SPACE.            
021700     05  W-VLARTNTO-ADINLOMR     PIC S9(9)V9(1) COMP-3 VALUE ZERO.        
021800     SKIP2                                                                
021900*    --- TABELL FÖR LPL                                                   
022000 01  LPL-TABELL.                                                          
022100  03 W-LPL-TAB          OCCURS 10 INDEXED BY LPL-IX.                      
022200     05  W-SUPROC-LPL           PIC S9(1)V9(2)  COMP-3 VALUE ZERO.        
022300     05  W-VLARTNTO-LPL         PIC S9(10)V9(1) COMP-3 VALUE ZERO.        
022400     05  W-LPL                  PIC X(4)        VALUE SPACE.              
022500     SKIP2                                                                
022600 77  INDATA-SW                   PIC X       VALUE 'J'.                   
022700     88  INDATA-OK                           VALUE 'J'.                   
022800     88  INDATA-FEL                          VALUE 'N'.                   
022900                                                                          
023000 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
023100     88  NYCKLAR-OK                          VALUE 'J'.                   
023200     88  NYCKLAR-FEL                         VALUE 'N'.                   
023300                                                                          
023400 77  KDCMDVAL-SW                 PIC X       VALUE 'N'.                   
023500     88  KDCMDVAL-EJ-IFYLLD                  VALUE 'N'.                   
023600                                                                          
023700 77  AKTIVERING-SW               PIC X       VALUE 'A'.                   
023800     88  AKTIVERING-AVI                      VALUE 'A'.                   
023900     88  AKTIVERING-BIL                      VALUE 'B'.                   
024000                                                                          
024100 77  INL-LAES-SW                 PIC X       VALUE '0'.                   
024200     88  INLB-LAES                           VALUE '1'.                   
024300     88  INLA-LAES                           VALUE '2'.                   
024400     88  INLA-SEQ-LAES                       VALUE '3'.                   
024500                                                                          
024600 77  FOERSTA-RAD-UPD-SW          PIC X       VALUE 'J'.                   
024700     88  FOERSTA-RAD-UPD                     VALUE 'J'.                   
024800                                                                          
024900 77  FOERSTA-6191-SW             PIC X       VALUE 'J'.                   
025000     88  FOERSTA-6191                        VALUE 'J'.                   
025100                                                                          
025200 77  FOERSTA-6196-SW             PIC X       VALUE 'J'.                   
025300     88  FOERSTA-6196                        VALUE 'J'.                   
025400                                                                          
025500 77  FOERSTA-6197-SW             PIC X       VALUE 'J'.                   
025600     88  FOERSTA-6197                        VALUE 'J'.                   
025700                                                                          
025800 77  FOERSTA-6199-SW             PIC X       VALUE 'J'.                   
025900     88  FOERSTA-6199                        VALUE 'J'.                   
026000                                                                          
026100 77  LOSSLISTA-SW                PIC X       VALUE 'J'.                   
026200     88  LOSSLISTA                           VALUE 'J'.                   
026300                                                                          
026400 77  ILISTA-SW                   PIC X       VALUE 'N'.                   
026500     88  ILISTA                              VALUE 'J'.                   
026600                                                                          
026700 77  TAB-TRAEFF-SW               PIC X       VALUE 'N'.                   
026800     88  TAB-TRAEFF                          VALUE 'J'.                   
026900     88  EJ-TAB-TRAEFF                       VALUE 'N'.                   
027000                                                                          
027100 77  REQU-KDCMDVAL-SW            PIC X       VALUE 'N'.                   
027200     88  REQU-KDCMDVAL-ALL-PLUS              VALUE 'J'.                   
027300                                                                          
027400 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
027500     88  EGEN-MID                            VALUE '6115'.                
027600     88  GODK-MID                            VALUE '6111' '6112'          
027700                                                   '6113' '6114'          
027800                                                   '6115' '6116'          
027900                                                   '6118' '6119'.         
028000     88  HELP-MID                            VALUE '0551'.                
028100     EJECT                                                                
028200*      --- VALID IDDC CODES                                               
028300*                                                                         
028400*01    -COPY WWDCKONS                                                     
028500*01    -COPY WWDC99                                                       
028600*01    -COPY WWDC99 -PRE SW-                                              
028700       EJECT                                                              
028800*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
028900 01  GENERELLA-SUBPROGRAM.                                                
029000     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
029100     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
029200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
029300     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
029400     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
029500     03  WINTSOR                 PIC X(8)    VALUE 'WINTSOR '.            
029600     03  CHECK                   PIC X(8)    VALUE 'CHECK   '.            
029700     03  W006KOM                 PIC X(8)    VALUE 'W006KOM '.            
029800     03  W006PRT                 PIC X(8)    VALUE 'W006PRT '.            
029900     03  W426KNTR                PIC X(8)    VALUE 'W426KNTR'.            
030000     03  W611PRIO                PIC X(8)    VALUE 'W611PRIO'.            
030100     03  W611STYR                PIC X(8)    VALUE 'W611STYR'.            
030200     03  WORKDAY                 PIC X(8)    VALUE 'WORKDAY '.            
030300     03  W61154                  PIC X(8)    VALUE 'W61154'.              
030400     03  W27125                  PIC X(8)    VALUE 'W27125'.              
030500     03  W22222                  PIC X(8)    VALUE 'W22222'.              
030600     03  WL01TIDZ                PIC X(8)    VALUE 'WL01TIDZ'.            
030700     03  W488ORCR                PIC X(8)    VALUE 'W488ORCR'.            
030800     EJECT                                                                
030900*    --- PARAMETERS TO ABEND                                              
031000                                                                          
031100 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +33.              
031200 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
031300 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
031400     SKIP3                                                                
031500*                                                                         
031600 01  FILLER                    PIC X(08) VALUE 'TIDZAREA'.                
031700*01  -COPY WL01TIDZ  -PRE TIDZ-                                           
031800     EJECT                                                                
031900*                                                                         
032000 01  FILLER                    PIC X(08) VALUE 'WDATAREA'.                
032100*    --- PARAMETRAR TILL SUBPROGRAM WDATKONV                              
032200*01 -COPY WDATAREA                                                        
032300     EJECT                                                                
032400*    --- PARAMETRAR TILL SUBPROGRAM W006PRT                               
032500*01 -COPY W006PRT                                                         
032600     EJECT                                                                
032700*01  -COPY W426KNTR                                                       
032800     EJECT                                                                
032900*01  -COPY W611PRIO                                                       
033000     EJECT                                                                
033100*01  -COPY W611STYR                                                       
033200     EJECT                                                                
033300 01  FILLER             PIC X(16) VALUE 'WORKAREA     '.                  
033400*01   -COPY WORKAREA.                                                     
033500     EJECT                                                                
033600 01  FILLER             PIC X(16) VALUE 'W61154       '.                  
033700*01  -COPY W61154 -PRE W61154-                                            
033800     EJECT                                                                
033900 01  FILLER             PIC X(16) VALUE 'W27125       '.                  
034000*01  -COPY W27125 -PRE W27125-                                            
034100     EJECT                                                                
034200*    --- PARAMETERS FOR SUB PROGRAM W488ORCR                              
034300*                                                                         
034400 01  FILLER                      PIC X(16)   VALUE 'W488ORCR'.            
034500*01 -COPY W488ORCR                                                        
034600     EJECT                                                                
034700*                                                                         
034800 01  TABENTRY-PARM.                                                       
034900     03  TABENTRY-LNGD           PIC S9(9)   COMP.                        
035000     03  ANTAL-ENTRY             PIC S9(9)   COMP.                        
035100     03  SORTBGP-LNGD            PIC S9(9)   COMP.                        
035200                                                                          
035300*----TABELL FÖR SKAPANDE AV INLÄGGNINGSLISTA                              
035400 01  INL-TAB.                                                             
035500     03  INL-POST  OCCURS 150.                                            
035600         05  I-TAB-SORTNYCKEL.                                            
035700           07  I-TAB-ADLAGOMR     PIC S9(3)   VALUE ZERO COMP-3.          
035800           07  I-TAB-TORG         PIC  X(4)   VALUE '0000'.               
035900           07  I-TAB-ADGANG       PIC S9(3)   VALUE ZERO COMP-3.          
036000           07  I-TAB-ADPLATS      PIC S9(5)   VALUE ZERO COMP-3.          
036100           07  I-TAB-IDARTNR      PIC S9(9)   VALUE ZERO COMP-3.          
036200           07  I-TAB-IDLEVNR-KOLLI PIC X(5)   VALUE SPACE.                
036300           07  I-TAB-IDOKOLLI     PIC  9(9)   VALUE ZERO.                 
036400         05  I-TAB-IDLEVNR        PIC  X(5)   VALUE SPACE.                
036500         05  I-TAB-IDFS           PIC  X(8)   VALUE SPACE.                
036600         05  I-TAB-TIAVIDAT       PIC S9(7)   VALUE ZERO COMP-3.          
036700         05  I-TAB-IDRADNR-INL    PIC S9(5)   VALUE ZERO COMP-3.          
036800         05  I-TAB-IDRADNR        PIC S9(5)   VALUE ZERO COMP-3.          
036900         05  I-TAB-IDILIST        PIC  9(5)   VALUE ZERO.                 
037000         05  I-TAB-IDILIRAD       PIC S9(5)   VALUE ZERO COMP-3.          
037100         05  I-TAB-IDPRTLST       PIC  X(8)   VALUE SPACE.                
037200*                                                                         
037300 SKIP2                                                                    
037400*----TOMTABELL FÖR INLÄGGNINGSLISTETABELL                                 
037500 01  INL-TAB-TOM.                                                         
037600     03  INL-TOM-POST  OCCURS 150.                                        
037700         05  I-TOM-SORTNYCKEL.                                            
037800           07  I-TOM-ADLAGOMR     PIC S9(3)   VALUE ZERO COMP-3.          
037900           07  I-TOM-TORG         PIC  X(4)   VALUE '0000'.               
038000           07  I-TOM-ADGANG       PIC S9(3)   VALUE ZERO COMP-3.          
038100           07  I-TOM-ADPLATS      PIC S9(5)   VALUE ZERO COMP-3.          
038200           07  I-TOM-IDARTNR      PIC S9(9)   VALUE ZERO COMP-3.          
038300           07  I-TOM-IDLEVNR-KOLLI PIC X(5)   VALUE SPACE.                
038400           07  I-TOM-IDOKOLLI     PIC  9(9)   VALUE ZERO.                 
038500         05  I-TOM-IDLEVNR        PIC X(5)    VALUE SPACE.                
038600         05  I-TOM-IDFS           PIC  X(8)   VALUE SPACE.                
038700         05  I-TOM-TIAVIDAT       PIC S9(7)   VALUE ZERO COMP-3.          
038800         05  I-TOM-IDRADNR-INL    PIC S9(5)   VALUE ZERO COMP-3.          
038900         05  I-TOM-IDRADNR        PIC S9(5)   VALUE ZERO COMP-3.          
039000         05  I-TOM-IDILIST        PIC  9(5)   VALUE ZERO.                 
039100         05  I-TOM-IDILIRAD       PIC S9(5)   VALUE ZERO COMP-3.          
039200         05  I-TOM-IDPRTLST       PIC  X(8)   VALUE SPACE.                
039300*                                                                         
039400 EJECT                                                                    
039500 01  ANTAL-I-TABELL              PIC S9(9)   COMP VALUE ZERO.             
039600     EJECT                                                                
039700*    *************************************                                
039800*    **  LINK-AREA                      **                                
039900*    **  BEHOVSTABELL                   **                                
040000*    *************************************                                
040100*01  AREA  -COPY W222L222   -PRE LINK-.                                   
040200*                                                                         
040300 01  BEHOVSTYPER.                                                         
040400     03  ENDAST-SEPARATBEHOV     PIC  X(2)   VALUE '01'.                  
040500*                                                                         
040600 EJECT                                                                    
040700 01  FILLER                      PIC X(16) VALUE 'MEDDELANDEN'.           
040800 01  MESSAGE-CODES.                                                       
040900     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '020'.                 
041000     03  INF-PRESS-PF11          PIC X(3)    VALUE '013'.                 
041100     03  ERR-NOT-FOUND           PIC X(3)    VALUE '025'.                 
041200     03  INF-FIRST-PAGE          PIC X(3)    VALUE '010'.                 
041300     03  ERR-UPDATE-FORBIDDEN    PIC X(3)    VALUE '007'.                 
041400     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '014'.                 
041500     03  INF-UPDATE-DONE         PIC X(3)    VALUE '001'.                 
041600     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '011'.                 
041700     03  INF-LAST-PAGE           PIC X(3)    VALUE '316'.                 
041800     03  INF-ALREADY-LAST        PIC X(3)    VALUE '346'.                 
041900     03  ERR-WRONG-KEY           PIC X(3)    VALUE '022'.                 
042000     03  ERR-WRONG-PRINTER       PIC X(3)    VALUE '347'.                 
042100     03  ERR-KEYS-MISSING        PIC X(3)    VALUE '348'.                 
042200     SKIP2                                                                
042300                                                                          
042400 01  INF-UTSKR-TEXT.                                                      
042500     03  FILLER                  PIC X(12) VALUE 'AR UTSKRIVNA'.          
042600     03  FILLER                  PIC X(12) VALUE 'RR PRINTED  '.          
042700 01  FILLER REDEFINES INF-UTSKR-TEXT.                                     
042800     03  UTSKR-TEXT OCCURS 2     PIC X(12).                               
042900                                                                          
043000     EJECT                                                                
043100*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
043200*                                                                         
043300 01  FILLER                    PIC X(16) VALUE 'SPAR-W6D111-AREA'.        
043400     SKIP3                                                                
043500*01  -COPY W6D111 -PRE SPAR-                                              
043600     EJECT                                                                
043700 01  FILLER                    PIC X(16) VALUE 'SPAR-W6D121-AREA'.        
043800     SKIP3                                                                
043900*01  -COPY W6D121 -PRE SPAR-                                              
044000     EJECT                                                                
044100 01  FILLER                      PIC X(16)   VALUE 'KOM-IO-AREA'.         
044200     SKIP3                                                                
044300 01  KOM-MSG-IO-AREA.                                                     
044400*03  -COPY WMSGKOM                                                        
044500     EJECT                                                                
044600 01      P-TO-P-SW.                                                       
044700                                                                          
044800  02     P-TO-P-KVLL             PIC S9(4)           COMP SYNC.           
044900  02     P-TO-P-KDZ1             PIC X(1)  VALUE LOW-VALUE.               
045000  02     P-TO-P-KDZ2             PIC X(1)  VALUE LOW-VALUE.               
045100  02     P-TO-P-KDTRANS          PIC X(8).                                
045200  02     P-TO-P-IDTRANS          PIC X(4).                                
045300  02     P-TO-P-KDMFSFOR         PIC X(1).                                
045400  02     P-TO-P-DATA             PIC X(1000).                             
045500     EJECT                                                                
045600 01      FILLER                  PIC X(24)   VALUE                        
045700                                 'MOD6191-MID-W6I19101'.                  
045800     SKIP2                                                                
045900     -COPY W6I19101 -PRE MOD6191-                                         
046000     EJECT                                                                
046100 01      FILLER                  PIC X(24)   VALUE                        
046200                                 'MOD6192-MID-W6I19201'.                  
046300     SKIP2                                                                
046400     -COPY W6I19201 -PRE MOD6192-                                         
046500     EJECT                                                                
046600 01      FILLER                  PIC X(24)   VALUE                        
046700                                 'MOD6196-MID-W6I19602'.                  
046800     SKIP2                                                                
046900 01  -COPY W6I19602 -PRE MOD6196-                                         
047000     EJECT                                                                
047100 01      FILLER                  PIC X(24)   VALUE                        
047200                                 'MOD6197-MID-W6I19701'.                  
047300     SKIP2                                                                
047400     -COPY W6I19701 -PRE MOD6197-                                         
047500     EJECT                                                                
047600 01      FILLER                  PIC X(24)   VALUE                        
047700                                 'MOD6199-MID-W6I19901'.                  
047800     SKIP2                                                                
047900     -COPY W6I19901 -PRE MOD6199-                                         
048000     EJECT                                                                
048100******************************************************************        
048200     SKIP3                                                                
048300 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
048400     SKIP3                                                                
048500*01  -COPY WMFSAREA                                                       
048600     EJECT                                                                
048700*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
048800*                                                                         
048900*                                                                         
049000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
049100     SKIP3                                                                
049200 01  NYCKLAR-TILL-DLI.                                                    
049300     03  W-IDDC-B6-X.                                                     
049400         05 W-IDDC-B6            PIC X(2).                                
049500                                                                          
049600     03  W-IDDC-X.                                                        
049700         05 W-IDDC               PIC X(2).                                
049800                                                                          
049900     03  W-W6D101KY-X.                                                    
050000         05  W-D101KY-IDDC       PIC X(2)     VALUE SPACE.                
050100         05  W-D101KY-IDLEVNR    PIC X(5)     VALUE SPACE.                
050200         05  W-D101KY-IDFS       PIC X(8)     VALUE SPACE.                
050300         05  W-D101KY-TIAVIDAT   PIC S9(7)    COMP-3 VALUE ZERO.          
050400                                                                          
050500     03  W-W6D1A1KY-X.                                                    
050600         05  W-D1A1KY-IDDC       PIC X(2)     VALUE SPACE.                
050700         05  W-D1A1KY-IDLEVNR    PIC X(5)     VALUE SPACE.                
050800         05  W-D1A1KY-IDFS       PIC X(8)     VALUE SPACE.                
050900         05  W-D1A1KY-TIAVIDAT   PIC S9(7)    COMP-3 VALUE ZERO.          
051000         05  W-D1A1KY-IDLBBET    PIC X(12)    VALUE SPACE.                
051100                                                                          
051200     03  W-W6D1A1KY-MIN-X.                                                
051300         05  W-D1A1KY-IDDC-MIN      PIC X(2)  VALUE SPACE.                
051400         05  W-D1A1KY-IDLEVNR-MIN    PIC X(5) VALUE SPACE.                
051500         05  W-D1A1KY-IDFS-MIN       PIC X(8)  VALUE SPACE.               
051600         05  W-D1A1KY-TIAVIDAT-MIN   PIC S9(7) COMP-3 VALUE ZERO.         
051700         05  W-D1A1KY-IDLBBET-MIN    PIC X(12) VALUE SPACE.               
051800                                                                          
051900     03  W-W6D1A1KY-MAX-X.                                                
052000         05  W-D1A1KY-IDDC-MAX      PIC X(2)  VALUE SPACE.                
052100         05  W-D1A1KY-IDLEVNR-MAX    PIC X(5) VALUE SPACE.                
052200         05  W-D1A1KY-IDFS-MAX       PIC X(8)  VALUE SPACE.               
052300         05  W-D1A1KY-TIAVIDAT-MAX   PIC S9(7) COMP-3 VALUE ZERO.         
052400         05  W-D1A1KY-IDLBBET-MAX    PIC X(12) VALUE SPACE.               
052500                                                                          
052600     03  W-W6D1ASEQ-X.                                                    
052700         05  W-D1ASEQ-IDDC          PIC X(2)  VALUE SPACE.                
052800         05  W-D1ASEQ-IDLEVNR       PIC X(5)  VALUE SPACE.                
052900         05  W-D1ASEQ-IDFS          PIC X(8)  VALUE SPACE.                
053000         05  W-D1ASEQ-TIAVIDAT      PIC S9(7) COMP-3 VALUE ZERO.          
053100         05  W-D1ASEQ-IDLBBET       PIC X(12) VALUE SPACE.                
053200                                                                          
053300     03  W-W6D1ASEQ-MIN-X.                                                
053400         05  W-D1ASEQ-IDDC-MIN      PIC X(2)  VALUE SPACE.                
053500         05  W-D1ASEQ-IDLEVNR-MIN   PIC X(5)  VALUE SPACE.                
053600         05  W-D1ASEQ-IDFS-MIN      PIC X(8)  VALUE SPACE.                
053700         05  W-D1ASEQ-TIAVIDAT-MIN  PIC S9(7) COMP-3 VALUE ZERO.          
053800         05  W-D1ASEQ-IDLBBET-MIN   PIC X(12) VALUE SPACE.                
053900                                                                          
054000     03  W-W6D1ASEQ-MAX-X.                                                
054100         05  W-D1ASEQ-IDDC-MAX      PIC X(2)  VALUE SPACE.                
054200         05  W-D1ASEQ-IDLEVNR-MAX   PIC X(5)  VALUE SPACE.                
054300         05  W-D1ASEQ-IDFS-MAX      PIC X(8)  VALUE SPACE.                
054400         05  W-D1ASEQ-TIAVIDAT-MAX  PIC S9(7) COMP-3 VALUE ZERO.          
054500         05  W-D1ASEQ-IDLBBET-MAX   PIC X(12) VALUE SPACE.                
054600*                                                                         
054700*--------W6D1D TILL W6INLA11 SEK.INGÅNG                                   
054800     03  W-W6D1D1KY-MIN-X.                                                
054900         05  W-D-MIN-IDILIST  PIC  9(5).                                  
055000         05  W-D-MIN-IDILIRAD PIC S9(5) COMP-3 VALUE ZERO.                
055100         05  W-D-MIN-IDRADNR-INL PIC S9(5) COMP-3 VALUE ZERO.             
055200         05  W-D-MIN-IDDC        PIC X(2)      VALUE SPACE.               
055300         05  W-D-MIN-IDLEVNR  PIC  X(5)        VALUE SPACE.               
055400         05  W-D-MIN-IDFS     PIC  X(8)        VALUE SPACE.               
055500         05  W-D-MIN-TIAVIDAT PIC S9(7) COMP-3 VALUE ZERO.                
055600         05  W-D-MIN-IDRADNR  PIC S9(5) COMP-3 VALUE ZERO.                
055700*                                                                         
055800     03  W-W6D1D1KY-MAX-X.                                                
055900         05  W-D-MAX-IDILIST  PIC  9(5).                                  
056000         05  W-D-MAX-IDILIRAD PIC S9(5) COMP-3 VALUE +999.                
056100         05  W-D-MAX-IDRADNR-INL PIC S9(5) COMP-3 VALUE +99999.           
056200         05  W-D-MAX-IDDC        PIC X(2)      VALUE SPACE.               
056300         05  W-D-MAX-IDLEVNR  PIC  X(5)        VALUE SPACE.               
056400         05  W-D-MAX-IDFS     PIC  X(8)        VALUE SPACE.               
056500         05  W-D-MAX-TIAVIDAT PIC S9(7) COMP-3 VALUE +9999999.            
056600         05  W-D-MAX-IDRADNR  PIC S9(5) COMP-3 VALUE +99999.              
056700*                                                                         
056800*--------SEQ NKL TILL W6D1I                                               
056900     03  W-W6D1I1KY-X.                                                    
057000         05  WI-IDARTNR          PIC S9(9)   COMP-3.                      
057100                                                                          
057200*                                                                         
057300     03  W-IDLBBET-X.                                                     
057400         05  W-IDLBBET           PIC  X(12)   VALUE SPACE.                
057500                                                                          
057600     03  W-IDARTNR-X.                                                     
057700         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
057800*                                                                         
057900     03  W-IDARTNR-K6-X.                                                  
058000         05  W-IDARTNR-K6        PIC S9(9)   VALUE ZERO COMP-3.           
058100*                                                                         
058200     03  W-IDRADNR-INL-X.                                                 
058300         05  W-IDRADNR-INL       PIC S9(5)   VALUE ZERO COMP-3.           
058400*                                                                         
058500     03  W-IDRADNR-INS-X.                                                 
058600         05  W-IDRADNR-INS       PIC S9(5)   VALUE ZERO COMP-3.           
058700*                                                                         
058800     03  W-IDRADNR-X.                                                     
058900         05  W-IDRADNR           PIC S9(5)   VALUE ZERO COMP-3.           
059000*                                                                         
059100     03  W-KDSEGKEY-X.                                                    
059200         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
059300*                                                                         
059400     03  W-W6GXKEY-6005-X.                                                
059500         05  W-6005-PLAA-IDHTYP  PIC X(4)    VALUE '6005'.                
059600         05  W-6005-PLAA-IDDC    PIC X(2)    VALUE SPACE.                 
059700         05  FILLER              PIC X(24)   VALUE LOW-VALUE.             
059800*                                                                         
059900     03  W-W6GXKEY-6006-X.                                                
060000         05  W-6006-ADINLOMR     PIC X(4)    VALUE SPACE.                 
060100         05  FILLER              PIC X(1)    VALUE LOW-VALUE.             
060200*                                                                         
060300     03  W-6006-ADINLOMR-PAR-X.                                           
060400         05  W-6006-ADINLOMR-PAR PIC X(4)    VALUE SPACE.                 
060500*                                                                         
060600     03  W-W6GXKEY-6107-X.                                                
060700         05  W-6107-LASA-IDHTYP  PIC X(4)    VALUE '6107'.                
060800         05  W-6107-LASA-IDDC    PIC X(2)    VALUE SPACE.                 
060900         05  FILLER              PIC X(24)   VALUE LOW-VALUE.             
061000*                                                                         
061100     03  W-W6GXKEY-6108-X.                                                
061200         05  W-6108-IDLBBET      PIC X(12)   VALUE SPACE.                 
061300         05  FILLER              PIC X(3)    VALUE LOW-VALUE.             
061400*                                                                         
061500     03  W-W6GXKEY-6110-X.                                                
061600         05  W-6110-IDLEVNR      PIC X(5)    VALUE SPACE.                 
061700         05  W-6110-IDFS         PIC X(8)    VALUE SPACE.                 
061800         05  W-6110-TIAVIDAT     PIC S9(7)   VALUE +0 COMP-3.             
061900         05  W-6110-IDARTNR      PIC S9(9)   VALUE +0 COMP-3.             
062000         05  W-6110-KDSORT1      PIC S9(1)   VALUE +0 COMP-3.             
062100*                                                                         
062200     03  W-W6GXKEY-6017-X.                                                
062300         05  W-6017-IDHTYP       PIC X(4)    VALUE '6017'.                
062400         05  FILLER              PIC X(26)   VALUE LOW-VALUE.             
062500*                                                                         
062600     03  W-W6GXKEY-6018-X.                                                
062700         05  W-6018-KDSEGKEY     PIC X(1)    VALUE '1'.                   
062800                                                                          
062900     03  W-WDJ1CSEQ-X.                                                    
063000         05  W-IDLEVNR-X.                                                 
063100             07   W-IDLEVNR      PIC X(5)   VALUE SPACE.                  
063200         05  W-BELEVART-X.                                                
063300             07   W-BELEVART     PIC X(30)  VALUE SPACE.                  
063400         05  W-IDARTNR-C-X.                                               
063500             07   W-IDARTNR-C    PIC S9(9)  VALUE ZERO COMP-3.            
063600                                                                          
063700     03  W-WDJ2ESEQ-X.                                                    
063800         05  W-KDCLAGER-ESEQ     PIC S9(1)   VALUE 1    COMP-3.           
063900         05  W-IDARTNR-ESEQ      PIC S9(9)   VALUE ZERO COMP-3.           
064000                                                                          
064100     03  W-WDD901KY-X.                                                    
064200         05  W-IDARTNR-D9        PIC S9(9)   VALUE ZERO COMP-3.           
064300         05  W-IDDC-D9           PIC X(2)    VALUE SPACE.                 
064400                                                                          
064500     03  W-KDAVROP-D9-X.                                                  
064600         05  W-KDAVROP-D9        PIC S9(1)   VALUE ZERO COMP-3.           
064700                                                                          
064800     SKIP2                                                                
064900 01  FILLER                      PIC X(08)   VALUE 'STATUSWS'.            
065000     SKIP3                                                                
065100*    --- STATUS-KOD FRÅN IMS                                              
065200 01  STATUS-WS                   PIC XX.                                  
065300     88  SEGMENT-FINNS                       VALUE '  '.                  
065400     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
065500     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
065600     88  SEGMENT-SLUT                        VALUE 'GB'.                  
065700     SKIP2                                                                
065800 01  GODK-STATUSKODER.                                                    
065900     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
066000     SKIP3                                                                
066100 01  FILLER                      PIC X(08)   VALUE 'SSA-AREA'.            
066200     SKIP3                                                                
066300 01  SSA1                        PIC X(192).                              
066400 01  SSA2                        PIC X(64).                               
066500 01  SSA3                        PIC X(64).                               
066600     EJECT                                                                
066700*    --- IMS FUNKTIONSKODER                                               
066800*01  -COPY W0003                                                          
066900     EJECT                                                                
067000*    ---  DLI INPUT-OUTPUT AREA                                           
067100 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-AREA1'.         
067200     SKIP3                                                                
067300 01  DLI-IO-AREA1.                                                        
067400     03  IO-AREA1                PIC X(150)  VALUE SPACE.                 
067500     SKIP3                                                                
067600     03  W6INLA01 REDEFINES IO-AREA1.                                     
067700*        05  -COPY W6D101                                                 
067800     EJECT                                                                
067900     03  W6INLB01 REDEFINES IO-AREA1.                                     
068000*        05  -COPY W6D1A1                                                 
068100     EJECT                                                                
068200 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-AREA2'.         
068300     SKIP3                                                                
068400 01  DLI-IO-AREA2.                                                        
068500*        05  -COPY W6D111                                                 
068600     EJECT                                                                
068700 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-AREA3'.         
068800     SKIP3                                                                
068900 01  DLI-IO-AREA3.                                                        
069000*        05  -COPY W6D121                                                 
069100     EJECT                                                                
069200 01  FILLER                      PIC X(16)  VALUE 'IO-D121-AREA'.         
069300     SKIP3                                                                
069400 01  D121-AREA.                                                           
069500*        05  -COPY W6D121 -PRE OLD-                                       
069600     EJECT                                                                
069700 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-AREA4'.         
069800 01  DLI-IO-AREA4.                                                        
069900     03  IO-AREA4                PIC X(700)  VALUE SPACE.                 
070000     SKIP3                                                                
070100     03  WLARTM01 REDEFINES IO-AREA4.                                     
070200*        05  -COPY WDK901  -PRE ARTM-                                     
070300     EJECT                                                                
070400     03  W6PLAA11 REDEFINES IO-AREA4.                                     
070500*        05  -COPY W6GX6006 -PRE PLAA-                                    
070600     EJECT                                                                
070700     03  W6LASA11 REDEFINES IO-AREA4.                                     
070800*        05  -COPY W6GX6108 -PRE LASA-                                    
070900     EJECT                                                                
071000     03  W6LASA21 REDEFINES IO-AREA4.                                     
071100*        05  -COPY W6GX6110 -PRE LASA-                                    
071200     EJECT                                                                
071300 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-AREA5'.         
071400 01  DLI-IO-AREA5.                                                        
071500*        05  -COPY W6GX6018 -PRE LOPA-                                    
071600     EJECT                                                                
071700 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA6'.        
071800     SKIP3                                                                
071900 01  DLI-IO-AREA6.                                                        
072000     03  IO-AREA6                PIC X(200)  VALUE SPACE.                 
072100     SKIP3                                                                
072200 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-WDJ1'.          
072300 01  DLI-IO-WDJ1.                                                         
072400     03  IO-WDJ1                 PIC X(400)  VALUE SPACE.                 
072500     03  WDJ101   REDEFINES IO-WDJ1.                                      
072600*        05  -COPY WDJ111   -PRE WDJ1-                                    
072700*        05  -COPY WDJ101   -PRE WDJ1-                                    
072800     EJECT                                                                
072900 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-WDK601'.        
073000     SKIP3                                                                
073100 01  DLI-IO-WDK601.                                                       
073200*        05  -COPY WDK601  -PRE ARTC01-                                   
073300     EJECT                                                                
073400 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-WDK611'.        
073500     SKIP3                                                                
073600 01  DLI-IO-WDK611.                                                       
073700*        05  -COPY WDK611  -PRE ARTC11-                                   
073800     EJECT                                                                
073900 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-WDK624'.        
074000     SKIP3                                                                
074100 01  DLI-IO-WDK624.                                                       
074200*        05  -COPY WDK624  -PRE ARTC24-                                   
074300     EJECT                                                                
074400 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-WDJ211'.        
074500     SKIP3                                                                
074600 01  DLI-IO-WDJ211.                                                       
074700*        05  -COPY WDJ211                                                 
074800     EJECT                                                                
074900 01  FILLER                    PIC X(16)  VALUE 'DLI-IO-W6D1I11'.         
075000     SKIP3                                                                
075100 01  DLI-IO-W6D1I11.                                                      
075200*        05  -COPY W6D111 -PRE I-                                         
075300     EJECT                                                                
075400 01  FILLER                    PIC X(16)  VALUE 'DLI-IO-W6D1I21'.         
075500     SKIP3                                                                
075600 01  DLI-IO-W6D1I21.                                                      
075700*        05  -COPY W6D121 -PRE I-                                         
075800     EJECT                                                                
075900 01  FILLER                    PIC X(16)  VALUE 'DLI-IO-WDB601'.          
076000 01  DLI-IO-WDB601.                                                       
076100*        05  -COPY WDB601                                                 
076200     EJECT                                                                
076300 01  FILLER                    PIC X(16)  VALUE 'DLI-IO-WDK901'.          
076400 01  DLI-IO-WDK901.                                                       
076500*        05  -COPY WDK901  -PRE K9-                                       
076600     EJECT                                                                
076700 01  FILLER                    PIC X(16)  VALUE 'DLI-IO-WDL201'.          
076800 01  DLI-IO-WDL201.                                                       
076900*        05  -COPY WDL201  -PRE L2-                                       
077000     EJECT                                                                
077100 01  FILLER                    PIC X(16)  VALUE 'DLI-IO-WDL221'.          
077200 01  DLI-IO-WDL221.                                                       
077300*        05  -COPY WDL221                                                 
077400     EJECT                                                                
077500 01  FILLER                    PIC X(16)  VALUE 'DLI-IO-WDD901'.          
077600 01  DLI-IO-WDD901.                                                       
077700*        05  -COPY WDD901                                                 
077800     EJECT                                                                
077900 01  FILLER                    PIC X(16)  VALUE 'DLI-IO-WDD905'.          
078000 01  DLI-IO-WDD905.                                                       
078100*        05  -COPY WDD905                                                 
078200     EJECT                                                                
078300 01  FILLER                    PIC X(16)  VALUE 'DLI-IO-W6D211'.          
078400 01  DLI-IO-W6D211.                                                       
078500*        05  -COPY W6D211                                                 
078600 01  FILLER                    PIC X(16)  VALUE 'DLI-IO-WDD801'.          
078700 01  DLI-IO-WDD801.                                                       
078800*        05  -COPY WDD801 -PRE WDD8-                                      
078900 01  FILLER                    PIC X(16)  VALUE 'DLI-IO-WDD811'.          
079000 01  DLI-IO-WDD811.                                                       
079100*        05  -COPY WDD811                                                 
079200                                                                          
079300 LINKAGE SECTION.                                                         
079400                                                                          
079500 01  REQU-AREA.                                                           
079600*    03 -COPY WZ01REQU                                                    
079700*    03 -COPY W60115I1                                                    
079800*                                                                         
079900 01  RESP-AREA.                                                           
080000*    03 -COPY WZ01RESP                                                    
080100*    03 -COPY W60115O1                                                    
080200*                                                                         
080300 01  MAX-KVRADER                 PIC S9(4) COMP.                          
080400                                                                          
080500*01  -COPY W0009   -PRE MSG-                                              
080600     EJECT                                                                
080700*01  -COPY W0009   -PRE DISP-                                             
080800     EJECT                                                                
080900*01  -COPY W0009   -PRE STAT-                                             
081000     EJECT                                                                
081100*01  -COPY W0009   -PRE AR-                                               
081200     EJECT                                                                
081300*01  -COPY W0009   -PRE FR-                                               
081400     EJECT                                                                
081500*01  -COPY W0009   -PRE ILIST-                                            
081600     EJECT                                                                
081700*01  -COPY W0009  -PRE SYNQ-                                              
081800     EJECT                                                                
081900*01  -COPY W0008  -PRE ARTC-                                              
082000     05  FILLER                  PIC X.                                   
082100     EJECT                                                                
082200*01  -COPY W0009   -PRE WDB6-                                             
082300     05  FILLER                  PIC X.                                   
082400     EJECT                                                                
082500*01  -COPY W0008  -PRE INLA1-                                             
082600     05  FILLER                  PIC X.                                   
082700     EJECT                                                                
082800*01  -COPY W0008  -PRE INLA2-                                             
082900     05  FILLER                  PIC X.                                   
083000     EJECT                                                                
083100*01  -COPY W0008  -PRE INLB-                                              
083200     05  FILLER                  PIC X.                                   
083300     EJECT                                                                
083400*01  -COPY W0008  -PRE INLA3-                                             
083500     05  FILLER                  PIC X.                                   
083600     EJECT                                                                
083700*01  -COPY W0008  -PRE INLA-D-                                            
083800     05  FILLER                  PIC X.                                   
083900     EJECT                                                                
084000*01  -COPY W0008  -PRE W6D1-I-                                            
084100     05  FILLER                  PIC X.                                   
084200     EJECT                                                                
084300*01  -COPY W0008  -PRE PLAA-                                              
084400     05  FILLER                  PIC X.                                   
084500     EJECT                                                                
084600*01  -COPY W0008  -PRE LASA-                                              
084700     05  FILLER                  PIC X.                                   
084800     EJECT                                                                
084900*01  -COPY W0008  -PRE LOPA-                                              
085000     05  FILLER                  PIC X.                                   
085100     EJECT                                                                
085200*01  -COPY W0008  -PRE ARTS-                                              
085300     05  FILLER                  PIC X.                                   
085400     EJECT                                                                
085500*01  -COPY W0008  -PRE WDJ1-                                              
085600     05  FILLER                  PIC X.                                   
085700     EJECT                                                                
085800*01  -COPY W0008  -PRE WDJ2-                                              
085900     05  FILLER                  PIC X.                                   
086000     EJECT                                                                
086100*01  -COPY W0008  -PRE INLA4-                                             
086200     05  FILLER                  PIC X.                                   
086300     EJECT                                                                
086400*01  -COPY W0008  -PRE WDK9-                                              
086500     05  FILLER                  PIC X.                                   
086600     EJECT                                                                
086700*01  -COPY W0008  -PRE WDL2-                                              
086800     05  FILLER                  PIC X.                                   
086900     EJECT                                                                
087000*01  -COPY W0008  -PRE WDD9-                                              
087100     05  FILLER                  PIC X.                                   
087200     EJECT                                                                
087300*01  -COPY W0008  -PRE WDD8-                                              
087400     05  FILLER                  PIC X.                                   
087500     EJECT                                                                
087600*    PCB'ER FÖR SUBPGM                                                    
087700                                                                          
087800 01 STYR-HANB-PCB                PIC X.                                   
087900                                                                          
088000 01 STYR-PLAA-PCB                PIC X.                                   
088100                                                                          
088200 01 PRIO-INLA-PCB                PIC X.                                   
088300                                                                          
088400 01 PRIO-ARTC-PCB                PIC X.                                   
088500                                                                          
088600 01 PRIO-ARTM-PCB                PIC X.                                   
088700                                                                          
088800 01 PRIO-ORDQ-PCB                PIC X.                                   
088900                                                                          
089000 01 PRIO-ARTS-PCB                PIC X.                                   
089100                                                                          
089200 01 PRIO-KVAI-PCB                PIC X.                                   
089300                                                                          
089400 01 PRIO-INLI1-PCB               PIC X.                                   
089500                                                                          
089600 01 PRIO-KVAE-PCB                PIC X.                                   
089700                                                                          
089800 01 KOM-KOMA-PCB                 PIC X.                                   
089900                                                                          
090000 01 KVAL-ARTC-PCB                PIC X.                                   
090100                                                                          
090200 01 KVAL-KVAH1-PCB               PIC X.                                   
090300                                                                          
090400 01 KVAL-KVAH2-PCB               PIC X.                                   
090500                                                                          
090600 01 KVAL-KVAG-PCB                PIC X.                                   
090700                                                                          
090800 01 KVAL-LEVA-PCB                PIC X.                                   
090900                                                                          
091000 01 KVAL-UPFA-PCB                PIC X.                                   
091100                                                                          
091200 01 KVAL-PROA-PCB                PIC X.                                   
091300                                                                          
091400 01 KVAL-XXLA-PCB                PIC X.                                   
091500                                                                          
091600 01 KVAL-KODA-PCB                PIC X.                                   
091700                                                                          
091800 01 BEHOV-WDK6-PCB               PIC X.                                   
091900                                                                          
092000 01 BEHOV-WDK7-PCB               PIC X.                                   
092100                                                                          
092200 01 BEHOV-WDR2-PCB               PIC X.                                   
092300                                                                          
092400 01 BEHOV-WDE3-PCB               PIC X.                                   
092500                                                                          
092600 01 BEHOV-WDK6-2-PCB             PIC X.                                   
092700                                                                          
092800 01 BEHOV-WDK7-2-PCB             PIC X.                                   
092900                                                                          
093000 01 BEHOV-WDL6-PCB               PIC X.                                   
093100                                                                          
093200 01 BEHOV-WDD7A-PCB              PIC X.                                   
093300                                                                          
093400 01 BEHOV-WDB6-PCB               PIC X.                                   
093500                                                                          
093600 01 W22222-WDK6-PCB              PIC X.                                   
093700                                                                          
093800 01 W22222-WDK7-PCB              PIC X.                                   
093900                                                                          
094000 01 W22222-ARTM-PCB              PIC X.                                   
094100                                                                          
094200 01 W22222-2501-PCB              PIC X.                                   
094300                                                                          
094400 01 W22222-WDB6-PCB              PIC X.                                   
094500                                                                          
094600 01 W22222-WDD7-PCB              PIC X.                                   
094700                                                                          
094800 01 W22222-WDK7E-PCB              PIC X.                                  
094900*01  -COPY W0008  -PRE W6D2-                                              
095000     05  FILLER                  PIC X.                                   
095100 01 SYNQ-ATAB-PCB             PIC X.                                      
095200 01 WDQ3-PCB                  PIC X.                                      
095300     EJECT                                                                
095400                                                                          
095500 PROCEDURE DIVISION  USING REQU-AREA     RESP-AREA    MAX-KVRADER         
095600                           MSG-PCB  DISP-PCB                              
095700                           STAT-PCB AR-PCB FR-PCB ILIST-PCB               
095800                           SYNQ-PCB                                       
095900                           ARTC-PCB WDB6-PCB                              
096000                           INLA1-PCB INLA2-PCB INLB-PCB INLA3-PCB         
096100                           INLA-D-PCB W6D1-I-PCB                          
096200                           PLAA-PCB LASA-PCB LOPA-PCB ARTS-PCB            
096300                           WDJ1-PCB WDJ2-PCB INLA4-PCB                    
096400                           WDK9-PCB WDL2-PCB WDD9-PCB WDD8-PCB            
096500                           STYR-HANB-PCB STYR-PLAA-PCB                    
096600                           PRIO-INLA-PCB PRIO-ARTC-PCB                    
096700                           PRIO-ARTM-PCB PRIO-ORDQ-PCB                    
096800                           PRIO-ARTS-PCB PRIO-KVAI-PCB                    
096900                           PRIO-INLI1-PCB                                 
097000                           PRIO-KVAE-PCB                                  
097100                           KOM-KOMA-PCB                                   
097200                           KVAL-ARTC-PCB  KVAL-KVAH1-PCB                  
097300                           KVAL-KVAH2-PCB KVAL-KVAG-PCB                   
097400                           KVAL-LEVA-PCB  KVAL-UPFA-PCB                   
097500                           KVAL-PROA-PCB  KVAL-XXLA-PCB                   
097600                           KVAL-KODA-PCB                                  
097700                           BEHOV-WDK6-PCB BEHOV-WDK7-PCB                  
097800                           BEHOV-WDR2-PCB BEHOV-WDE3-PCB                  
097900                           BEHOV-WDK6-2-PCB BEHOV-WDK7-2-PCB              
098000                           BEHOV-WDL6-PCB BEHOV-WDD7A-PCB                 
098100                           BEHOV-WDB6-PCB                                 
098200                           W22222-WDK6-PCB W22222-WDK7-PCB                
098300                           W22222-ARTM-PCB W22222-2501-PCB                
098400                           W22222-WDB6-PCB W22222-WDD7-PCB                
098500                           W22222-WDK7E-PCB W6D2-PCB                      
098600                           SYNQ-ATAB-PCB WDQ3-PCB.                        
098700                                                                          
098800     PERFORM A-INIT                                                       
098900                                                                          
099000     IF REQU-UPD-X                                                        
              PERFORM BE-KOLLA-ADINLOMR-PRT                                     
099100        PERFORM BF-KOLLA-FLYTTA-IDDC                                      
099200        PERFORM HA-UPPDATERA-AKTIVERING-AVI                               
099300     ELSE                                                                 
099400        PERFORM B-CONTROL-KEYS                                            
099500        IF NYCKLAR-OK                                                     
099600           IF REQU-UPDATE                                                 
099700              PERFORM G-CONTROL-INPUT                                     
099800              IF INDATA-OK                                                
099900                 PERFORM H-UPDATE                                         
100000              END-IF                                                      
100100           ELSE                                                           
100200              IF REQU-FIRST                                               
100300                 PERFORM C-FIRST-PAGE                                     
100400              ELSE                                                        
100500                IF REQU-NEXT                                              
100600                   PERFORM D-NEXT-PAGE                                    
100700                ELSE                                                      
100800                   PERFORM E-SAME-PAGE                                    
100900                END-IF                                                    
101000              END-IF                                                      
101100           END-IF                                                         
101200                                                                          
101300           IF INDATA-OK                                                   
101400              PERFORM F-READ-SHOW-INFO                                    
101500           END-IF                                                         
101600        END-IF                                                            
101700     END-IF                                                               
101800                                                                          
101900     MOVE ZERO TO RETURN-CODE                                             
102000     GOBACK                                                               
102100     .                                                                    
102200     EJECT                                                                
102300 A-INIT SECTION.                                                          
102400     MOVE ALL '+'     TO RESP-W60115O1                                    
102500     MOVE 001         TO RESP-IDMSGVER                                    
102600*MAX-KVRADER                                                              
102700     MOVE REQU-KVRADER TO RESP-KVRADER                                    
102800     MOVE SPACE       TO RESP-IDMSG-ERROR                                 
102900                         RESP-IDMSG-INFO                                  
103000                         RESP-IDELMT-ERROR                                
103100     MOVE SPACE       TO RESP-TEMFSFEL                                    
103200     MOVE SPACE       TO RESP-TEMFSINF                                    
103300                                                                          
103400     ACCEPT DAGENS-DATUM       FROM DATE                                  
103500     ACCEPT DAGENS-TID         FROM TIME                                  
103600                                                                          
103700     MOVE 'IDAG'               TO DAT-KDDATFORM                           
103800     CALL WDATKONV USING          DAT-KDDATFORM                           
103900                                  DAT-I-TIDATUM                           
104000                                  DAT-O-TIDATUM                           
104100                                  DAT-KDSVAR                              
104200                                                                          
104300     MOVE DAT-TID              TO WS-DAT-TID                              
104400     MOVE DAT-TIAAVV-GRP       TO WS-DAT-TIAAVV-GRP                       
104500                                                                          
104600     SET ADINLOMR-IX           TO 1                                       
104700     PERFORM UNTIL ADINLOMR-IX >  MAX-ADINLOMR-IX                         
104800       MOVE SPACE              TO W-ADINLOMR        (ADINLOMR-IX)         
104900       MOVE ZERO               TO W-VLARTNTO-ADINLOMR(ADINLOMR-IX)        
105000       SET ADINLOMR-IX UP BY 1                                            
105100     END-PERFORM                                                          
105200                                                                          
105300     SET LPL-IX                TO 1                                       
105400     PERFORM UNTIL LPL-IX      >  MAX-LPL-IX                              
105500       MOVE SPACE              TO W-LPL        (LPL-IX)                   
105600       MOVE ZERO               TO W-SUPROC-LPL (LPL-IX)                   
105700                                W-VLARTNTO-LPL(LPL-IX)                    
105800       SET LPL-IX UP BY 1                                                 
105900     END-PERFORM                                                          
106000                                                                          
106100     MOVE JA                   TO INDATA-SW                               
106200                                  FOERSTA-6191-SW                         
106300                                  FOERSTA-6196-SW                         
106400                                  FOERSTA-6197-SW                         
106500                                                                          
106600     MOVE 'A'                  TO AKTIVERING-SW                           
106700                                                                          
106800     IF REQU-IDLAND-SPR = 'GB'                                            
106900       MOVE +2 TO SPRAK-IX                                                
107000       MOVE  2 TO WS-KDMFSFOR                                             
107100     ELSE                                                                 
107200       MOVE +1 TO SPRAK-IX                                                
107300       MOVE  1 TO WS-KDMFSFOR                                             
107400     END-IF                                                               
107500*FÖR NDC:ERNAS SKULL HÄMTAR MAN LOKAL TID MHA ETT ANROP TILL              
107600*W005INIT. DETTA SKA KUNNA GÄLLA FÖR SAMTLIGA DC:N ÄVEN CDC               
107700*                                                                         
107800*WL01TIDZ                                                                 
107900     PERFORM I-CALL-WL01TIDZ                                              
108000                                                                          
108100     MOVE 'AAMMDD'             TO DAT-KDDATFORM                           
108200     MOVE TIDZ-MSGI-TILOKDAT   TO DAT-I-TIDATUM                           
108300     CALL WDATKONV USING          DAT-KDDATFORM                           
108400                                  DAT-I-TIDATUM                           
108500                                  DAT-O-TIDATUM                           
108600                                  DAT-KDSVAR                              
108700     MOVE DAT-TIAAVVD-GRP(3:3) TO WS-DAT-TIVVD                            
108800                                                                          
108900                                                                          
109000     .                                                                    
109100     EJECT                                                                
109200 B-CONTROL-KEYS      SECTION.                                             
109300     MOVE JA                   TO NYCKLAR-SW                              
109400                                                                          
109500     PERFORM BA-KOLLA-IDLEVNR                                             
109600                                                                          
109700     PERFORM BB-KOLLA-IDFS                                                
109800     PERFORM BC-KOLLA-TIAVIDAT                                            
109900     PERFORM BD-KOLLA-IDLBBET                                             
110000                                                                          
110100     IF    WS-IDLEVNR = SPACE                                             
110200     AND ( WS-IDLBBET   =   SPACE OR ALL '+' )                            
110300       MOVE NEJ    TO NYCKLAR-SW                                          
110400       MOVE 'IDLEVNR+IDLBBET'    TO RESP-IDELMT-ERROR                     
110500     END-IF                                                               
110600                                                                          
110700     PERFORM BE-KOLLA-ADINLOMR-PRT                                        
110800     PERFORM BF-KOLLA-FLYTTA-IDDC                                         
110900                                                                          
111000     IF REQU-KVUTSKR-AR = ALL '+'                                         
111100       MOVE ZERO           TO W-KVUTSKR-AR                                
111200     ELSE                                                                 
111300       MOVE REQU-KVUTSKR-AR TO W-KVUTSKR-AR                               
111400     END-IF                                                               
111500                                                                          
111600     IF NYCKLAR-FEL                                                       
111700       MOVE ERR-WRONG-KEY      TO RESP-IDMSG-ERROR                        
111800       PERFORM MFS-ERASE-FIELD-IN                                         
111900       PERFORM MFS-ERASE-FIELD-OUT                                        
112000     ELSE                                                                 
112100       IF WS-IDLBBET > SPACE                                              
112200         SET     INLB-LAES         TO TRUE                                
112300         MOVE ZERO                 TO WS-IDLEVNR                          
112400         MOVE ZERO                 TO WS-IDFS                             
112500         MOVE SPACE                TO WS-TIAVIDAT                         
112600         MOVE ZERO                 TO W-D101KY-TIAVIDAT                   
112700       ELSE                                                               
112800*        IF REQU-TIAVIDAT-KEY NOT = ALL '+'                               
112900         IF WS-TIAVIDAT > ZERO                                            
113000           IF WS-IDLEVNR NOT = SPACE AND WS-IDFS NOT = SPACE              
113100             SET INLA-LAES        TO TRUE                                 
113200           ELSE                                                           
113300             MOVE NEJ TO NYCKLAR-SW                                       
113400             MOVE 'IDLEVNR+IDFS'  TO RESP-IDELMT-ERROR                    
113500           END-IF                                                         
113600         ELSE                                                             
113700*          IF REQU-IDFS-KEY NOT = ALL '+'                                 
113800           IF WS-IDFS > ZERO                                              
113900             IF WS-IDLEVNR NOT = SPACE                                    
114000             SET INLA-SEQ-LAES    TO TRUE                                 
114100             ELSE                                                         
114200               MOVE NEJ TO NYCKLAR-SW                                     
114300               MOVE 'IDLEVNR'       TO RESP-IDELMT-ERROR                  
114400             END-IF                                                       
114500           ELSE                                                           
114600*            IF REQU-IDLEVNR-KEY NOT = ALL '+'                            
114700             IF WS-IDLEVNR > SPACE AND NOT = ALL '+'                      
114800               SET INLA-SEQ-LAES     TO TRUE                              
114900               MOVE SPACE   TO WS-IDFS                                    
115000               MOVE ZERO    TO WS-TIAVIDAT                                
115100             ELSE                                                         
115200               IF WS-IDLBBET  > SPACE                                     
115300                 SET INLB-LAES TO TRUE                                    
115400               ELSE                                                       
115500                 IF WS-TIAVIDAT >  ZERO                                   
115600                    SET INLA-LAES      TO TRUE                            
115700                  ELSE                                                    
115800                    SET INLA-SEQ-LAES  TO TRUE                            
115900                 END-IF                                                   
116000               END-IF                                                     
116100             END-IF                                                       
116200           END-IF                                                         
116300         END-IF                                                           
116400       END-IF                                                             
116500       IF NYCKLAR-FEL                                                     
116600         PERFORM MFS-ERASE-FIELD-IN                                       
116700         PERFORM MFS-ERASE-FIELD-OUT                                      
116800         MOVE ERR-WRONG-KEY      TO RESP-IDMSG-ERROR                      
116900       END-IF                                                             
117000     END-IF                                                               
117100     .                                                                    
117200     EJECT                                                                
117300 BA-KOLLA-IDLEVNR   SECTION.                                              
117400                                                                          
117500     IF REQU-IDLEVNR-KEY       = ALL '+'                                  
117600     OR REQU-IDLEVNR-KEY       = SPACE                                    
117700     OR REQU-IDLEVNR-KEY       = ZERO                                     
117800       MOVE ZERO               TO WS-IDLEVNR                              
117900     ELSE                                                                 
118000       MOVE REQU-IDLEVNR-KEY   TO WS-IDLEVNR                              
118100     END-IF                                                               
118200     .                                                                    
118300     EJECT                                                                
118400 BB-KOLLA-IDFS      SECTION.                                              
118500                                                                          
118600     IF REQU-IDFS-KEY          =  ALL '+'                                 
118700     OR REQU-IDFS-KEY          =  SPACE                                   
118800     OR REQU-IDFS-KEY          =  ZERO                                    
118900       MOVE ZERO               TO WS-IDFS                                 
119000     ELSE                                                                 
119100       MOVE REQU-IDFS-KEY      TO WS-IDFS                                 
119200     END-IF                                                               
119300     .                                                                    
119400     EJECT                                                                
119500 BC-KOLLA-TIAVIDAT  SECTION.                                              
119600                                                                          
119700     IF REQU-TIAVIDAT-KEY      = ALL '+'                                  
119800     OR REQU-TIAVIDAT-KEY      = SPACE                                    
119900     OR REQU-TIAVIDAT-KEY      = ZERO                                     
120000       MOVE ZERO               TO WS-TIAVIDAT                             
120100     ELSE                                                                 
120200       MOVE REQU-TIAVIDAT-KEY  TO WS-TIAVIDAT                             
120300       INSPECT WS-TIAVIDAT REPLACING LEADING SPACE BY ZERO                
120400     END-IF                                                               
120500                                                                          
120600     IF WS-TIAVIDAT NUMERIC                                               
120700         MOVE WS-TIAVIDAT      TO W-D101KY-TIAVIDAT                       
120800     END-IF                                                               
120900     .                                                                    
121000     EJECT                                                                
121100 BD-KOLLA-IDLBBET   SECTION.                                              
121200                                                                          
121300     IF REQU-IDLBBET-KEY       = ALL '+'                                  
121400     OR REQU-IDLBBET-KEY       <= SPACE                                   
121500       MOVE SPACE               TO WS-IDLBBET                             
121600     ELSE                                                                 
121700       MOVE REQU-IDLBBET-KEY TO WS-IDLBBET                                
121800     END-IF                                                               
121900                                                                          
122000     MOVE WS-IDLBBET           TO W-IDLBBET                               
122100     .                                                                    
122200     EJECT                                                                
122300 BE-KOLLA-ADINLOMR-PRT  SECTION.                                          
122400                                                                          
122500     IF REQU-ADINLOMR-PRT-KEY       =  ALL '+'                            
122600     OR REQU-ADINLOMR-PRT-KEY      <=  SPACE                              
122700       MOVE SPACE                   TO WS-ADINLOMR-PRT                    
122800     ELSE                                                                 
122900       MOVE REQU-ADINLOMR-PRT-KEY   TO WS-ADINLOMR-PRT                    
123000     END-IF                                                               
123100     .                                                                    
123200     EJECT                                                                
123300 BF-KOLLA-FLYTTA-IDDC  SECTION.                                           
123400     MOVE     SPACE       TO WS-IDDC                                      
123500                                                                          
123600     MOVE REQU-IDDC-KEY        TO WS-IDDC                                 
123700                                                                          
123800     IF CDC OR NDC                                                        
123900       MOVE  WS-IDDC   TO PRIO-IDDC                                       
124000                          STYR-IDDC                                       
124100                          W-D1A1KY-IDDC                                   
124200                          W-D1A1KY-IDDC-MIN                               
124300                          W-D1A1KY-IDDC-MAX                               
124400                          W-D1ASEQ-IDDC                                   
124500                          W-D1ASEQ-IDDC-MIN                               
124600                          W-D1ASEQ-IDDC-MAX                               
124700                          W-6005-PLAA-IDDC                                
124800                          W-6107-LASA-IDDC                                
124900                          W-IDDC-B6                                       
125000                          W-IDDC                                          
125100       PERFORM IMS-GU-WDB601                                              
125200       IF SEGMENT-FINNS                                                   
125300         MOVE DCS-FLWEBDC TO WS-FLWEBDC                                   
125400       END-IF                                                             
125500     ELSE                                                                 
125600       IF NOT REQU-UPD-X                                                  
125700          MOVE    NEJ     TO NYCKLAR-SW                                   
125800          MOVE   SPACE    TO WS-IDDC                                      
125900          MOVE 'IDDC'     TO RESP-IDELMT-ERROR                            
126000       END-IF                                                             
126100     END-IF                                                               
126200     .                                                                    
126300     EJECT                                                                
126400                                                                          
126500 C-FIRST-PAGE       SECTION.                                              
126600                                                                          
126700     MOVE INF-FIRST-PAGE TO RESP-IDMSG-INFO                               
126800     .                                                                    
126900     EJECT                                                                
127000                                                                          
127100 D-NEXT-PAGE SECTION.                                                     
127200     MOVE WS-IDDC              TO W-D1ASEQ-IDDC                           
127300                                  W-D1A1KY-IDDC                           
127400     MOVE REQU-IDLEVNR-START   TO W-D1ASEQ-IDLEVNR                        
127500                                  W-D1A1KY-IDLEVNR                        
127600     MOVE REQU-IDFS-START      TO W-D1ASEQ-IDFS                           
127700                                  W-D1A1KY-IDFS                           
127800     IF REQU-TIAVIDAT-START NUMERIC                                       
127900       MOVE REQU-TIAVIDAT-START  TO W-D1ASEQ-TIAVIDAT                     
128000                                    W-D1A1KY-TIAVIDAT                     
128100     ELSE                                                                 
128200       MOVE ZERO                 TO W-D1ASEQ-TIAVIDAT                     
128300                                    W-D1A1KY-TIAVIDAT                     
128400     END-IF                                                               
128500     MOVE REQU-IDLBBET-START   TO W-D1ASEQ-IDLBBET                        
128600     .                                                                    
128700     EJECT                                                                
128800                                                                          
128900 E-SAME-PAGE SECTION.                                                     
129000     MOVE WS-IDDC                TO W-D1ASEQ-IDDC                         
129100                                W-D1A1KY-IDDC                             
129200     MOVE REQU-IDLEVNR-START TO W-D1ASEQ-IDLEVNR                          
129300                                W-D1A1KY-IDLEVNR                          
129400     MOVE REQU-IDFS-START        TO W-D1ASEQ-IDFS                         
129500                                W-D1A1KY-IDFS                             
129600     IF REQU-TIAVIDAT-START NUMERIC                                       
129700       MOVE REQU-TIAVIDAT-START TO W-D1ASEQ-TIAVIDAT                      
129800                                   W-D1A1KY-TIAVIDAT                      
129900     ELSE                                                                 
130000       MOVE ZERO                TO W-D1ASEQ-TIAVIDAT                      
130100                                   W-D1A1KY-TIAVIDAT                      
130200     END-IF                                                               
130300     MOVE REQU-IDLBBET-START TO W-D1ASEQ-IDLBBET                          
130400                                                                          
130500     MOVE JA                     TO REQU-KDCMDVAL-SW                      
130600     MOVE +1                     TO INDX                                  
130700     PERFORM UNTIL INDX        >  MAX-KVRADER                             
130800       IF REQU-KDCMDVAL-LINE(INDX) NOT =      ALL '+'                     
130900         MOVE NEJ                TO REQU-KDCMDVAL-SW                      
131000       END-IF                                                             
131100       ADD +1                    TO INDX                                  
131200     END-PERFORM                                                          
131300                                                                          
131400     IF  REQU-INPUT  =  ALL '+'                                           
131500     AND REQU-KDCMDVAL-ALL-PLUS                                           
131600       CONTINUE                                                           
131700     ELSE                                                                 
131800       MOVE NEJ                  TO INDATA-SW                             
131900       MOVE INF-PRESS-PF11       TO RESP-IDMSG-INFO                       
132000       PERFORM MFS-LAES-IN-IGEN                                           
132100       PERFORM EA-FLYTTA-REQU-TILL-RESP                                   
132200     END-IF                                                               
132300     .                                                                    
132400     EJECT                                                                
132500 EA-FLYTTA-REQU-TILL-RESP   SECTION.                                      
132600                                                                          
132700     IF REQU-IDLBBET-UPD   =  ALL '+'                                     
132800         MOVE SPACE            TO RESP-IDLBBET-UPD                        
132900     ELSE                                                                 
133000         MOVE REQU-IDLBBET-UPD TO RESP-IDLBBET-UPD                        
133100     END-IF                                                               
133200                                                                          
133300     IF REQU-FLKLAR-UPD    =  ALL '+'                                     
133400         MOVE SPACE            TO RESP-FLKLAR-UPD                         
133500     ELSE                                                                 
133600         MOVE REQU-FLKLAR-UPD  TO RESP-FLKLAR-UPD                         
133700     END-IF                                                               
133800                                                                          
133900     IF REQU-ADINLOMR-LPL-UPD  = ALL '+'                                  
134000         MOVE SPACE                TO RESP-ADINLOMR-LPL-UPD               
134100     ELSE                                                                 
134200         MOVE REQU-ADINLOMR-LPL-UPD TO RESP-ADINLOMR-LPL-UPD              
134300     END-IF                                                               
134400     .                                                                    
134500     EJECT                                                                
134600 F-READ-SHOW-INFO SECTION.                                                
134700     IF REQU-UPDATE AND AKTIVERING-BIL                                    
134800       IF REQU-ADINLOMR-LPL-UPD = ALL '+' AND LOSSLISTA                   
134900         PERFORM FB-VISA-LPL                                              
135000       END-IF                                                             
135100     ELSE                                                                 
135200       PERFORM FC-VISA-BILD                                               
135300     END-IF                                                               
135400                                                                          
135500     IF RESP-IDLEVNR-START = ALL '+'                                      
135600       MOVE SPACE            TO RESP-IDLEVNR-START                        
135700     END-IF                                                               
135800     IF RESP-IDLEVNR-NEXT  = ALL '+'                                      
135900       MOVE SPACE            TO RESP-IDLEVNR-NEXT                         
136000     END-IF                                                               
136100     IF RESP-IDFS-START = ALL '+'                                         
136200       MOVE SPACE            TO RESP-IDFS-START                           
136300     END-IF                                                               
136400     IF RESP-IDFS-NEXT  = ALL '+'                                         
136500       MOVE SPACE            TO RESP-IDFS-NEXT                            
136600     END-IF                                                               
136700     IF RESP-IDLBBET-START = ALL '+'                                      
136800       MOVE SPACE            TO RESP-IDLBBET-START                        
136900     END-IF                                                               
137000     IF RESP-IDLBBET-NEXT  = ALL '+'                                      
137100       MOVE SPACE            TO RESP-IDLBBET-NEXT                         
137200     END-IF                                                               
137300     IF RESP-TIAVIDAT-START = ALL '+'                                     
137400       MOVE ZERO             TO RESP-TIAVIDAT-START                       
137500     END-IF                                                               
137600     IF RESP-TIAVIDAT-NEXT  = ALL '+'                                     
137700       MOVE ZERO             TO RESP-TIAVIDAT-NEXT                        
137800     END-IF                                                               
137900                                                                          
138000*HANTERING AV TEXT I RESP-TEMFSINF FINNS I W6011500                       
138100     IF W-KVUTSKR-AR > +0                                                 
138200       MOVE SPACE        TO RESP-IDMSG-ERROR                              
138300       MOVE W-KVUTSKR-AR TO RESP-TEMFSINF(1:3)                            
138400       MOVE UTSKR-TEXT (SPRAK-IX)    TO RESP-TEMFSINF(5:12)               
138500     END-IF                                                               
138600     .                                                                    
138700     EJECT                                                                
138800 FB-VISA-LPL  SECTION.                                                    
138900     MOVE +1                   TO RESP-LPL-IX                             
139000     PERFORM UNTIL RESP-LPL-IX > MAX-RESP-LPL-IX OR                       
139100                   LPL-IX         < 1                                     
139200       MOVE W-LPL (LPL-IX)     TO RESP-ADINLOMR-LPL (RESP-LPL-IX)         
139300       COMPUTE    RESP-SUPROC-LPL (RESP-LPL-IX) =                         
139400                W-SUPROC-LPL (LPL-IX) * 100                               
139500       END-COMPUTE                                                        
139600       ADD +1                  TO RESP-LPL-IX                             
139700       SET LPL-IX DOWN BY 1                                               
139800     END-PERFORM                                                          
139900     .                                                                    
140000     EJECT                                                                
140100 FC-VISA-BILD SECTION.                                                    
140200     IF REQU-IDLBBET-UPD NOT = ALL '+'                                    
140300       MOVE REQU-IDLBBET-UPD TO RESP-IDLBBET-UPD                          
140400     ELSE                                                                 
140500       MOVE SPACE            TO RESP-IDLBBET-UPD                          
140600     END-IF                                                               
140700                                                                          
140800     MOVE +1                   TO INDX                                    
140900     MOVE  0                   TO RESP-KVRADER                            
141000                                                                          
141100     PERFORM FCA-LAES-RADDATA                                             
141200     IF SEGMENT-FINNS                                                     
141300       MOVE INL-IDLEVNR        TO RESP-IDLEVNR-START                      
141400       MOVE INL-IDFS           TO RESP-IDFS-START                         
141500       MOVE INL-TIAVIDAT       TO RESP-TIAVIDAT-START                     
141600       MOVE INL-IDLBBET        TO RESP-IDLBBET-START                      
141700     ELSE                                                                 
141800       IF REQU-UPDATE                                                     
141900         MOVE SPACE              TO RESP-IDMSG-INFO                       
142000       ELSE                                                               
142100         IF REQU-NEXT                                                     
142200           MOVE INF-ALREADY-LAST TO RESP-IDMSG-INFO                       
142300         ELSE                                                             
142400           MOVE ERR-KEYS-MISSING TO RESP-IDMSG-ERROR                      
142500         END-IF                                                           
142600       END-IF                                                             
142700                                                                          
142800       PERFORM MFS-ERASE-FIELD-OUT                                        
142900       MOVE SPACE              TO RESP-IDLEVNR-START                      
143000                                  RESP-IDFS-START                         
143100                                  RESP-IDLBBET-START                      
143200       MOVE ZERO               TO RESP-TIAVIDAT-START                     
143300     END-IF                                                               
143400                                                                          
143500     PERFORM UNTIL INDX > MAX-KVRADER OR SEGMENT-SAKNAS OR                
143600         (INLA-LAES AND INL-TIINLMOT > ZERO)                              
143700         MOVE INL-IDLEVNR          TO RESP-IDLEVNR-LINE(INDX)             
143800         MOVE INL-IDFS             TO RESP-IDFS-LINE(INDX)                
143900         MOVE INL-TIAVIDAT         TO W-TIAVIDAT-RAD                      
144000         MOVE INL-IDSHIPM          TO RESP-IDSHIPM-LINE(INDX)             
144100         MOVE W-TIAVIDAT-RAD(2:6)  TO RESP-TIAVIDAT-LINE(INDX)            
144200         MOVE INL-IDLBBET          TO RESP-IDLBBET-LINE(INDX)             
144300         PERFORM FCB-RAEKNA-PARTIER                                       
144400         MOVE W-KVPARTI            TO RESP-KVPARTI-LINE(INDX)             
144500*                                                                         
144600         IF INL-FLFEL              =  JA                                  
144700           IF REQU-IDLAND-SPR = 'GB'                                      
144800             MOVE YES              TO RESP-FLFEL-LINE(INDX)               
144900           ELSE                                                           
145000             MOVE JA               TO RESP-FLFEL-LINE(INDX)               
145100           END-IF                                                         
145200         ELSE                                                             
145300           MOVE SPACE              TO RESP-FLFEL-LINE(INDX)               
145400         END-IF                                                           
145500*                                                                         
145600         PERFORM FCC-HAMTA-ADINLOMR-FB                                    
145700         ADD 1  TO INDX                                                   
145800         ADD 1  TO RESP-KVRADER                                           
145900                                                                          
146000         IF INLA-LAES                                                     
146100           CONTINUE                                                       
146200         ELSE                                                             
146300           PERFORM FCA-LAES-RADDATA                                       
146400         END-IF                                                           
146500     END-PERFORM                                                          
146600                                                                          
146700     IF INLA-LAES  AND SEGMENT-FINNS AND INL-TIINLMOT > ZERO              
146800       IF REQU-UPDATE                                                     
146900         CONTINUE                                                         
147000       ELSE                                                               
147100         MOVE ERR-KEYS-MISSING TO RESP-IDMSG-ERROR                        
147200       END-IF                                                             
147300     END-IF                                                               
147400                                                                          
147500     IF SEGMENT-SAKNAS OR INLA-LAES                                       
147600       MOVE SPACE                    TO RESP-IDLEVNR-NEXT                 
147700                                        RESP-IDFS-NEXT                    
147800                                        RESP-IDLBBET-NEXT                 
147900       MOVE ZERO                     TO RESP-TIAVIDAT-NEXT                
148000     ELSE                                                                 
148100       IF SEGMENT-FINNS                                                   
148200         MOVE INL-IDLEVNR            TO RESP-IDLEVNR-NEXT                 
148300         MOVE INL-IDFS               TO RESP-IDFS-NEXT                    
148400         MOVE INL-TIAVIDAT           TO RESP-TIAVIDAT-NEXT                
148500         MOVE INL-IDLBBET            TO RESP-IDLBBET-NEXT                 
148600         IF REQU-UPDATE                                                   
148700           CONTINUE                                                       
148800         ELSE                                                             
148900           MOVE INF-MORE-INFO-EXISTS TO RESP-IDMSG-INFO                   
149000         END-IF                                                           
149100       ELSE                                                               
149200         MOVE INF-LAST-PAGE          TO RESP-IDMSG-INFO                   
149300       END-IF                                                             
149400     END-IF                                                               
149500     .                                                                    
149600     EJECT                                                                
149700 FCA-LAES-RADDATA SECTION.                                                
149800     IF INLB-LAES                                                         
149900*      READ WITH W6D1-KEYS + IDLBBET                                      
150000       PERFORM FCAA-INLB-LAESNING                                         
150100     ELSE                                                                 
150200       IF INLA-LAES                                                       
150300*        READ WITH W6D1-KEYS + TIAVIDAT + IDLEVNR                         
150400         PERFORM FCAB-INLA-LAESNING                                       
150500       ELSE                                                               
150600*        READ WITH W6D1-KEYS + IDFS                                       
150700         PERFORM FCAC-INLA-SEQ-LAESNING                                   
150800       END-IF                                                             
150900     END-IF                                                               
151000     .                                                                    
151100     EJECT                                                                
151200 FCAA-INLB-LAESNING    SECTION.                                           
151300     MOVE LOW-VALUE            TO W-W6D1A1KY-MIN-X                        
151400     MOVE HIGH-VALUE           TO W-W6D1A1KY-MAX-X                        
151500                                                                          
151600     MOVE WS-IDLBBET           TO W-IDLBBET                               
151700                                                                          
151800     IF INDX = 1                                                          
151900       IF (REQU-QUERY OR REQU-NEXT) AND NOT REQU-UPDATE                   
152000         MOVE WS-IDLBBET       TO W-D1A1KY-IDLBBET                        
152100         PERFORM IMS-GU-INLB-INLB01-KVAL                                  
152200       ELSE                                                               
152300         IF REQU-UPDATE                                                   
152400           MOVE INL-IDLEVNR        TO W-D1A1KY-IDLEVNR-MIN                
152500           MOVE INL-IDFS           TO W-D1A1KY-IDFS-MIN                   
152600           MOVE INL-TIAVIDAT       TO W-D1A1KY-TIAVIDAT-MIN               
152700           MOVE INL-IDLBBET        TO W-D1A1KY-IDLBBET-MIN                
152800         END-IF                                                           
152900         PERFORM IMS-GU-INLB-INLB01                                       
153000       END-IF                                                             
153100     ELSE                                                                 
153200       PERFORM IMS-GN-INLB-INLB01                                         
153300     END-IF                                                               
153400                                                                          
153500     IF SEGMENT-FINNS                                                     
153600       MOVE WS-IDDC            TO W-D101KY-IDDC                           
153700       MOVE SEQA-IDLEVNR       TO W-D101KY-IDLEVNR                        
153800       MOVE SEQA-IDFS          TO W-D101KY-IDFS                           
153900       MOVE SEQA-TIAVIDAT      TO W-D101KY-TIAVIDAT                       
154000       PERFORM IMS-GU-INLA1-INLA01                                        
154100     END-IF                                                               
154200     .                                                                    
154300     EJECT                                                                
154400 FCAB-INLA-LAESNING    SECTION.                                           
154500     MOVE WS-IDDC              TO W-D101KY-IDDC                           
154600     MOVE WS-IDLEVNR           TO W-D101KY-IDLEVNR                        
154700     MOVE WS-IDFS              TO W-D101KY-IDFS                           
154800     IF WS-TIAVIDAT NUMERIC                                               
154900        MOVE WS-TIAVIDAT          TO W-D101KY-TIAVIDAT                    
155000     ELSE                                                                 
155100        MOVE ZERO                 TO W-D101KY-TIAVIDAT                    
155200     END-IF                                                               
155300     PERFORM IMS-GU-INLA1-INLA01                                          
155400     .                                                                    
155500     EJECT                                                                
155600 FCAC-INLA-SEQ-LAESNING    SECTION.                                       
155700     IF W-D1ASEQ-IDLEVNR = SPACE AND                                      
155800        W-D1ASEQ-IDFS    = SPACE AND                                      
155900        W-D1ASEQ-TIAVIDAT = ZERO AND                                      
156000        W-D1ASEQ-IDLBBET = SPACE AND                                      
156100        REQU-NEXT                                                         
156200       MOVE 'GE'    TO STATUS-WS                                          
156300     ELSE                                                                 
156400       MOVE LOW-VALUE        TO W-W6D1ASEQ-MIN-X                          
156500                                W-D1ASEQ-IDLBBET                          
156600       MOVE HIGH-VALUE       TO W-W6D1ASEQ-MAX-X                          
156700                                                                          
156800       MOVE WS-IDDC          TO W-D1ASEQ-IDDC                             
156900                                W-D1ASEQ-IDDC-MIN                         
157000                                W-D1ASEQ-IDDC-MAX                         
157100                                                                          
157200       MOVE WS-IDLEVNR       TO W-D1ASEQ-IDLEVNR-MIN                      
157300                                W-D1ASEQ-IDLEVNR-MAX                      
157400       IF WS-IDFS NOT = SPACE                                             
157500         MOVE WS-IDFS        TO W-D1ASEQ-IDFS-MIN                         
157600                                W-D1ASEQ-IDFS-MAX                         
157700       END-IF                                                             
157800                                                                          
157900       IF INDX                   =  1                                     
158000         IF (REQU-KDPGMACT = SPACE                                        
158100         OR  REQU-NEXT) AND NOT REQU-UPDATE                               
158200             PERFORM IMS-GU-INLA2-INLA01-KVAL                             
158300         ELSE                                                             
158400           IF REQU-UPDATE                                                 
158500             MOVE INL-IDFS TO W-D1ASEQ-IDFS-MIN                           
158600           END-IF                                                         
158700           PERFORM IMS-GU-INLA2-INLA01                                    
158800         END-IF                                                           
158900       ELSE                                                               
159000         PERFORM IMS-GN-INLA2-INLA01                                      
159100       END-IF                                                             
159200     END-IF                                                               
159300     .                                                                    
159400     EJECT                                                                
159500 FCB-RAEKNA-PARTIER SECTION.                                              
159600     MOVE ZERO                 TO W-KVPARTI                               
159700     IF INLA-LAES OR INLB-LAES                                            
159800       PERFORM IMS-GNP-INLA1-INLA11                                       
159900     ELSE                                                                 
160000       PERFORM IMS-GNP-INLA2-INLA11                                       
160100     END-IF                                                               
160200     PERFORM UNTIL SEGMENT-SAKNAS                                         
160300       ADD +1                  TO W-KVPARTI                               
160400       IF INLA-LAES OR INLB-LAES                                          
160500         PERFORM IMS-GNP-INLA1-INLA11                                     
160600       ELSE                                                               
160700         PERFORM IMS-GNP-INLA2-INLA11                                     
160800       END-IF                                                             
160900     END-PERFORM                                                          
161000     .                                                                    
161100     EJECT                                                                
161200 FCC-HAMTA-ADINLOMR-FB SECTION.                                           
161300     MOVE INL-IDLEVNR TO STYR-IDLEVNR                                     
161400     MOVE +0          TO STYR-IDFKNGRP                                    
161500                         STYR-IDARTNR                                     
161600                         STYR-BEFT                                        
161700                                                                          
161800     CALL W611STYR USING STYR-W611STYR STYR-HANB-PCB STYR-PLAA-PCB        
161900     IF STYR-KDSVAR-OK                                                    
162000       MOVE STYR-ADINLOMR-FB     TO RESP-ADINLOMR-FB-LINE(INDX)           
162100                                                                          
162200       IF CDC                                                             
162300         IF STYR-ADINLOMR-FB = 'FB? ' OR 'INS?'                           
162400           MOVE STYR-IDDC   TO SW-WS-IDDC                                 
162500           IF SW-CDC-SE                                                   
162600             MOVE WC-CDC-TR TO STYR-IDDC                                  
162700           ELSE                                                           
162800             MOVE WC-CDC-SE TO STYR-IDDC                                  
162900           END-IF                                                         
163000           CALL W611STYR USING STYR-W611STYR STYR-HANB-PCB                
163100                                             STYR-PLAA-PCB                
163200           IF STYR-KDSVAR-OK                                              
163300             IF STYR-ADINLOMR-FB = 'FB? ' OR 'INS?'                       
163400             MOVE RESP-ADINLOMR-FB-LINE(INDX) TO STYR-ADINLOMR-FB         
163500               MOVE STYR-IDDC   TO SW-WS-IDDC                             
163600               IF SW-CDC-SE                                               
163700                 MOVE WC-CDC-TR TO STYR-IDDC                              
163800               ELSE                                                       
163900                 MOVE WC-CDC-SE TO STYR-IDDC                              
164000               END-IF                                                     
164100             ELSE                                                         
164200             MOVE STYR-ADINLOMR-FB TO RESP-ADINLOMR-FB-LINE(INDX)         
164300             END-IF                                                       
164400           ELSE                                                           
164500                                                                          
164600             MOVE SPACE            TO RESP-ADINLOMR-FB-LINE(INDX)         
164700           END-IF                                                         
164800         END-IF                                                           
164900       END-IF                                                             
165000     ELSE                                                                 
165100                                                                          
165200       MOVE SPACE               TO RESP-ADINLOMR-FB-LINE(INDX)            
165300     END-IF                                                               
165400     .                                                                    
165500     EJECT                                                                
165600 G-CONTROL-INPUT SECTION.                                                 
165700     MOVE JA   TO REQU-KDCMDVAL-SW                                        
165800     MOVE +1   TO INDX                                                    
165900     PERFORM UNTIL INDX        >  MAX-KVRADER                             
166000       IF REQU-KDCMDVAL-LINE(INDX) NOT =      ALL '+'                     
166100         MOVE NEJ            TO REQU-KDCMDVAL-SW                          
166200       END-IF                                                             
166300       ADD +1                TO INDX                                      
166400     END-PERFORM                                                          
166500                                                                          
166600     MOVE SPACE                     TO RESP-IDMSG-ERROR                   
166700     IF  REQU-FLKLAR-UPD  =  ALL '+'                                      
166800     AND REQU-KDCMDVAL-ALL-PLUS                                           
166900       MOVE ERR-PF11-AND-NO-DATA    TO RESP-IDMSG-ERROR                   
167000       MOVE NEJ                     TO INDATA-SW                          
167100     ELSE                                                                 
167200       IF REQU-FLKLAR-UPD      =  ALL '+' OR NEJ                          
167300         MOVE 'A'              TO AKTIVERING-SW                           
167400         PERFORM GA-KOLLA-AKTIVERING-AVI                                  
167500       ELSE                                                               
167600*NDC:ER SKA INTE KUNNA GÖRA BIL-KLAR                                      
167700*                                                                         
167800         IF CDC                                                           
167900           MOVE 'B'              TO AKTIVERING-SW                         
168000           PERFORM GB-KOLLA-AKTIVERING-BIL                                
168100         ELSE                                                             
168200           MOVE NEJ TO INDATA-SW                                          
168300           MOVE ERR-UPDATE-FORBIDDEN TO RESP-IDMSG-ERROR                  
168400         END-IF                                                           
168500       END-IF                                                             
168600                                                                          
168700       IF INDATA-FEL                                                      
168800         IF RESP-IDMSG-ERROR           =  SPACE                           
168900           MOVE ERR-CORR-HILITE-FLDS TO RESP-IDMSG-ERROR                  
169000         END-IF                                                           
169100       END-IF                                                             
169200     END-IF                                                               
169300     .                                                                    
169400     EJECT                                                                
169500 GA-KOLLA-AKTIVERING-AVI   SECTION.                                       
169600     PERFORM GAA-KOLLA-KDCMDVAL                                           
169700     PERFORM GAB-KOLLA-FLFEL-KDINLSTA                                     
169800                                                                          
169900**KOLLA IDLBBET GÖRS ENDAST FÖR CDC, NDC SKA INTE ANVÄNDA BILD            
170000* 6121, LOSSNING                                                          
170100* GÖRS NUMERA ÄVEN FÖR KINA OCH USA                                       
170200                                                                          
170300     IF CDC OR NDC-CN OR NDC-US                                           
170400       PERFORM GAC-KOLLA-IDLBBET                                          
170500     END-IF                                                               
170600     PERFORM GAD-KOLLA-ADINLOMR-LPL                                       
170700                                                                          
170800     IF WS-FLWEBDC = 'J'                                                  
170900       CONTINUE                                                           
171000     ELSE                                                                 
171100       PERFORM GAE-KOLLA-ADINLOMR-PRT                                     
171200     END-IF                                                               
171300     .                                                                    
171400     EJECT                                                                
171500 GAA-KOLLA-KDCMDVAL      SECTION.                                         
171600     MOVE NEJ                  TO KDCMDVAL-SW                             
171700     MOVE +1                   TO INDX                                    
171800     PERFORM UNTIL INDX        >  MAX-KVRADER OR                          
171900                   REQU-IDLEVNR-LINE(INDX) <= SPACE                       
172000       IF REQU-KDCMDVAL-LINE(INDX) = 'V   ' OR 'S   ' OR SPACE OR         
172100                                ALL '+'                                   
172200         MOVE MFS-ALPHA-FIELD-OK TO RESP-KDCMDVAL-LINE-ATTR(INDX)         
172300                                                                          
172400         MOVE REQU-KDCMDVAL-LINE(INDX) TO RESP-KDCMDVAL-LINE(INDX)        
172500                                                                          
172600         IF REQU-KDCMDVAL-LINE(INDX) = SPACE OR ALL '+'                   
172700           CONTINUE                                                       
172800         ELSE                                                             
172900           MOVE JA           TO KDCMDVAL-SW                               
173000         END-IF                                                           
173100       ELSE                                                               
173200         MOVE MFS-ALPHA-FIELD-WRONG                                       
173300                           TO RESP-KDCMDVAL-LINE-ATTR(INDX)               
173400         MOVE ERR-CORR-HILITE-FLDS                                        
173500                           TO RESP-IDMSG-ERROR                            
173600         MOVE NEJ              TO INDATA-SW                               
173700       END-IF                                                             
173800       ADD +1                  TO INDX                                    
173900     END-PERFORM                                                          
174000                                                                          
174100     IF INLB-LAES                                                         
174200       CONTINUE                                                           
174300     ELSE                                                                 
174400       IF KDCMDVAL-EJ-IFYLLD AND INDX > 2                                 
174500         MOVE ERR-UPDATE-FORBIDDEN TO RESP-IDMSG-ERROR                    
174600         MOVE NEJ              TO INDATA-SW                               
174700       END-IF                                                             
174800     END-IF                                                               
174900     .                                                                    
175000     EJECT                                                                
175100 GAB-KOLLA-FLFEL-KDINLSTA  SECTION.                                       
175200                                                                          
175300     MOVE +1               TO INDX                                        
175400     MOVE WS-IDDC          TO W-D101KY-IDDC                               
175500                                                                          
175600     PERFORM UNTIL INDX        >  MAX-KVRADER                             
175700       IF (REQU-KDCMDVAL-LINE (INDX) = 'V   ' OR 'S   ') OR               
175800          (INLA-LAES       AND INDX = 1)                 OR               
175900          (INLA-SEQ-LAES AND INDX = 1 AND KDCMDVAL-EJ-IFYLLD) OR          
176000          (INLB-LAES AND REQU-IDFS-LINE(INDX) NOT = SPACE AND             
176100           KDCMDVAL-EJ-IFYLLD)                                            
176200         MOVE REQU-IDLEVNR-LINE(INDX) TO W-D101KY-IDLEVNR                 
176300         MOVE REQU-IDFS-LINE   (INDX) TO W-D101KY-IDFS                    
176400         MOVE REQU-TIAVIDAT-LINE(INDX) TO W-D101KY-TIAVIDAT               
176500                                                                          
176600         PERFORM IMS-GU-INLA1-INLA01                                      
176700         IF SEGMENT-FINNS                                                 
176800           MOVE INL-IDARTNR TO W-IDRADNR-INL                              
176900           PERFORM IMS-GNP-INLA1-INLA21-KVAL                              
177000           IF SEGMENT-SAKNAS OR RAD-KDINLSTA = SPACE OR                   
177100              INL-FLFEL        = JA                                       
177200             MOVE NEJ          TO INDATA-SW                               
177300             MOVE ERR-UPDATE-FORBIDDEN TO RESP-IDMSG-ERROR                
177400             MOVE MFS-ADD-HILIGHT-FIELD                                   
177500                           TO RESP-IDLEVNR-LINE-ATTR (INDX)               
177600                              RESP-IDFS-LINE-ATTR (INDX)                  
177700           END-IF                                                         
177800         ELSE                                                             
177900           MOVE NEJ            TO INDATA-SW                               
178000           MOVE ERR-UPDATE-FORBIDDEN TO RESP-IDMSG-ERROR                  
178100         END-IF                                                           
178200       END-IF                                                             
178300       ADD +1                  TO INDX                                    
178400     END-PERFORM                                                          
178500     .                                                                    
178600     EJECT                                                                
178700 GAC-KOLLA-IDLBBET       SECTION.                                         
178800     IF INLB-LAES                                                         
178900       IF REQU-IDLBBET-UPD             =  SPACE OR ALL '+'                
179000         MOVE WS-IDLBBET             TO W-6108-IDLBBET                    
179100       ELSE                                                               
179200         MOVE REQU-IDLBBET-UPD       TO W-6108-IDLBBET                    
179300       END-IF                                                             
179400       PERFORM IMS-GU-LASA-LASA11                                         
179500       IF  SEGMENT-FINNS                                                  
179600       AND LASA-6108-ADINLOMR-LPL NOT = SPACE                             
179700         MOVE NEJ                   TO INDATA-SW                          
179800         MOVE ERR-UPDATE-FORBIDDEN  TO RESP-IDMSG-ERROR                   
179900         MOVE MFS-ALPHA-FIELD-WRONG TO RESP-IDLBBET-UPD-ATTR              
180000       END-IF                                                             
180100     ELSE                                                                 
180200       IF REQU-IDLBBET-UPD      =  ALL '+'  OR SPACE                      
180300                                                                          
180400         MOVE MFS-ALPHA-FIELD-WRONG   TO RESP-IDLBBET-UPD-ATTR            
180500         MOVE NEJ                     TO INDATA-SW                        
180600       ELSE                                                               
180700         MOVE REQU-IDLBBET-UPD        TO W-6108-IDLBBET                   
180800         PERFORM IMS-GU-LASA-LASA11                                       
180900         IF  SEGMENT-FINNS                                                
181000         AND LASA-6108-ADINLOMR-LPL  NOT = SPACE                          
181100           MOVE NEJ                   TO INDATA-SW                        
181200           MOVE ERR-UPDATE-FORBIDDEN  TO RESP-IDMSG-ERROR                 
181300           MOVE MFS-ALPHA-FIELD-WRONG TO RESP-IDLBBET-UPD-ATTR            
181400         ELSE                                                             
181500           MOVE MFS-ALPHA-FIELD-OK    TO RESP-IDLBBET-UPD-ATTR            
181600         END-IF                                                           
181700       END-IF                                                             
181800     END-IF                                                               
181900     .                                                                    
182000     EJECT                                                                
182100 GAD-KOLLA-ADINLOMR-LPL  SECTION.                                         
182200     IF REQU-ADINLOMR-LPL-UPD       =  ALL '+'                            
182300       MOVE MFS-ALPHA-FIELD-OK     TO RESP-ADINLOMR-LPL-UPD-ATTR          
182400     ELSE                                                                 
182500       MOVE MFS-ALPHA-FIELD-WRONG  TO RESP-ADINLOMR-LPL-UPD-ATTR          
182600       MOVE NEJ                    TO INDATA-SW                           
182700     END-IF                                                               
182800     .                                                                    
182900     EJECT                                                                
183000 GAE-KOLLA-ADINLOMR-PRT  SECTION.                                         
183100     MOVE SPACE                       TO PRT-IDPRTLST                     
183200     MOVE SPACE                       TO W-IDPRTLST-VAL                   
183300     MOVE '6L'                        TO PRT-IDPRTLST(1:2)                
183400     MOVE WS-ADINLOMR-PRT             TO PRT-IDPRTLST(3:6)                
183500     MOVE 1                           TO PRT-KDCALL                       
183600     CALL W006PRT  USING PRT-W006PRT                                      
183700     IF PRT-KDSVAR =  'F'                                                 
183800       MOVE SPACE                     TO PRT-IDPRTLST                     
183900       MOVE '6F'                      TO PRT-IDPRTLST(1:2)                
184000       MOVE WS-ADINLOMR-PRT           TO PRT-IDPRTLST(3:6)                
184100       MOVE 1                         TO PRT-KDCALL                       
184200       CALL W006PRT USING PRT-W006PRT                                     
184300       IF PRT-KDSVAR  =  'F'                                              
184400          MOVE NEJ                    TO INDATA-SW                        
184500          MOVE ERR-WRONG-PRINTER      TO RESP-IDMSG-ERROR                 
184600       ELSE                                                               
184700          MOVE '6F'                   TO W-IDPRTLST-VAL                   
184800       END-IF                                                             
184900     ELSE                                                                 
185000        MOVE '6L'                     TO W-IDPRTLST-VAL                   
185100     END-IF                                                               
185200     .                                                                    
185300     EJECT                                                                
185400 GB-KOLLA-AKTIVERING-BIL   SECTION.                                       
185500     PERFORM GBA-KOLLA-FLKLAR                                             
185600     PERFORM GBB-KOLLA-ADINLOMR-LPL                                       
185700     PERFORM GBC-KOLLA-IDLBBET                                            
185800     .                                                                    
185900     EJECT                                                                
186000 GBA-KOLLA-FLKLAR        SECTION.                                         
186100     IF REQU-FLKLAR-UPD            =   JA  OR  YES                        
186200       MOVE MFS-ALPHA-FIELD-OK TO RESP-FLKLAR-UPD-ATTR                    
186300     ELSE                                                                 
186400       MOVE NEJ                  TO INDATA-SW                             
186500       MOVE MFS-ALPHA-FIELD-WRONG TO RESP-FLKLAR-UPD-ATTR                 
186600     END-IF                                                               
186700     .                                                                    
186800     EJECT                                                                
186900 GBB-KOLLA-ADINLOMR-LPL          SECTION.                                 
187000     IF REQU-ADINLOMR-LPL-UPD      NOT = ALL '+'                          
187100       MOVE REQU-ADINLOMR-LPL-UPD TO W-6006-ADINLOMR                      
187200       PERFORM IMS-GU-PLAA-PLAA11                                         
187300       IF SEGMENT-FINNS                                                   
187400         IF PLAA-6006-KDINLOMR = 'LPL'                                    
187500             MOVE MFS-ALPHA-FIELD-OK TO                                   
187600                                     RESP-ADINLOMR-LPL-UPD-ATTR           
187700         ELSE                                                             
187800           MOVE NEJ                   TO INDATA-SW                        
187900           MOVE MFS-ALPHA-FIELD-WRONG TO                                  
188000                                      RESP-ADINLOMR-LPL-UPD-ATTR          
188100         END-IF                                                           
188200       ELSE                                                               
188300         MOVE NEJ                    TO INDATA-SW                         
188400         MOVE MFS-ALPHA-FIELD-WRONG  TO                                   
188500                                     RESP-ADINLOMR-LPL-UPD-ATTR           
188600       END-IF                                                             
188700     ELSE                                                                 
188800       MOVE MFS-ALPHA-FIELD-OK     TO RESP-ADINLOMR-LPL-UPD-ATTR          
188900     END-IF                                                               
189000     .                                                                    
189100     EJECT                                                                
189200 GBC-KOLLA-IDLBBET       SECTION.                                         
189300     IF INLB-LAES                                                         
189400       IF REQU-IDLBBET-UPD  =  SPACE OR ALL '+'                           
189500         MOVE WS-IDLBBET               TO W-6108-IDLBBET                  
189600         PERFORM IMS-GU-LASA-LASA11                                       
189700         IF SEGMENT-SAKNAS OR                                             
189800            LASA-6108-ADINLOMR-LPL NOT = SPACE                            
189900             MOVE NEJ                  TO INDATA-SW                       
190000             MOVE ERR-UPDATE-FORBIDDEN TO RESP-IDMSG-ERROR                
190100                                                                          
190200         END-IF                                                           
190300       ELSE                                                               
190400         MOVE REQU-IDLBBET-UPD         TO W-6108-IDLBBET                  
190500         PERFORM IMS-GU-LASA-LASA11                                       
190600         IF  SEGMENT-FINNS                                                
190700         AND LASA-6108-ADINLOMR-LPL = SPACE                               
190800          CONTINUE                                                        
190900           MOVE JA                 TO INDATA-SW                           
191000           MOVE MFS-ALPHA-FIELD-OK TO RESP-IDLBBET-UPD-ATTR               
191100         ELSE                                                             
191200           MOVE NEJ                TO INDATA-SW                           
191300                                                                          
191400           MOVE MFS-ALPHA-FIELD-WRONG TO RESP-IDLBBET-UPD-ATTR            
191500         END-IF                                                           
191600       END-IF                                                             
191700     ELSE                                                                 
191800       IF REQU-IDLBBET-UPD =  SPACE OR ALL '+'                            
191900                                                                          
192000         MOVE MFS-ALPHA-FIELD-WRONG   TO RESP-IDLBBET-UPD-ATTR            
192100         MOVE NEJ                     TO INDATA-SW                        
192200       ELSE                                                               
192300         MOVE REQU-IDLBBET-UPD        TO W-6108-IDLBBET                   
192400         PERFORM IMS-GU-LASA-LASA11                                       
192500         IF SEGMENT-FINNS AND LASA-6108-ADINLOMR-LPL = SPACE              
192600           MOVE MFS-ALPHA-FIELD-OK    TO RESP-IDLBBET-UPD-ATTR            
192700           MOVE JA                    TO INDATA-SW                        
192800         ELSE                                                             
192900           MOVE NEJ                   TO INDATA-SW                        
193000           MOVE MFS-ALPHA-FIELD-WRONG TO RESP-IDLBBET-UPD-ATTR            
193100         END-IF                                                           
193200       END-IF                                                             
193300     END-IF                                                               
193400     .                                                                    
193500     EJECT                                                                
193600 H-UPDATE SECTION.                                                        
193700     MOVE ALL '+'                TO RESP-IDLBBET-UPD                      
193800     IF AKTIVERING-AVI                                                    
193900       PERFORM HA-UPPDATERA-AKTIVERING-AVI                                
194000     ELSE                                                                 
194100       PERFORM HB-UPPDATERA-AKTIVERING-BIL                                
194200     END-IF                                                               
194300                                                                          
194400     MOVE INF-UPDATE-DONE      TO RESP-IDMSG-INFO                         
194500     PERFORM MFS-FORM-DEFAULT-ATTR                                        
194600     PERFORM MFS-ERASE-FIELD-IN                                           
194700     .                                                                    
194800     EJECT                                                                
194900 HA-UPPDATERA-AKTIVERING-AVI SECTION.                                     
195000     IF REQU-IDLBBET-UPD    = ALL '+' OR SPACE                            
195100       MOVE WS-IDLBBET         TO W-IDLBBET                               
195200     ELSE                                                                 
195300       MOVE REQU-IDLBBET-UPD   TO W-IDLBBET                               
195400     END-IF                                                               
195500                                                                          
195600     MOVE W-IDLBBET            TO W-6108-IDLBBET                          
195700*NDC, LASTBÄRARE FINNS BARA PÅ CDC, 6121-BILDEN SKA INTE ANVÄNDAS         
195800*    AV NDC:ERNA                                                          
195900* NUMER ÄVEN KINA OCH USA                                                 
196000     IF CDC OR NDC-CN OR NDC-US                                           
196100       PERFORM IMS-GU-LASA-LASA11                                         
196200       IF SEGMENT-SAKNAS                                                  
196300         PERFORM HAA-SKAPA-LASA11                                         
196400       END-IF                                                             
196500     END-IF                                                               
196600                                                                          
196700     PERFORM IMS-GHU-LOPA-LOPA11                                          
196800*NDC HAR EN EGEN LÖPNUMMERSERIE SEPARAT FRÅN CDC:S                        
196900*JAPAN/AUSTRALIEN/KINA YTTERLIGARE EN                                     
197000     IF CDC                                                               
197100       MOVE LOPA-6018-IDLOPNRM         TO W-IDLOPNRM                      
197200                                          W-IDLOPNRM-LOPA                 
197300     ELSE                                                                 
197400       IF NDC-PACIFIC OR NDC-CN OR NDC-AE                                 
197500         MOVE LOPA-6018-IDLOPNRM-JP-AU TO W-IDLOPNRM                      
197600                                          W-IDLOPNRM-LOPA                 
197700       ELSE                                                               
197800         MOVE LOPA-6018-IDLOPNRM-NDC   TO W-IDLOPNRM                      
197900                                          W-IDLOPNRM-LOPA                 
198000       END-IF                                                             
198100     END-IF                                                               
198200                                                                          
198300     MOVE +1                  TO INDX                                     
198400                                                                          
198500     MOVE INL-TAB-TOM         TO  INL-TAB                                 
198600     MOVE ZEROS               TO  I-TAB-IX                                
198700                                  W-KVPOST-I-LISTA                        
198800                                                                          
198900     MOVE +1                  TO 6191-IX                                  
199000                                 6196-IX                                  
199100                                 6197-IX                                  
199200     MOVE JA                  TO FOERSTA-RAD-UPD-SW                       
199300                                                                          
199400     IF WS-IDDC <= SPACES                                                 
199500        MOVE REQU-IDDC-KEY    TO WS-IDDC                                  
199600     END-IF                                                               
199700                                                                          
199800     MOVE WS-IDDC             TO W-D101KY-IDDC                            
199900                                  W-D1ASEQ-IDDC                           
200000                                                                          
200100     PERFORM UNTIL INDX > MAX-KVRADER                                     
200200       IF (REQU-KDCMDVAL-LINE (INDX) = 'V   ' OR 'S   ') OR               
200300          (INLA-LAES       AND INDX = 1)                 OR               
200400          (INLA-SEQ-LAES AND INDX = 1 AND KDCMDVAL-EJ-IFYLLD) OR          
200500          (INLB-LAES AND REQU-IDFS-LINE(INDX) NOT = SPACE AND             
200600           KDCMDVAL-EJ-IFYLLD)                                            
200700         MOVE REQU-IDLEVNR-LINE (INDX)   TO W-D101KY-IDLEVNR              
200800         MOVE REQU-IDFS-LINE      (INDX) TO W-D101KY-IDFS                 
200900         MOVE REQU-TIAVIDAT-LINE(INDX)   TO W-D101KY-TIAVIDAT             
201000                                                                          
201100         IF INLA-SEQ-LAES AND FOERSTA-RAD-UPD                             
201200           MOVE REQU-IDLEVNR-LINE (INDX)     TO W-D1ASEQ-IDLEVNR          
201300           MOVE REQU-IDFS-LINE        (INDX) TO W-D1ASEQ-IDFS             
201400           MOVE REQU-TIAVIDAT-LINE(INDX)     TO W-D1ASEQ-TIAVIDAT         
201500           MOVE W-IDLBBET                    TO W-D1ASEQ-IDLBBET          
201600           MOVE NEJ                          TO FOERSTA-RAD-UPD-SW        
201700         END-IF                                                           
201800                                                                          
201900         PERFORM IMS-GHU-INLA1-INLA01                                     
202000         MOVE INL-IDARTNR            TO W-IDRADNR-INL                     
202100         MOVE INL-IDLEVNR            TO WS-IDLEVNR-2                      
202200         MOVE INL-TIAVIDAT           TO W-TIAVIDAT                        
202300         MOVE INL-IDFS               TO W-IDFS                            
202400         IF INL-IDLBBET NOT = W-IDLBBET                                   
202500           IF W-IDLBBET NOT = SPACE AND ALL '+'                           
202600             MOVE W-IDLBBET TO INL-IDLBBET                                
202700           END-IF                                                         
202800         END-IF                                                           
202900         PERFORM IMS-REPL-INLA1-INLA01                                    
203000                                                                          
203100         PERFORM IMS-GHNP-INLA1-INLA11-KVAL                               
203200         PERFORM UNTIL SEGMENT-SAKNAS                                     
203300           MOVE ART-IDARTNR           TO W-IDARTNR                        
203400                                         W-IDARTNR-C                      
203500                                         W-IDARTNR-K6                     
203600           MOVE ART-IDRADNR-INL TO W-IDRADNR-INL                          
203700           PERFORM HAA-LAES-ARTIKEL-INFO                                  
203800                                                                          
203900           PERFORM HAB-TA-UT-IDLOPNRM                                     
204000           PERFORM S08-KOLLA-STYRNING                                     
204100           IF (ART-KDRT = 0 OR 1 OR 2 OR 4 OR 5 OR 9 OR                   
204200                         10)            AND                               
204300               ART-KVAVIS > ZERO        AND                               
204400               INL-KDINL        = 'R31' AND                               
204500               ART-FLSPLPART = NEJ AND                                    
204600               (CDC OR NDC-CN OR NDC-US)                                  
204700             PERFORM HAC-KVALITETS-KONTROLL                               
204800           ELSE                                                           
204900             MOVE NEJ                 TO  KVAL-FLKVAKAR                   
205000             MOVE ZERO                TO  KVAL-KDKVAANT                   
205100                                  KVAL-KVKVAPRIM                          
205200                                  KVAL-KVKVASEK                           
205300           END-IF                                                         
205400                                                                          
205500           IF ART-ADTRDEST(1:2) NOT = 'CD'                                
205600             PERFORM HAE-PRIORITERING                                     
205700           END-IF                                                         
205800                                                                          
205900           IF CDC                                                         
206000             IF ARTC11-CLAG-FLCDART        = NEJ     OR                   
206100               ARTC11-CLAG-KVROS           > 0       OR                   
206200               ARTC11-CLAG-KVQPACK-3 = 0             OR                   
206300               ART-FLSPLPART               = JA      OR                   
206400               (ART-KDRT = 6 OR 7 OR 77 OR 8)        OR                   
206500               KVAL-KVKVAPRIM              > 0       OR                   
206600               KVAL-KVKVASEK               > 0       OR                   
206700               KVAL-KDKVAANT               > 0       OR                   
206800               PRIO-KDINLPRIO              < 30                           
206900               CONTINUE                                                   
207000             ELSE                                                         
207100               PERFORM HAJ-KOLLA-CD-PARTI                                 
207200             END-IF                                                       
207300           END-IF                                                         
207400                                                                          
207500                                                                          
207600           IF (CDC) AND ART-ADTRDEST(1:2) NOT = 'CD'                      
207700             PERFORM HAD-BERAEKNA-OFOERAEDLAT                             
207800             IF (ARTC11-CLAG-ADLAGOMR = 11 OR 22)                         
207900             AND (PRIO-KDINLPRIO >= 10 AND <= 19)                         
208000              IF FL-BUFFOMR = JA                                          
208100               PERFORM HAEA-SEND-SYNQ                                     
208200              END-IF                                                      
208300             END-IF                                                       
208400           END-IF                                                         
208500                                                                          
208600           PERFORM HAM-KOLLA-LOSSLISTA-ILISTA                             
208700           PERFORM HAF-UPPD-INLA11-21-6191-TRANS                          
208800*NDC SKA INTE HA LOSS-LISTA UTSKRIVEN (ANVÄMDER INTE 6121-BILDEN)         
208900*                                                                         
209000           IF LOSSLISTA AND                                               
209100           (CDC OR NDC-CN)                                                
209200             PERFORM HAG-UPPDATERA-LASA21                                 
209300           END-IF                                                         
209400           IF WS-FLWEBDC = 'J'                                            
209500             CONTINUE                                                     
209600           ELSE                                                           
209700             PERFORM HAH-SKAPA-EV-6196-TRANS                              
209800             PERFORM HAI-SKAPA-EV-6197-TRANS                              
209900           END-IF                                                         
210000           PERFORM IMS-GHNP-INLA1-INLA11                                  
210100         END-PERFORM                                                      
210200         PERFORM HAL-BEHANDLA-6192-TRANS                                  
210300         PERFORM IMS-GHU-INLA1-INLA01                                     
210400*WL01TIDZ                                                                 
210500         PERFORM I-CALL-WL01TIDZ                                          
210600         MOVE TIDZ-MSGI-TILOKDAT TO INL-TIINLMOT                          
210700                                                                          
210800         MOVE ZERO               TO INL-IDARTNR                           
210900         PERFORM IMS-REPL-INLA1-INLA01                                    
211000       END-IF                                                             
211100       ADD +1                   TO INDX                                   
211200     END-PERFORM                                                          
211300                                                                          
211400     IF I-TAB-IX               > ZERO                                     
211500       PERFORM HAN-BEHANDLA-ILISTA                                        
211600       PERFORM S07-STARTA-6199-TRANS                                      
211700     END-IF                                                               
211800                                                                          
211900     IF 6191-IX                > 1                                        
212000       PERFORM S03-STARTA-6191-TRANS                                      
212100     END-IF                                                               
212200                                                                          
212300     IF 6196-IX                > 1                                        
212400       PERFORM S05-STARTA-6196-TRANS                                      
212500     END-IF                                                               
212600                                                                          
212700     IF 6197-IX                > 1                                        
212800       PERFORM S06-STARTA-6197-TRANS                                      
212900     END-IF                                                               
213000                                                                          
213100     PERFORM IMS-REPL-LOPA-LOPA11                                         
213200     .                                                                    
213300     EJECT                                                                
213400 HAA-SKAPA-LASA11  SECTION.                                               
213500     MOVE W-IDLBBET            TO LASA-6108-IDLBBET                       
213600     MOVE LOW-VALUE            TO LASA-6108-LOW-VALUE                     
213700     MOVE SPACE                TO LASA-6108-ADINLOMR-LPL                  
213800     MOVE DAT-TIAAMMDD         TO LASA-6108-TIREGDAT                      
213900     MOVE ZERO                 TO LASA-6108-VLLBNTO                       
214000                                                                          
214100     PERFORM IMS-ISRT-LASA-LASA11                                         
214200     .                                                                    
214300     EJECT                                                                
214400 HAA-LAES-ARTIKEL-INFO  SECTION.                                          
214500     PERFORM IMS-GU-ARTC-ARTC11                                           
214600                                                                          
214700     IF SEGMENT-FINNS AND                                                 
214800     CDC                                                                  
214900       MOVE ARTC11-CLAG-KDVVKL       TO W-KDVVKL                          
215000       MOVE ARTC11-CLAG-KVPB-SEP     TO W-KVPB-SEP-CL                     
215100       MOVE ARTC11-CLAG-KVPB-SATS    TO W-KVPB-SATS-CL                    
215200       MOVE ARTC11-CLAG-KVPB-TPO     TO W-KVPB-TPO-CL                     
215300       MOVE ARTC11-CLAG-FLFSP        TO W-FLFSP-CL                        
215400       MOVE ARTC11-CLAG-FLEJBUFF     TO W-FLEJBUFF                        
215500       MOVE ARTC11-CLAG-ADINLOMR-BOA TO W-ADINLOMR-BOA                    
215600     ELSE                                                                 
215700       MOVE ZERO                     TO W-KVPB-SEP-CL                     
215800                                        W-KVPB-SATS-CL                    
215900                                        W-KVPB-TPO-CL                     
216000       MOVE NEJ                      TO W-FLFSP-CL                        
216100     END-IF                                                               
216200                                                                          
216300     .                                                                    
216400     EJECT                                                                
216500 HAB-TA-UT-IDLOPNRM      SECTION.                                         
216600* NDC:ERNA SKA HA  LOKALT DATUM VID SKAPANDE AV IDLOPNRM,                 
216700* MAN SÄTTER DET LOKALA DATUMET I B-SECTIONEN.                            
216800* NDC:ERNAS IDLOPNRM NUMMERSERIEN BÖRJAR PÅ 6000                          
216900* JAPAN/AUSTRALIEN/KINAS  NUMMERSERIE BÖRJAR PÅ 7000                      
217000*                                                                         
217100     IF INL-KDINL              =  'R31'                                   
217200        IF WS-DAT-TIVVD = W-VVD                                           
217300          ADD +1               TO W-LLLL                                  
217400        ELSE                                                              
217500          MOVE DAT-TIAAVVD-GRP (3:3)  TO W-VVD                            
217600          IF CDC                                                          
217700            MOVE +1                   TO W-LLLL                           
217800          ELSE                                                            
217900            IF NDC-PACIFIC OR NDC-CN OR NDC-AE                            
218000              MOVE +7001              TO W-LLLL                           
218100            ELSE                                                          
218200              MOVE +6001              TO W-LLLL                           
218300            END-IF                                                        
218400          END-IF                                                          
218500        END-IF                                                            
218600        CALL CHECK USING W-IDLOPNRM-LOPA (2:7) FLT-LGD                    
218700             VAEGNINGSTAL VAEGNTAL-LGD W-K MODUL-10-11 ALT-A-B            
218800* NDC:ER HAR EGEN LÖPNUMMERSERIE SKILD FRÅN CDC:S                         
218900        IF CDC                                                            
219000          MOVE W-IDLOPNRM-LOPA      TO LOPA-6018-IDLOPNRM                 
219100                                       W-IDLOPNRM                         
219200        ELSE                                                              
219300          IF NDC-PACIFIC OR NDC-CN OR NDC-AE                              
219400            MOVE W-IDLOPNRM-LOPA    TO LOPA-6018-IDLOPNRM-JP-AU           
219500                                       W-IDLOPNRM                         
219600          ELSE                                                            
219700            MOVE W-IDLOPNRM-LOPA    TO LOPA-6018-IDLOPNRM-NDC             
219800                                       W-IDLOPNRM                         
219900          END-IF                                                          
220000        END-IF                                                            
220100     ELSE                                                                 
220200        MOVE ART-IDLOPNRM TO W-IDLOPNRM                                   
220300     END-IF                                                               
220400     .                                                                    
220500     EJECT                                                                
220600 HAC-KVALITETS-KONTROLL  SECTION.                                         
220700     MOVE STYR-ADINLOMR-FB TO W-6006-ADINLOMR                             
220800     PERFORM IMS-GU-PLAA-PLAA11-BLANK                                     
220900                                                                          
221000     MOVE PLAA-6006-FLKVARED   TO KVAL-FLKVARED                           
221100     MOVE INL-IDDC             TO KVAL-IDDC                               
221200     MOVE INL-IDLEVNR          TO KVAL-IDLEVNR                            
221300     MOVE ART-IDARTNR          TO KVAL-IDARTNR                            
221400     MOVE W-IDLOPNRM           TO KVAL-IDLOPNRM                           
221500     MOVE ART-KVAVIS           TO KVAL-KVAVIS                             
221600     MOVE ART-BEFT             TO KVAL-BEFT                               
221700                                                                          
221800     CALL W426KNTR USING KVAL-W426KNTR                                    
221900                         KVAL-ARTC-PCB  KVAL-KVAH1-PCB                    
222000                         KVAL-KVAH2-PCB KVAL-KVAG-PCB                     
222100                         KVAL-LEVA-PCB  KVAL-UPFA-PCB                     
222200                         KVAL-PROA-PCB  KVAL-XXLA-PCB                     
222300                         KVAL-KODA-PCB                                    
222400     .                                                                    
222500     EJECT                                                                
222600 HAD-BERAEKNA-OFOERAEDLAT        SECTION.                                 
222700                                                                          
222800     MOVE ZERO TO W-OFOERAEDLAT                                           
222900                                                                          
223000     IF INL-KDINL              = 'R31'                                    
223100       PERFORM IMS-GU-ARTC-ARTC01                                         
223200       IF ARTC01-ART-FLIART = JA                                          
223300         IF ARTC11-CLAG-FLLSRDEL = NEJ                                    
223400* 100% SATS                                                               
223500           MOVE ART-KVAVIS TO W-OFOERAEDLAT                               
223600         ELSE                                                             
223700           IF ARTC11-CLAG-KVPB-SATS > 0                                   
223800             MOVE ART-KVAVIS TO WS-KVAR                                   
223900             PERFORM HADA-BERAEKNA-BEHOV                                  
224000             PERFORM HADB-CHECK-BUFFER                                    
224100             MOVE ZERO TO WS-REDAN-UTTAGET                                
224200             IF ARTC11-CLAG-KVAKS-CDC > 0                                 
224300               PERFORM HADE-KOLLA-REDAN-UTTAGET                           
224400             END-IF                                                       
224500             IF ARTC11-CLAG-KVROS > 0                                     
224600               COMPUTE WS-KVROS-RES = ARTC11-CLAG-KVROS -                 
224700                                      WS-SATSBEHOV-ROS                    
224800*1 - COVER BO                                                             
224900               IF WS-KVROS-RES > 0                                        
225000                  COMPUTE WS-KVAR = WS-KVAR - WS-KVROS-RES                
225100               END-IF                                                     
225200             END-IF                                                       
225300* 2 - COVER BO-KIT                                                        
225400             MOVE WS-SATSBEHOV-ROS TO WS-SATSBEHOV-SAVE                   
225500             COMPUTE WS-SATSBEHOV-ROS = WS-SATSBEHOV-ROS -                
225600                                WS-REDAN-UTTAGET                          
225700             IF WS-SATSBEHOV-ROS < ZERO                                   
225800                MOVE ZERO TO WS-SATSBEHOV-ROS                             
225900             END-IF                                                       
226000             IF WS-SATSBEHOV-ROS > WS-KVAR                                
226100                MOVE WS-KVAR          TO W-OFOERAEDLAT                    
226200             ELSE                                                         
226300                MOVE WS-SATSBEHOV-ROS TO W-OFOERAEDLAT                    
226400             END-IF                                                       
226500             COMPUTE WS-REDAN-UTTAGET = WS-REDAN-UTTAGET -                
226600                                      WS-SATSBEHOV-SAVE                   
226700             IF WS-REDAN-UTTAGET < ZERO                                   
226800                MOVE ZERO TO WS-REDAN-UTTAGET                             
226900             END-IF                                                       
227000             COMPUTE WS-KVAR = WS-KVAR - W-OFOERAEDLAT                    
227100             IF WS-KVAR > ZERO                                            
227200               PERFORM HADD-CHECK-TPO                                     
227300* 3 - TPO ORDER                                                           
227400               COMPUTE WS-KVAR = WS-KVAR - WS-TPO-BEHOV                   
227500               IF WS-KVAR > ZERO                                          
227600* 4 - KIT DEMAND OTHER                                                    
227700                  COMPUTE WS-SATSBEHOV = WS-SATSBEHOV -                   
227800                          WS-KVBUFF-OF - WS-REDAN-UTTAGET                 
227900                  IF WS-SATSBEHOV < 0                                     
228000                     MOVE +0 TO WS-SATSBEHOV                              
228100                  END-IF                                                  
228200                  IF WS-KVAR < WS-SATSBEHOV                               
228300                    ADD WS-KVAR      TO W-OFOERAEDLAT                     
228400                  ELSE                                                    
228500                    ADD WS-SATSBEHOV TO W-OFOERAEDLAT                     
228600                  END-IF                                                  
228700               END-IF                                                     
228800             END-IF                                                       
228900           END-IF                                                         
229000         END-IF                                                           
229100       END-IF                                                             
229200     END-IF                                                               
229300     .                                                                    
229400     EJECT                                                                
229500 HADA-BERAEKNA-BEHOV SECTION.                                             
229600     PERFORM HADC-BERAEKNA-TILLDATUM                                      
229700     MOVE ZERO TO WS-SATSBEHOV                                            
229800                  WS-SATSBEHOV-ROS                                        
229900     MOVE NEJ  TO DATUM-PASSERAT                                          
230000     PERFORM IMS-GET-SATSBEHOV-FIRST                                      
230100     PERFORM UNTIL SEGMENT-SAKNAS OR DATUM-PASSERAT = JA                  
230200       IF ARTC24-SATS-TIBEHOV-SATS > WS-TILLDATUM                         
230300         MOVE JA TO DATUM-PASSERAT                                        
230400       ELSE                                                               
230500         ADD ARTC24-SATS-KVBEHOV-TOTSATS TO WS-SATSBEHOV                  
230600       END-IF                                                             
230700       PERFORM IMS-GET-SATSBEHOV-NEXT                                     
230800     END-PERFORM                                                          
230900                                                                          
231000     MOVE W-IDARTNR       TO W-IDARTNR-ESEQ                               
231100     PERFORM IMS-GET-WDJ211-ESEQ-FIRST                                    
231200     PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                         
231300       ADD  SRAD-KVSATROS TO WS-SATSBEHOV-ROS                             
231400       ADD  SRAD-KVSATRES TO WS-SATSBEHOV                                 
231500       PERFORM IMS-GET-WDJ211-ESEQ-NEXT                                   
231600     END-PERFORM                                                          
231700     .                                                                    
231800     EJECT                                                                
231900 HADB-CHECK-BUFFER SECTION.                                               
232000     MOVE ZERO TO WS-KVBUFF-OF                                            
232100     MOVE JA TO FL-BUFFOMR                                                
232200     PERFORM IMS-GU-WDD801                                                
232300     IF SEGMENT-FINNS                                                     
232400       PERFORM IMS-GNP-WDD811                                             
232500       PERFORM  UNTIL SEGMENT-SAKNAS                                      
232600         IF SEGMENT-FINNS                                                 
232700           COMPUTE WS-KVBUFF-OF = WS-KVBUFF-OF + SALDO-KVBUFF-OF          
232800           IF SALDO-ADBUFFOMR = 43 AND SALDO-IDDC = '11'                  
232900            MOVE NEJ TO FL-BUFFOMR                                        
233000           END-IF                                                         
233100           PERFORM IMS-GNP-WDD811                                         
233200         END-IF                                                           
233300       END-PERFORM                                                        
233400     END-IF                                                               
233500     .                                                                    
233600     EJECT                                                                
233700 HADC-BERAEKNA-TILLDATUM SECTION.                                         
233800     MOVE 002               TO WORK-KDCALL                                
233900     MOVE X-DAGAR           TO WORK-KVWORKD                               
234000     MOVE WC-CDC-SE         TO WORK-IDDC                                  
234100     MOVE DAGENS-DATUM      TO WORK-TIAAMMDD-FOM                          
234200     CALL WORKDAY    USING WORK-KDCALL,                                   
234300                           WORK-DATE-AREA,                                
234400                           WORK-KDSVAR                                    
234500     IF WORK-KDSVAR-OK                                                    
234600       MOVE WORK-TIAAMMDD-NEXT-WORKDAY TO DAT-I-TIDATUM                   
234700       MOVE 'AAMMDD'                   TO DAT-KDDATFORM                   
234800       CALL WDATKONV                USING DAT-KDDATFORM                   
234900                                          DAT-I-TIDATUM                   
235000                                          DAT-O-TIDATUM                   
235100                                          DAT-KDSVAR                      
235200       MOVE DAT-TIAA-VECKA             TO WS-TILLD-AA                     
235300       MOVE DAT-TIVV                   TO WS-TILLD-VV                     
235400                                                                          
235500     END-IF                                                               
235600     .                                                                    
235700     EJECT                                                                
235800 HADD-CHECK-TPO SECTION.                                                  
235900     MOVE ZERO TO WS-TPO-BEHOV                                            
236000     MOVE ART-IDARTNR TO W-IDARTNR                                        
236100     PERFORM IMS-GU-WDK901                                                
236200     IF SEGMENT-FINNS                                                     
236300       MOVE K9-ART-SUTPO-TOT TO WS-TPO-BEHOV                              
236400     END-IF                                                               
236500     .                                                                    
236600     EJECT                                                                
236700 HADE-KOLLA-REDAN-UTTAGET SECTION.                                        
236800     MOVE ART-IDARTNR TO WI-IDARTNR                                       
236900     PERFORM IMS-GU-W6D1ISEQ                                              
237000     IF SEGMENT-FINNS                                                     
237100       PERFORM UNTIL SEGMENT-SAKNAS                                       
237200         MOVE JA TO FL-UTTAG                                              
237300         IF I-ART-KVAVIS-KIT > 0                                          
237400           PERFORM UNTIL SEGMENT-SAKNAS OR FL-UTTAG = NEJ                 
237500             PERFORM IMS-GNP-W6D121                                       
237600             IF SEGMENT-FINNS                                             
237700               IF I-RAD-TIUPPDAT > 0                                      
237800                 MOVE NEJ TO FL-UTTAG                                     
237900               END-IF                                                     
238000             END-IF                                                       
238100           END-PERFORM                                                    
238200           IF FL-UTTAG = JA                                               
238300             ADD I-ART-KVAVIS-KIT TO WS-REDAN-UTTAGET                     
238400           END-IF                                                         
238500         END-IF                                                           
238600         PERFORM IMS-GN-W6D1ISEQ                                          
238700       END-PERFORM                                                        
238800     END-IF                                                               
238900     .                                                                    
239000     EJECT                                                                
239100 HAE-PRIORITERING  SECTION.                                               
239200     MOVE REQU-IDLEVNR-LINE(INDX)  TO PRIO-IDLEVNR                        
239300     MOVE REQU-IDFS-LINE   (INDX)  TO PRIO-IDFS                           
239400     MOVE REQU-TIAVIDAT-LINE(INDX) TO PRIO-TIAVIDAT                       
239500     MOVE ART-IDRADNR-INL          TO PRIO-IDRADNR-INL                    
239600     MOVE ZERO                     TO PRIO-KVUPPDAT                       
239700                                      PRIO-KDINLPRIO                      
239800                                      PRIO-KVAVIS-PRIO                    
239900     MOVE 'W6011500'             TO PRIO-IDPGM                            
240000                                                                          
240100     CALL W611PRIO USING PRIO-W611PRIO                                    
240200                         PRIO-INLA-PCB PRIO-ARTC-PCB                      
240300                         PRIO-ARTM-PCB                                    
240400                         PRIO-ORDQ-PCB                                    
240500                         PRIO-ARTS-PCB                                    
240600                         PRIO-KVAI-PCB                                    
240700                         PRIO-INLI1-PCB                                   
240800                         PRIO-KVAE-PCB                                    
240900     .                                                                    
241000     EJECT                                                                
241100 HAEA-SEND-SYNQ SECTION.                                                  
241200     MOVE 'BAC'             TO SYNQ-ORDERTYPE                             
241300     MOVE PRIO-KVAVIS-PRIO  TO SYNQ-KVBEST                                
241400     MOVE ARTC11-CLAG-ADLAGOMR TO SYNQ-ADLAGOMR-TOM                       
241500     MOVE ARTC11-CLAG-ADGANG   TO SYNQ-ADGANG-TOM                         
241600     MOVE W-IDARTNR         TO SYNQ-IDARTNR                               
241700     MOVE WS-IDDC           TO SYNQ-IDDC                                  
241800     MOVE FUNCTION CURRENT-DATE(3:6)  TO ORDDATE                          
241900     MOVE FUNCTION CURRENT-DATE(9:6)  TO ORDTIME                          
242000     MOVE WS-TIORDTIME TO SYNQ-TIORDTIME                                  
242100     CALL W488ORCR USING  SYNQ-W488ORCR SYNQ-PCB                          
242200                        SYNQ-ATAB-PCB WDQ3-PCB                            
242300     .                                                                    
242400     EJECT                                                                
242500 HAF-UPPD-INLA11-21-6191-TRANS    SECTION.                                
242600     IF ART-ADTRDEST(1:2) NOT = 'CD'                                      
242700       COMPUTE W-SUMMA-PRIO-OFR = W-OFOERAEDLAT + PRIO-KVAVIS-PRIO        
242800       IF W-SUMMA-PRIO-OFR > ART-KVAVIS                                   
242900         COMPUTE PRIO-KVAVIS-PRIO = ART-KVAVIS - W-OFOERAEDLAT            
243000         IF PRIO-KVAVIS-PRIO < ZERO                                       
243100           MOVE ZERO TO PRIO-KVAVIS-PRIO                                  
243200         END-IF                                                           
243300       END-IF                                                             
243400                                                                          
243500       IF INL-KDINL              =  'R31'                                 
243600         MOVE KVAL-FLKVAKAR      TO ART-FLKVAKAR                          
243700         MOVE KVAL-KDKVAANT      TO ART-KDKVAANT                          
243800         MOVE KVAL-KVKVAPRIM     TO ART-KVKVAPRIM-BER                     
243900         MOVE KVAL-KVKVASEK      TO ART-KVKVASEK-BER                      
244000       ELSE                                                               
244100         MOVE NEJ                TO ART-FLKVAKAR                          
244200         MOVE ZERO               TO ART-KVKVAPRIM-BER                     
244300                                    ART-KVKVASEK-BER                      
244400                                    ART-KDKVAANT                          
244500       END-IF                                                             
244600       MOVE PRIO-KDINLPRIO       TO ART-KDINLPRIO                         
244700       MOVE PRIO-KVAVIS-PRIO     TO ART-KVAVIS-PRIO                       
244800       MOVE W-OFOERAEDLAT        TO ART-KVAVIS-KIT                        
244900     END-IF                                                               
245000                                                                          
245100     MOVE W-IDLOPNRM             TO ART-IDLOPNRM                          
245200                                                                          
245300     IF ART-KDRT = 8 AND ART-KDKVAINL NOT = 'QC'                          
245400        PERFORM IMS-GU-W6D201                                             
245500        IF SEGMENT-FINNS                                                  
245600           PERFORM IMS-GNP-W6D211                                         
245700           PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT  OR               
245800                                           ART-KDKVAINL = 'QC'            
245900             IF INFO-KDKVAINF = 'R'                                       
246000                MOVE 'QC'      TO ART-KDKVAINL                            
246100             END-IF                                                       
246200             PERFORM IMS-GNP-W6D211                                       
246300           END-PERFORM                                                    
246400        END-IF                                                            
246500     END-IF                                                               
246600     PERFORM IMS-REPL-INLA1-INLA11                                        
246700                                                                          
246800                                                                          
246900     MOVE ART-IDARTNR          TO W-IDARTNR                               
247000     MOVE ART-IDRADNR-INL      TO W-IDRADNR-INL                           
247100     PERFORM IMS-GHNP-INLA1-INLA21                                        
247200     PERFORM UNTIL SEGMENT-SAKNAS                                         
247300       PERFORM S02-FYLL-I-6191MID                                         
247400       MOVE SPACE              TO RAD-KDINLSTA                            
247500*                                                                         
247600       MOVE REQU-IDDC-KEY      TO SW-WS-IDDC                              
247700       IF SW-NDC-US-RU                                                    
247800         MOVE 'US41'           TO RAD-ADINLOMR                            
247900       END-IF                                                             
248000       IF SW-NDC-US-LA                                                    
248100         MOVE 'US43'           TO RAD-ADINLOMR                            
248200       END-IF                                                             
248300       IF SW-NDC-US-SE                                                    
248400         MOVE 'US44'           TO RAD-ADINLOMR                            
248500       END-IF                                                             
248600       IF SW-NDC-US-CH                                                    
248700         MOVE 'US45'           TO RAD-ADINLOMR                            
248800       END-IF                                                             
248900       IF SW-NDC-US-JA                                                    
249000         MOVE 'US46'           TO RAD-ADINLOMR                            
249100       END-IF                                                             
249200       IF SW-NDC-US-DA                                                    
249300         MOVE 'US47'           TO RAD-ADINLOMR                            
249400       END-IF                                                             
249500       IF SW-NDC-CA                                                       
249600         MOVE 'CA51'           TO RAD-ADINLOMR                            
249700       END-IF                                                             
249800       IF SW-NDC-BR                                                       
249900         MOVE 'BR52'           TO RAD-ADINLOMR                            
250000       END-IF                                                             
250100       IF SW-NDC-MX                                                       
250200         MOVE 'MX53'           TO RAD-ADINLOMR                            
250300       END-IF                                                             
250400       IF SW-NDC-CN-71                                                    
250500         MOVE 'CN71'           TO RAD-ADINLOMR                            
250600       END-IF                                                             
250700       IF SW-NDC-CN-72                                                    
250800         MOVE 'CN72'           TO RAD-ADINLOMR                            
250900       END-IF                                                             
251000       IF SW-NDC-CN-73                                                    
251100         MOVE 'CN73'           TO RAD-ADINLOMR                            
251200       END-IF                                                             
251300       IF SW-NDC-CN-74                                                    
251400         MOVE 'CN74'           TO RAD-ADINLOMR                            
251500       END-IF                                                             
251600       IF SW-NDC-JP                                                       
251700         MOVE 'JP  '           TO RAD-ADINLOMR                            
251800       END-IF                                                             
251900       IF SW-NDC-AU                                                       
252000         MOVE 'AU  '           TO RAD-ADINLOMR                            
252100       END-IF                                                             
252200       IF SW-NDC-IN                                                       
252300         MOVE 'IN67'           TO RAD-ADINLOMR                            
252400       END-IF                                                             
252500       IF SW-NDC-KR                                                       
252600         MOVE 'KR65'           TO RAD-ADINLOMR                            
252700       END-IF                                                             
252800       IF SW-NDC-MY                                                       
252900         MOVE 'MY66'           TO RAD-ADINLOMR                            
253000       END-IF                                                             
253100       IF SW-NDC-TH-63                                                    
253200         MOVE 'TH63'           TO RAD-ADINLOMR                            
253300       END-IF                                                             
253400       IF SW-NDC-TW                                                       
253500         MOVE 'TW64'           TO RAD-ADINLOMR                            
253600       END-IF                                                             
253700       IF SW-NDC-AE                                                       
253800         MOVE 'AE  '           TO RAD-ADINLOMR                            
253900       END-IF                                                             
254000       IF SW-NDC-US-BAT                                                   
254100         MOVE 'US92'           TO RAD-ADINLOMR                            
254200       END-IF                                                             
254300       IF SW-NDC-ZA                                                       
254400         MOVE 'ZA85'           TO RAD-ADINLOMR                            
254500       END-IF                                                             
254600       IF SW-NDC-TR                                                       
254700         MOVE 'TR86'           TO RAD-ADINLOMR                            
254800       END-IF                                                             
254900       IF SW-NDC-TH-93                                                    
255000         MOVE 'TH93'           TO RAD-ADINLOMR                            
255100       END-IF                                                             
255200       PERFORM IMS-REPL-INLA1-INLA21                                      
255300       IF ILISTA                                                          
255400         PERFORM HAFA-SKAPA-ILISTTABELL                                   
255500       END-IF                                                             
255600       PERFORM IMS-GHNP-INLA1-INLA21                                      
255700     END-PERFORM                                                          
255800     .                                                                    
255900     EJECT                                                                
256000 HAFA-SKAPA-ILISTTABELL SECTION.                                          
256100     IF I-TAB-IX < I-TAB-MAX-IX                                           
256200       ADD +1                  TO I-TAB-IX                                
256300       ADD +1                  TO I-TAB-IX-MAX                            
256400       MOVE W-IDPRTLST         TO I-TAB-IDPRTLST(I-TAB-IX)                
256500       MOVE ART-ADLAGOMR       TO I-TAB-ADLAGOMR(I-TAB-IX)                
256600       MOVE '0000'             TO I-TAB-TORG(I-TAB-IX)                    
256700       MOVE ART-ADGANG         TO I-TAB-ADGANG(I-TAB-IX)                  
256800       MOVE ART-ADPLATS        TO I-TAB-ADPLATS(I-TAB-IX)                 
256900       MOVE ART-IDARTNR        TO I-TAB-IDARTNR(I-TAB-IX)                 
257000       MOVE ART-IDRADNR-INL    TO I-TAB-IDRADNR-INL(I-TAB-IX)             
257100       MOVE WS-IDLEVNR-2       TO I-TAB-IDLEVNR(I-TAB-IX)                 
257200       MOVE W-TIAVIDAT         TO I-TAB-TIAVIDAT(I-TAB-IX)                
257300       MOVE W-IDFS             TO I-TAB-IDFS(I-TAB-IX)                    
257400       MOVE RAD-IDLEVNR-KOLLI  TO I-TAB-IDLEVNR-KOLLI(I-TAB-IX)           
257500       MOVE RAD-IDOKOLLI       TO I-TAB-IDOKOLLI(I-TAB-IX)                
257600       MOVE RAD-IDRADNR        TO I-TAB-IDRADNR(I-TAB-IX)                 
257700     ELSE                                                                 
257800       MOVE 'INLISTE-TABELLEN ÖVERSKRIDEN SEKT HAFA' TO ERRORTEXT         
257900       CALL ABEND USING RKOD-ABEND                                        
258000     END-IF                                                               
258100     .                                                                    
258200     EJECT                                                                
258300                                                                          
258400 HAG-UPPDATERA-LASA21        SECTION.                                     
258500     MOVE ZERO                 TO W-KVAVIS-LO                             
258600                                  W-KVAVIS-BO                             
258700                                  W-KVAVIS-FP                             
258800                                  W-KVAVIS-FB                             
258900     PERFORM HAGA-KOLLA-STYRNING                                          
259000     PERFORM HAGB-SKAPA-LASA21                                            
259100     .                                                                    
259200     EJECT                                                                
259300 HAGA-KOLLA-STYRNING           SECTION.                                   
259400     MOVE ART-KVAVIS           TO W-KVKVAR-ATT-STYRA                      
259500     IF INL-KDINL  =  'R31'                                               
259600        PERFORM HAGAA-STYR-KVALITET-KVAANT                                
259700                                                                          
259800        IF W-KVKVAR-ATT-STYRA  >  ZERO                                    
259900          PERFORM HAGAB-STYR-BEFT                                         
260000        END-IF                                                            
260100     END-IF                                                               
260200                                                                          
260300     IF CDC-SE OR NDC                                                     
260400       PERFORM HAGAC-STYR-PRIO                                            
260500                                                                          
260600       IF W-KVKVAR-ATT-STYRA > ZERO                                       
260700         PERFORM HAGAD-STYR-RESTEN                                        
260800       END-IF                                                             
260900     ELSE                                                                 
261000       PERFORM HAGAE-STYR-TILL-OVR                                        
261100     END-IF                                                               
261200     .                                                                    
261300     EJECT                                                                
261400 HAGAA-STYR-KVALITET-KVAANT  SECTION.                                     
261500     IF ART-KVKVAPRIM-BER      >  ZERO OR                                 
261600        ART-KVKVASEK-BER       >  ZERO                                    
261700       IF ART-KDKVAANT         >  ZERO                                    
261800         MOVE ART-KVAVIS       TO W-KVAVIS-FB                             
261900         MOVE ZERO             TO W-KVKVAR-ATT-STYRA                      
262000       ELSE                                                               
262100         COMPUTE W-KVAVIS-FB = ART-KVKVAPRIM-BER +                        
262200                               ART-KVKVASEK-BER                           
262300         COMPUTE W-KVKVAR-ATT-STYRA =                                     
262400                 W-KVKVAR-ATT-STYRA - W-KVAVIS-FB                         
262500       END-IF                                                             
262600     ELSE                                                                 
262700       IF ART-KDKVAANT         >  ZERO                                    
262800         IF ART-BEFT                    >  ZERO AND                       
262900            STYR-ADINLOMR-FP            NOT = SPACE                       
263000           MOVE ART-KVAVIS              TO W-KVAVIS-FP                    
263100           MOVE ZERO                    TO W-KVKVAR-ATT-STYRA             
263200         ELSE                                                             
263300           MOVE ART-KVAVIS              TO W-KVAVIS-FB                    
263400           MOVE ZERO                    TO W-KVKVAR-ATT-STYRA             
263500         END-IF                                                           
263600       END-IF                                                             
263700     END-IF                                                               
263800     .                                                                    
263900     EJECT                                                                
264000 HAGAB-STYR-BEFT             SECTION.                                     
264100     IF ART-BEFT                 >  ZERO AND                              
264200        STYR-ADINLOMR-FP         NOT = SPACE                              
264300       MOVE W-KVKVAR-ATT-STYRA TO W-KVAVIS-FP                             
264400       MOVE ZERO                 TO W-KVKVAR-ATT-STYRA                    
264500     END-IF                                                               
264600     .                                                                    
264700     EJECT                                                                
264800 HAGAC-STYR-PRIO             SECTION.                                     
264900     IF PRIO-KDINLPRIO                  <  30                             
265000       PERFORM IMS-GU-INLA3-INLA11                                        
265100       COMPUTE W-KVAVIS-PRIO-EGET       =  ART-KVAVIS-PRIO                
265200       IF W-KVKVAR-ATT-STYRA            >= W-KVAVIS-PRIO-EGET             
265300         MOVE W-KVAVIS-PRIO-EGET        TO W-KVAVIS-LO                    
265400         COMPUTE W-KVKVAR-ATT-STYRA =      W-KVKVAR-ATT-STYRA -           
265500                                       W-KVAVIS-PRIO-EGET                 
265600       ELSE                                                               
265700         MOVE W-KVKVAR-ATT-STYRA        TO W-KVAVIS-LO                    
265800         MOVE ZERO                      TO W-KVKVAR-ATT-STYRA             
265900       END-IF                                                             
266000     ELSE                                                                 
266100       MOVE ZERO                        TO W-KVAVIS-PRIO-EGET             
266200     END-IF                                                               
266300     .                                                                    
266400     EJECT                                                                
266500 HAGAD-STYR-RESTEN           SECTION.                                     
266600     IF W-FLEJBUFF = JA                                                   
266700       COMPUTE W-KVAVIS-LO      =  W-KVAVIS-LO +                          
266800                                       W-KVKVAR-ATT-STYRA                 
266900     ELSE                                                                 
267000       IF W-ADINLOMR-BOA = SPACE OR ART-ADTRDEST(1:2) = 'CD'              
267100         MOVE ART-ADLAGOMR            TO W-ADLAGOMR                       
267200         MOVE W-ADLAGOMR              TO W-6006-ADINLOMR                  
267300         PERFORM IMS-GU-PLAA-PLAA11                                       
267400         IF SEGMENT-FINNS AND PLAA-6006-ADINLOMR-BO NOT = SPACE           
267500             MOVE W-KVKVAR-ATT-STYRA     TO W-KVAVIS-BO                   
267600             MOVE PLAA-6006-ADINLOMR-BO  TO W-ADINLOMR-BO                 
267700          ELSE                                                            
267800             COMPUTE W-KVAVIS-LO      =  W-KVAVIS-LO +                    
267900                                         W-KVKVAR-ATT-STYRA               
268000         END-IF                                                           
268100       ELSE                                                               
268200         MOVE W-KVKVAR-ATT-STYRA     TO W-KVAVIS-BO                       
268300         MOVE W-ADINLOMR-BOA         TO W-ADINLOMR-BO                     
268400       END-IF                                                             
268500     END-IF                                                               
268600     .                                                                    
268700     EJECT                                                                
268800 HAGAE-STYR-TILL-OVR         SECTION.                                     
268900     MOVE W-KVKVAR-ATT-STYRA     TO W-KVAVIS-OVR                          
269000     IF W-FLFSP-CL = NEJ                                                  
269100       MOVE 'CDC '               TO W-ADINLOMR-OVR                        
269200     ELSE                                                                 
269300       MOVE 'BORN'               TO W-ADINLOMR-OVR                        
269400     END-IF                                                               
269500     .                                                                    
269600     EJECT                                                                
269700 HAGB-SKAPA-LASA21             SECTION.                                   
269800     IF W-KVAVIS-OVR           >  ZERO                                    
269900       PERFORM S04-INIT-LASA21-SEG                                        
270000       PERFORM HAGBE-SKAPA-LASA21-OVR                                     
270100     END-IF                                                               
270200                                                                          
270300     IF W-KVAVIS-BO            >  ZERO                                    
270400       PERFORM S04-INIT-LASA21-SEG                                        
270500       PERFORM HAGBA-SKAPA-LASA21-BO                                      
270600     END-IF                                                               
270700                                                                          
270800     IF W-KVAVIS-LO  >  ZERO                                              
270900       PERFORM S04-INIT-LASA21-SEG                                        
271000       PERFORM HAGBB-SKAPA-LASA21-LO                                      
271100     END-IF                                                               
271200                                                                          
271300     IF W-KVAVIS-FP >  ZERO                                               
271400       IF W-KVAVIS-FB > +0 AND STYR-ADINLOMR-FB = STYR-ADINLOMR-FP        
271500         CONTINUE                                                         
271600       ELSE                                                               
271700         PERFORM S04-INIT-LASA21-SEG                                      
271800         PERFORM HAGBC-SKAPA-LASA21-FP                                    
271900       END-IF                                                             
272000     END-IF                                                               
272100                                                                          
272200     IF W-KVAVIS-FB >  ZERO                                               
272300       IF W-KVAVIS-FP > +0 AND STYR-ADINLOMR-FB = STYR-ADINLOMR-FP        
272400         ADD W-KVAVIS-FP TO W-KVAVIS-FB                                   
272500       END-IF                                                             
272600       PERFORM S04-INIT-LASA21-SEG                                        
272700       PERFORM HAGBD-SKAPA-LASA21-FB                                      
272800     END-IF                                                               
272900     .                                                                    
273000     EJECT                                                                
273100 HAGBA-SKAPA-LASA21-BO  SECTION.                                          
273200     MOVE +3                          TO LASA-6110-KDSORT1                
273300     MOVE ART-VLARTNTO                TO LASA-6110-VLARTNTO               
273400     IF W-OFOERAEDLAT >  ZERO                                             
273500       IF W-KVAVIS-BO >  W-OFOERAEDLAT                                    
273600         IF W-ADINLOMR-BO =  'HL  '                                       
273700           MOVE W-ADINLOMR-BO         TO LASA-6110-ADINLOMR               
273800           MOVE W-KVAVIS-BO           TO LASA-6110-KVAVIS                 
273900           MOVE W-OFOERAEDLAT         TO LASA-6110-KVAVIS-KIT             
274000           PERFORM S09-ISRT-REPL-LASA-LASA21                              
274100         ELSE                                                             
274200           MOVE W-ADINLOMR-BO         TO LASA-6110-ADINLOMR               
274300           COMPUTE LASA-6110-KVAVIS =  W-KVAVIS-BO -                      
274400                                       W-OFOERAEDLAT                      
274500           PERFORM S09-ISRT-REPL-LASA-LASA21                              
274600           PERFORM S04-INIT-LASA21-SEG                                    
274700                                                                          
274800           MOVE +4                    TO LASA-6110-KDSORT1                
274900           MOVE 'HL  '                TO LASA-6110-ADINLOMR               
275000           MOVE W-OFOERAEDLAT         TO LASA-6110-KVAVIS                 
275100                                       LASA-6110-KVAVIS-KIT               
275200           PERFORM S09-ISRT-REPL-LASA-LASA21                              
275300         END-IF                                                           
275400         MOVE ZERO                    TO W-OFOERAEDLAT                    
275500       ELSE                                                               
275600         MOVE 'HL  '                  TO LASA-6110-ADINLOMR               
275700         MOVE W-KVAVIS-BO             TO LASA-6110-KVAVIS                 
275800                                     LASA-6110-KVAVIS-KIT                 
275900         PERFORM S09-ISRT-REPL-LASA-LASA21                                
276000         COMPUTE W-OFOERAEDLAT        =  W-OFOERAEDLAT -                  
276100                                     W-KVAVIS-BO                          
276200       END-IF                                                             
276300     ELSE                                                                 
276400       MOVE W-ADINLOMR-BO           TO LASA-6110-ADINLOMR                 
276500       MOVE W-KVAVIS-BO             TO LASA-6110-KVAVIS                   
276600       PERFORM S09-ISRT-REPL-LASA-LASA21                                  
276700     END-IF                                                               
276800     .                                                                    
276900     EJECT                                                                
277000 HAGBB-SKAPA-LASA21-LO          SECTION.                                  
277100     MOVE +5                        TO LASA-6110-KDSORT1                  
277200     MOVE ART-VLARTNTO              TO LASA-6110-VLARTNTO                 
277300     MOVE ART-ADLAGOMR              TO W-ADLAGOMR                         
277400     MOVE W-ADLAGOMR                TO LASA-6110-ADINLOMR                 
277500     MOVE W-KVAVIS-LO               TO LASA-6110-KVAVIS                   
277600     MOVE ZERO                      TO LASA-6110-KVAVIS-PRIO              
277700     IF W-KVAVIS-PRIO-EGET          >  ZERO                               
277800       IF W-KVAVIS-LO               >= W-KVAVIS-PRIO-EGET                 
277900           MOVE W-KVAVIS-PRIO-EGET TO LASA-6110-KVAVIS-PRIO               
278000           MOVE ZERO                TO W-KVAVIS-PRIO-EGET                 
278100        ELSE                                                              
278200           MOVE W-KVAVIS-LO         TO LASA-6110-KVAVIS-PRIO              
278300           COMPUTE W-KVAVIS-PRIO-EGET    =  W-KVAVIS-PRIO-EGET -          
278400                                          W-KVAVIS-LO                     
278500       END-IF                                                             
278600     END-IF                                                               
278700     IF W-OFOERAEDLAT                >  ZERO                              
278800       COMPUTE W-KVAVIS-KVAR = W-KVAVIS-LO -                              
278900                           LASA-6110-KVAVIS-PRIO                          
279000       IF W-KVAVIS-KVAR             >= W-OFOERAEDLAT                      
279100          MOVE W-OFOERAEDLAT       TO LASA-6110-KVAVIS-KIT                
279200          MOVE ZERO                TO W-OFOERAEDLAT                       
279300       ELSE                                                               
279400          MOVE W-KVAVIS-KVAR       TO LASA-6110-KVAVIS-KIT                
279500          COMPUTE W-OFOERAEDLAT    =  W-OFOERAEDLAT -                     
279600                                           W-KVAVIS-KVAR                  
279700       END-IF                                                             
279800     END-IF                                                               
279900     PERFORM S09-ISRT-REPL-LASA-LASA21                                    
280000     .                                                                    
280100     EJECT                                                                
280200 HAGBC-SKAPA-LASA21-FP          SECTION.                                  
280300     MOVE +2                           TO LASA-6110-KDSORT1               
280400     MOVE ART-VLARTNTO                 TO LASA-6110-VLARTNTO              
280500     MOVE W-KVAVIS-FP                  TO LASA-6110-KVAVIS                
280600     MOVE STYR-ADINLOMR-FP             TO LASA-6110-ADINLOMR              
280700     MOVE ZERO                         TO LASA-6110-KVAVIS-PRIO           
280800     IF W-KVAVIS-PRIO-EGET             >  ZERO                            
280900       IF W-KVAVIS-FP                  >= W-KVAVIS-PRIO-EGET              
281000         MOVE W-KVAVIS-PRIO-EGET       TO LASA-6110-KVAVIS-PRIO           
281100         MOVE ZERO                     TO W-KVAVIS-PRIO-EGET              
281200       ELSE                                                               
281300         MOVE W-KVAVIS-FP               TO LASA-6110-KVAVIS-PRIO          
281400         COMPUTE W-KVAVIS-PRIO-EGET =      W-KVAVIS-PRIO-EGET -           
281500                                       W-KVAVIS-FP                        
281600       END-IF                                                             
281700     END-IF                                                               
281800     IF W-OFOERAEDLAT                >  ZERO                              
281900       COMPUTE W-KVAVIS-KVAR = W-KVAVIS-FP -                              
282000                           LASA-6110-KVAVIS-PRIO                          
282100       IF W-KVAVIS-KVAR             >= W-OFOERAEDLAT                      
282200          MOVE W-OFOERAEDLAT       TO LASA-6110-KVAVIS-KIT                
282300          MOVE ZERO                TO W-OFOERAEDLAT                       
282400       ELSE                                                               
282500          MOVE W-KVAVIS-KVAR       TO LASA-6110-KVAVIS-KIT                
282600          COMPUTE W-OFOERAEDLAT    =  W-OFOERAEDLAT -                     
282700                                           W-KVAVIS-KVAR                  
282800       END-IF                                                             
282900     END-IF                                                               
283000     PERFORM S09-ISRT-REPL-LASA-LASA21                                    
283100     .                                                                    
283200     EJECT                                                                
283300 HAGBD-SKAPA-LASA21-FB          SECTION.                                  
283400     MOVE +1                           TO LASA-6110-KDSORT1               
283500     MOVE ART-VLARTNTO                 TO LASA-6110-VLARTNTO              
283600     MOVE W-KVAVIS-FB                  TO LASA-6110-KVAVIS                
283700     MOVE STYR-ADINLOMR-FB             TO LASA-6110-ADINLOMR              
283800     MOVE ZERO                         TO LASA-6110-KVAVIS-PRIO           
283900     IF W-KVAVIS-PRIO-EGET             >  ZERO                            
284000       IF W-KVAVIS-FB                  >= W-KVAVIS-PRIO-EGET              
284100         MOVE W-KVAVIS-PRIO-EGET       TO LASA-6110-KVAVIS-PRIO           
284200         MOVE ZERO                     TO W-KVAVIS-PRIO-EGET              
284300       ELSE                                                               
284400         MOVE W-KVAVIS-FB               TO LASA-6110-KVAVIS-PRIO          
284500         COMPUTE W-KVAVIS-PRIO-EGET =      W-KVAVIS-PRIO-EGET -           
284600                                       W-KVAVIS-FB                        
284700       END-IF                                                             
284800     END-IF                                                               
284900     IF W-OFOERAEDLAT                >  ZERO                              
285000       COMPUTE W-KVAVIS-KVAR = W-KVAVIS-FB -                              
285100                           LASA-6110-KVAVIS-PRIO                          
285200       IF W-KVAVIS-KVAR             >= W-OFOERAEDLAT                      
285300          MOVE W-OFOERAEDLAT       TO LASA-6110-KVAVIS-KIT                
285400          MOVE ZERO                TO W-OFOERAEDLAT                       
285500       ELSE                                                               
285600          MOVE W-KVAVIS-KVAR       TO LASA-6110-KVAVIS-KIT                
285700          COMPUTE W-OFOERAEDLAT    =  W-OFOERAEDLAT -                     
285800                                           W-KVAVIS-KVAR                  
285900       END-IF                                                             
286000     END-IF                                                               
286100     PERFORM S09-ISRT-REPL-LASA-LASA21                                    
286200     .                                                                    
286300     EJECT                                                                
286400 HAGBE-SKAPA-LASA21-OVR         SECTION.                                  
286500     MOVE +3                           TO LASA-6110-KDSORT1               
286600     MOVE ART-VLARTNTO                 TO LASA-6110-VLARTNTO              
286700     MOVE W-KVAVIS-OVR                 TO LASA-6110-KVAVIS                
286800     MOVE W-ADINLOMR-OVR               TO LASA-6110-ADINLOMR              
286900     MOVE ZERO                         TO LASA-6110-KVAVIS-PRIO           
287000                                          LASA-6110-KVAVIS-KIT            
287100     PERFORM S09-ISRT-REPL-LASA-LASA21                                    
287200     .                                                                    
287300     EJECT                                                                
287400 HAH-SKAPA-EV-6196-TRANS  SECTION.                                        
287500     IF RAD-IDOKOLLI           =  ZERO                                    
287600       IF CDC-SE AND                                                      
287700         (INL-IDLEVNR = '6492 ' OR                                        
287800          INL-IDLEVNR = 'BZFFA')                                          
287900         CONTINUE                                                         
288000       ELSE                                                               
288100         MOVE ART-ADLAGOMR     TO W-ADLAGOMR                              
288200         MOVE W-ADLAGOMR       TO W-6006-ADINLOMR                         
288300         PERFORM IMS-GU-PLAA-PLAA11-NDC                                   
288400         IF SEGMENT-FINNS AND PLAA-6006-KDLORAPP = 5                      
288500** GODKÄNNER STATUSKOD 'GE' I DENNA LÄSNING                               
288600           CONTINUE                                                       
288700         ELSE                                                             
288800           MOVE ART-IDLOPNRM     TO MOD6196-MID-IDLOPNRM (6196-IX)        
288900           ADD +1                TO 6196-IX                               
289000           MOVE WS-ADINLOMR-PRT  TO MOD6196-MID-ADINLOMR-PRT              
289100           IF 6196-IX                    > MAX-6196-IX                    
289200               PERFORM S05-STARTA-6196-TRANS                              
289300           END-IF                                                         
289400         END-IF                                                           
289500       END-IF                                                             
289600     ELSE                                                                 
289700       IF CDC-SE                                                          
289800         MOVE ART-ADLAGOMR     TO W-ADLAGOMR                              
289900         MOVE W-ADLAGOMR       TO W-6006-ADINLOMR                         
290000         PERFORM IMS-GU-PLAA-PLAA11-BLANK                                 
290100         IF PLAA-6006-KDLORAPP =  2 OR                                    
290200            INL-IDLEVNR        = '6679 ' OR                               
290300            INL-IDLEVNR        = 'T5BVB' OR                               
290400            INL-KDINL          = '310'                                    
290500           IF INL-IDLEVNR  = '6492 '  OR                                  
290600              INL-IDLEVNR  = 'BZFFA'  OR                                  
290700              INL-IDLEVNR  = '3324 '                                      
290800             CONTINUE                                                     
290900           ELSE                                                           
291000             MOVE ART-IDLOPNRM TO MOD6196-MID-IDLOPNRM (6196-IX)          
291100             ADD +1            TO 6196-IX                                 
291200             MOVE WS-ADINLOMR-PRT TO MOD6196-MID-ADINLOMR-PRT             
291300             IF 6196-IX                > MAX-6196-IX                      
291400                 PERFORM S05-STARTA-6196-TRANS                            
291500             END-IF                                                       
291600           END-IF                                                         
291700         END-IF                                                           
291800       ELSE                                                               
291900*NDC     HIT KOMMER TILLS VIDARE ALLA NDC:ER                              
292000*                                                                         
292100         MOVE ART-ADLAGOMR     TO W-ADLAGOMR                              
292200         MOVE W-ADLAGOMR       TO W-6006-ADINLOMR                         
292300         PERFORM IMS-GU-PLAA-PLAA11-NDC                                   
292400** GODKÄNNER STATUSKOD 'GE' I DENNA LÄSNING                               
292500         IF SEGMENT-FINNS AND PLAA-6006-KDLORAPP = 5                      
292600           CONTINUE                                                       
292700         ELSE                                                             
292800           MOVE ART-IDLOPNRM TO MOD6196-MID-IDLOPNRM (6196-IX)            
292900           ADD +1            TO 6196-IX                                   
293000           MOVE WS-ADINLOMR-PRT TO MOD6196-MID-ADINLOMR-PRT               
293100           IF 6196-IX                > MAX-6196-IX                        
293200             PERFORM S05-STARTA-6196-TRANS                                
293300           END-IF                                                         
293400         END-IF                                                           
293500       END-IF                                                             
293600     END-IF                                                               
293700     .                                                                    
293800     EJECT                                                                
293900 HAI-SKAPA-EV-6197-TRANS  SECTION.                                        
294000     IF INL-KDINL            = 'R31' AND                                  
294100        STYR-ADINLOMR-FP NOT = SPACE AND                                  
294200        ART-KVAVIS-PRIO        >  ZERO AND                                
294300        CDC                                                               
294400       MOVE INL-IDLEVNR        TO MOD6197-MID-IDLEVNR (6197-IX)           
294500       MOVE INL-IDFS           TO MOD6197-MID-IDFS    (6197-IX)           
294600       MOVE INL-TIAVIDAT       TO MOD6197-MID-TIAVIDAT(6197-IX)           
294700       MOVE ART-IDRADNR-INL    TO MOD6197-MID-IDRADNR-INL(6197-IX)        
294800       ADD +1                  TO 6197-IX                                 
294900       IF 6197-IX              > MAX-6197-IX                              
295000           PERFORM S06-STARTA-6197-TRANS                                  
295100       END-IF                                                             
295200     END-IF                                                               
295300     .                                                                    
295400     EJECT                                                                
295500 HAJ-KOLLA-CD-PARTI  SECTION.                                             
295600     MOVE ART-IDARTNR TO W61154-IDARTNR                                   
295700     MOVE 0           TO W61154-TIAAVVD                                   
295800     CALL W61154 USING W61154-W61154                                      
295900          BEHOV-WDK6-PCB BEHOV-WDK7-PCB BEHOV-WDR2-PCB                    
296000          BEHOV-WDK6-2-PCB BEHOV-WDK7-2-PCB                               
296100          BEHOV-WDL6-PCB BEHOV-WDD7A-PCB                                  
296200          BEHOV-WDB6-PCB                                                  
296300                                                                          
296400     PERFORM HAJA-KOLLA-CD-AK                                             
296500                                                                          
296600     MOVE 0                          TO W27125-KVANTAL-CD(1)              
296700                                        W27125-KVANTAL-CD(2)              
296800                                        W27125-KVANTAL-CD(3)              
296900                                        W27125-KVANTAL-CD(4)              
297000     MOVE NEJ                        TO FLSLUT                            
297100     MOVE 1 TO IX                                                         
297200     PERFORM UNTIL IX > 4 OR FLSLUT = JA  OR                              
297300                        ARTC11-CLAG-ADLAGOMR-CD(IX) = 0                   
297400       MOVE 0 TO WS-KVANTAL-CD                                            
297500       IF (W61154-KVBEHOV-CD(IX) - WS-AK-CD(IX)) > ART-KVAVIS             
297600         MOVE ART-KVAVIS            TO WS-KVBEHOV-CD                      
297700       ELSE                                                               
297800         COMPUTE WS-KVBEHOV-CD = (W61154-KVBEHOV-CD(IX) -                 
297900                                                    WS-AK-CD(IX))         
298000       END-IF                                                             
298100       PERFORM UNTIL ((WS-KVBEHOV-CD /                                    
298200          ARTC11-CLAG-KVQPACK-3) < 1) OR WS-KVBEHOV-CD = 0                
298300                                                                          
298400         ADD ARTC11-CLAG-KVQPACK-3      TO WS-KVANTAL-CD                  
298500         SUBTRACT ARTC11-CLAG-KVQPACK-3 FROM WS-KVBEHOV-CD                
298600       END-PERFORM                                                        
298700                                                                          
298800       MOVE WS-KVANTAL-CD              TO W27125-KVANTAL-CD(IX)           
298900                                                                          
299000       IF WS-KVANTAL-CD = 0                                               
299100*        KOLLAR OM PARTIET ÄNDÅ SKA STYRAS MOT CD                         
299200*        (FÖR ATT FYLLA UPP EN REDAN SKAPAD REFILLORDER)                  
299300         MOVE W61154-IDARTNR          TO W27125-IDARTNR                   
299400         MOVE W61154-IDDC-GRUPP       TO W27125-IDDC-GRUPP                
299500         MOVE W61154-KVBEHOV-DC-GRUPP TO W27125-KVBEHOV-DC-GRUPP          
299600         MOVE ART-KVAVIS              TO W27125-KVANTAL-CD-KOMPL          
299700         MOVE 0                       TO W27125-KVANTAL-CD-RES            
299800         MOVE IX                      TO W27125-IX                        
299900         MOVE 2                       TO W27125-ANROPSTYP                 
300000         CALL W27125 USING W27125-W27125                                  
300100             BEHOV-WDK6-PCB BEHOV-WDK7-PCB BEHOV-WDE3-PCB                 
300200             BEHOV-WDB6-PCB                                               
300300         MOVE W27125-KVANTAL-CD-RES   TO WS-KVANTAL-CD                    
300400       END-IF                                                             
300500                                                                          
300600       IF WS-KVANTAL-CD = 0                                               
300700         CONTINUE                                                         
300800       ELSE                                                               
300900         MOVE ART-IDRADNR-INL      TO SPAR-IDRADNR-INL                    
301000** SPARAR UNDAN D111-NYCKEL                                               
301100         IF ART-KVAVIS = WS-KVANTAL-CD                                    
301200** ALLT TILL CD                                                           
301300           MOVE 'CD'                        TO ART-ADTRDEST(1:2)          
301400           MOVE IX                          TO ART-ADTRDEST(3:1)          
301500           MOVE ARTC11-CLAG-ADLAGOMR-CD(IX) TO ART-ADLAGOMR               
301600           MOVE ARTC11-CLAG-ADGANG-CD(IX)   TO ART-ADGANG                 
301700           MOVE ARTC11-CLAG-ADPLATS-CD(IX)  TO ART-ADPLATS                
301800           PERFORM IMS-REPL-INLA1-INLA11                                  
301900           MOVE JA TO FLSLUT                                              
302000         ELSE                                                             
302100** SPLITTAT PARTI                                                         
302200           MOVE JA            TO ART-FLSPLPART                            
302300           MOVE ART-W6D111    TO SPAR-ART-W6D111                          
302400           COMPUTE ART-KVAVIS = ART-KVAVIS - WS-KVANTAL-CD                
302500           MOVE WS-KVANTAL-CD TO SPAR-ART-KVAVIS                          
302600           MOVE '   '         TO ART-ADTRDEST                             
302700           MOVE 'CD'          TO SPAR-ART-ADTRDEST(1:2)                   
302800           MOVE IX            TO SPAR-ART-ADTRDEST(3:1)                   
302900           MOVE ARTC11-CLAG-ADLAGOMR-CD(IX) TO SPAR-ART-ADLAGOMR          
303000           MOVE ARTC11-CLAG-ADGANG-CD(IX)   TO SPAR-ART-ADGANG            
303100           MOVE ARTC11-CLAG-ADPLATS-CD(IX)  TO SPAR-ART-ADPLATS           
303200           PERFORM IMS-REPL-INLA1-INLA11                                  
303300                                                                          
303400           MOVE ART-IDRADNR-INL      TO W-IDRADNR-INL                     
303500                                        SPAR-IDRADNR-INL                  
303600           PERFORM IMS-GHNP-INLA1-INLA21                                  
303700                                                                          
303800           IF RAD-IDRADNR > 1                                             
303900*  PARTIET ÄR UPPDELAT PÅ FLERA RADER(W6D121-SEG)                         
304000             MOVE RAD-KVINLART TO HELP-KVINLART                           
304100             PERFORM UNTIL HELP-KVINLART NOT < ART-KVAVIS                 
304200               PERFORM IMS-GHNP-INLA1-INLA21                              
304300               ADD RAD-KVINLART TO HELP-KVINLART                          
304400             END-PERFORM                                                  
304500                                                                          
304600             IF HELP-KVINLART > ART-KVAVIS                                
304700*HANTERING AV TEXT I RESP-TEMFSFEL FINNS I W6011500                       
304800               MOVE SPACE TO RESP-TEMFSFEL                                
304900               MOVE ART-IDARTNR TO WS-IDARTNR-DISP                        
305000               MOVE WS-IDARTNR-DISP TO RESP-TEMFSFEL(1:9)                 
305100               MOVE 'ANTAL I KOLLI MATCHAR EJ Q3   '                      
305200                                      TO RESP-TEMFSFEL(11:30)             
305300                                                                          
305400               PERFORM IMS-ROLLBACK                                       
305500               GOBACK                                                     
305600             END-IF                                                       
305700                                                                          
305800             MOVE RAD-IDRADNR TO HELP-IDRADNR                             
305900                                                                          
306000             MOVE SPAR-ART-W6D111 TO ART-W6D111                           
306100             ADD 1 TO ART-IDRADNR-INL                                     
306200                      W-IDRADNR-INL                                       
306300                                                                          
306400             PERFORM IMS-ISRT-W6D111                                      
306500             PERFORM UNTIL SEGMENT-FINNS                                  
306600               ADD 1 TO ART-IDRADNR-INL                                   
306700                        W-IDRADNR-INL                                     
306800               PERFORM IMS-ISRT-W6D111                                    
306900             END-PERFORM                                                  
307000                                                                          
307100** LÄSER IN DET JUST SKAPADE 11-SEGMENTET I PCB INLA1                     
307200             PERFORM IMS-GHU-INLA1-INLA11                                 
307300                                                                          
307400             MOVE W-IDRADNR-INL    TO W-IDRADNR-INS                       
307500             MOVE SPAR-IDRADNR-INL TO W-IDRADNR-INL                       
307600             MOVE HELP-IDRADNR     TO W-IDRADNR                           
307700             ADD 1                 TO W-IDRADNR                           
307800             MOVE 1                TO HELP-IDRADNR-NY                     
307900** LÄSER IN 21-SEGMENTET EFTER DET DÅ ANTALET FYLLTS UPP                  
308000** I INLA4-PCB                                                            
308100             PERFORM IMS-GHU-INLA4-W6D121                                 
308200             MOVE NEJ TO HELP-FLSLUT                                      
308300             PERFORM UNTIL HELP-FLSLUT = JA                               
308400               ADD 1 TO HELP-IDRADNR-NY                                   
308500** INSERTAR EFTERFÖLJANDE 21-SEGMENT MOT DET NYA 11-SEGMENTET             
308600** MED NYCKEL = 2 FRÅN START.                                             
308700               MOVE OLD-RAD-W6D121  TO RAD-W6D121                         
308800               MOVE HELP-IDRADNR-NY TO RAD-IDRADNR                        
308900               PERFORM IMS-ISRT-W6D121                                    
309000** DELETAR MOTSVARANDE URSPRUNGSSEGMENT                                   
309100               PERFORM IMS-DLET-INLA4-W6D121                              
309200               ADD 1  TO W-IDRADNR                                        
309300               PERFORM IMS-GHU-INLA4-W6D121                               
309400               IF SEGMENT-SAKNAS                                          
309500                 MOVE JA TO HELP-FLSLUT                                   
309600               END-IF                                                     
309700             END-PERFORM                                                  
309800           ELSE                                                           
309900             MOVE RAD-W6D121    TO SPAR-RAD-W6D121                        
310000             MOVE WS-KVANTAL-CD TO SPAR-RAD-KVINLART                      
310100             COMPUTE RAD-KVINLART = RAD-KVINLART - WS-KVANTAL-CD          
310200             PERFORM IMS-REPL-INLA1-INLA21                                
310300                                                                          
310400             MOVE SPAR-ART-W6D111 TO ART-W6D111                           
310500             MOVE SPAR-RAD-W6D121 TO RAD-W6D121                           
310600             ADD 1 TO ART-IDRADNR-INL                                     
310700                      W-IDRADNR-INL                                       
310800                                                                          
310900             PERFORM IMS-ISRT-W6D111                                      
311000             PERFORM UNTIL SEGMENT-FINNS                                  
311100               ADD 1 TO ART-IDRADNR-INL                                   
311200                        W-IDRADNR-INL                                     
311300               PERFORM IMS-ISRT-W6D111                                    
311400             END-PERFORM                                                  
311500             MOVE W-IDRADNR-INL    TO W-IDRADNR-INS                       
311600             PERFORM IMS-ISRT-W6D121                                      
311700           END-IF                                                         
311800         END-IF                                                           
311900** ÅTERSTÄLLER URSPRUNGLIG POSITIONERING                                  
312000         MOVE SPAR-IDRADNR-INL TO W-IDRADNR-INL                           
312100         PERFORM IMS-GU-INLA1-INLA01                                      
312200         PERFORM IMS-GHNP-INLA1-INLA11-KVAL                               
312300       END-IF                                                             
312400       ADD 1 TO IX                                                        
312500     END-PERFORM                                                          
312600     IF W61154-IDDC-GRUPP NOT = SPACE                                     
312700*        SKAPA REFILLORDER                                                
312800       MOVE W61154-IDARTNR          TO W27125-IDARTNR                     
312900       MOVE W61154-IDDC-GRUPP       TO W27125-IDDC-GRUPP                  
313000       MOVE W61154-KVBEHOV-DC-GRUPP TO W27125-KVBEHOV-DC-GRUPP            
313100       MOVE 1                       TO W27125-ANROPSTYP                   
313200       CALL W27125 USING W27125-W27125                                    
313300             BEHOV-WDK6-PCB BEHOV-WDK7-PCB BEHOV-WDE3-PCB                 
313400             BEHOV-WDB6-PCB                                               
313500     END-IF                                                               
313600     .                                                                    
313700     EJECT                                                                
313800 HAJA-KOLLA-CD-AK SECTION.                                                
313900     MOVE ZERO TO WS-AK-CD(1)                                             
314000                  WS-AK-CD(2)                                             
314100                  WS-AK-CD(3)                                             
314200                  WS-AK-CD(4)                                             
314300     MOVE ART-IDARTNR TO WI-IDARTNR                                       
314400     PERFORM IMS-GU-W6D1ISEQ                                              
314500     IF SEGMENT-FINNS                                                     
314600       PERFORM UNTIL SEGMENT-SAKNAS                                       
314700         IF I-ART-ADTRDEST(1:2) = 'CD'                                    
314800           MOVE I-ART-ADTRDEST(3:1) TO IX                                 
314900           ADD I-ART-KVAVIS TO WS-AK-CD(IX)                               
315000         END-IF                                                           
315100         PERFORM IMS-GN-W6D1ISEQ                                          
315200       END-PERFORM                                                        
315300     END-IF                                                               
315400     .                                                                    
315500     EJECT                                                                
315600 HAL-BEHANDLA-6192-TRANS  SECTION.                                        
315700                                                                          
315800     MOVE INL-IDDC             TO MOD6192-MID-IDDC                        
315900     MOVE INL-IDLEVNR          TO MOD6192-MID-IDLEVNR                     
316000     MOVE INL-IDFS             TO MOD6192-MID-IDFS                        
316100     MOVE INL-TIAVIDAT         TO MOD6192-MID-TIAVIDAT                    
316200     MOVE ZERO                 TO MOD6192-MID-IDRADNR-INL                 
316300                                                                          
316400     MOVE SPACE                TO MSG-KOM-WMSGKOM                         
316500     MOVE +53                  TO MSG-KOM-KVLL                            
316600     MOVE LOW-VALUE            TO MSG-KOM-KDZ1                            
316700     MOVE LOW-VALUE            TO MSG-KOM-KDZ2                            
316800     MOVE SPACE                TO MSG-KOM-KDTRANS                         
316900     MOVE 'W6I19201'           TO MSG-KOM-IDCPYTXT                        
317000     MOVE 'INLEV   '           TO MSG-KOM-IDSNDNOD                        
317100     MOVE 'W6011500'           TO MSG-KOM-IDSNDJOB                        
317200     MOVE DAGENS-DATUM         TO MSG-KOM-TIREGDAT                        
317300     MOVE DAGENS-TID           TO MSG-KOM-TIKLOCK                         
317400     MOVE SPACE                TO MSG-KOM-IDMFSMED                        
317500                                                                          
317600     COMPUTE P-TO-P-KVLL       =  LNG-P-TO-P-PREFIX + 27                  
317700     MOVE 'W6T192X '           TO P-TO-P-KDTRANS                          
317800     MOVE '6115'               TO P-TO-P-IDTRANS                          
317900     MOVE WS-KDMFSFOR          TO P-TO-P-KDMFSFOR                         
318000     MOVE MOD6192-MID-W6I19201 TO P-TO-P-DATA                             
318100                                                                          
318200     CALL W006KOM USING MSG-PCB                                           
318300                        DISP-PCB                                          
318400                        KOM-KOMA-PCB                                      
318500                        MSG-KOM-WMSGKOM                                   
318600                        P-TO-P-SW                                         
318700     .                                                                    
318800     EJECT                                                                
318900 HAM-KOLLA-LOSSLISTA-ILISTA   SECTION.                                    
319000                                                                          
319100     MOVE JA TO LOSSLISTA-SW                                              
319200     MOVE NEJ TO ILISTA-SW                                                
319300     IF CDC-SE                                                            
319400       MOVE ART-ADLAGOMR TO W-ADLAGOMR                                    
319500       MOVE W-ADLAGOMR TO W-6006-ADINLOMR                                 
319600       PERFORM IMS-GU-PLAA-PLAA11-BLANK                                   
319700       IF PLAA-6006-FLLOLL = NEJ                                          
319800         MOVE NEJ TO LOSSLISTA-SW                                         
319900       END-IF                                                             
320000       IF PLAA-6006-KDLORAPP = 3                                          
320100         MOVE JA TO ILISTA-SW                                             
320200         MOVE '6M'        TO W-IDPRTLST(1:2)                              
320300         MOVE PLAA-6006-ADINLOMR-PRT TO W-IDPRTLST(3:6)                   
320400       END-IF                                                             
320500     END-IF                                                               
320600     .                                                                    
320700     EJECT                                                                
320800 HAN-BEHANDLA-ILISTA   SECTION.                                           
320900                                                                          
321000     PERFORM HANB-SORT-I-LIST                                             
321100     PERFORM HANB-REGISTRERA-TORG                                         
321200     PERFORM HANC-REG-LISTNR-RADNR-DAT-REPL                               
321300     MOVE ZEROS TO I-TAB-IX                                               
321400                   I-TAB-IX-MAX                                           
321500     .                                                                    
321600     EJECT                                                                
321700 HANB-SORT-I-LIST SECTION.                                                
321800                                                                          
321900     MOVE +65            TO  TABENTRY-LNGD                                
322000     MOVE I-TAB-IX-MAX   TO  ANTAL-ENTRY                                  
322100     MOVE +28            TO  SORTBGP-LNGD                                 
322200                                                                          
322300     CALL WINTSOR        USING INL-TAB TABENTRY-LNGD ANTAL-ENTRY          
322400                               I-TAB-SORTNYCKEL(1) SORTBGP-LNGD           
322500     .                                                                    
322600     EJECT                                                                
322700 HANB-REGISTRERA-TORG SECTION.                                            
322800                                                                          
322900     MOVE +1 TO I-TAB-IX                                                  
323000     MOVE I-TAB-ADLAGOMR(I-TAB-IX)   TO W-SPAR-ADLAGOMR                   
323100                                        W-JFR-ADLAGOMR                    
323200     MOVE SPACE                      TO W-6006-ADINLOMR-PAR               
323300     MOVE W-SPAR-ADLAGOMR            TO W-6006-ADINLOMR-PAR               
323400     PERFORM IMS-GU-PLAA11-PAR                                            
323500     PERFORM UNTIL I-TAB-IX > I-TAB-IX-MAX                                
323600       MOVE NEJ TO TAB-TRAEFF-SW                                          
323700       PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT OR TAB-TRAEFF         
323800         IF PLAA-6006-KDINLOMR     = 'TRG' AND                            
323900            PLAA-6006-ADPLATS-FOM <= I-TAB-ADPLATS(I-TAB-IX) AND          
324000            PLAA-6006-ADPLATS-TOM >= I-TAB-ADPLATS(I-TAB-IX)              
324100           MOVE PLAA-6006-ADINLOMR TO I-TAB-TORG(I-TAB-IX)                
324200           MOVE JA TO TAB-TRAEFF-SW                                       
324300         ELSE                                                             
324400           PERFORM IMS-GN-PLAA11-PAR                                      
324500         END-IF                                                           
324600       END-PERFORM                                                        
324700       IF TAB-TRAEFF                                                      
324800         ADD 1 TO I-TAB-IX                                                
324900         PERFORM UNTIL (I-TAB-IX > I-TAB-IX-MAX)  OR                      
325000                       (I-TAB-ADLAGOMR(I-TAB-IX)  NOT =                   
325100                            W-JFR-ADLAGOMR)       OR                      
325200                       (I-TAB-ADPLATS(I-TAB-IX)   >                       
325300                        PLAA-6006-ADPLATS-TOM)                            
325400           MOVE PLAA-6006-ADINLOMR TO I-TAB-TORG(I-TAB-IX)                
325500           ADD 1 TO I-TAB-IX                                              
325600         END-PERFORM                                                      
325700       ELSE                                                               
325800         ADD 1 TO I-TAB-IX                                                
325900       END-IF                                                             
326000       IF I-TAB-ADLAGOMR(I-TAB-IX) = W-JFR-ADLAGOMR                       
326100         PERFORM IMS-GN-PLAA11-PAR                                        
326200       ELSE                                                               
326300         MOVE I-TAB-ADLAGOMR(I-TAB-IX) TO W-SPAR-ADLAGOMR                 
326400                                          W-JFR-ADLAGOMR                  
326500         MOVE SPACE                    TO W-6006-ADINLOMR-PAR             
326600         MOVE W-SPAR-ADLAGOMR          TO W-6006-ADINLOMR-PAR             
326700         PERFORM IMS-GN-PLAA11-PAR                                        
326800       END-IF                                                             
326900     END-PERFORM                                                          
327000     .                                                                    
327100     EJECT                                                                
327200 HANC-REG-LISTNR-RADNR-DAT-REPL SECTION.                                  
327300                                                                          
327400     MOVE +0 TO I-TAB-IX                                                  
327500                W-KVRADER                                                 
327600     PERFORM UNTIL I-TAB-IX = I-TAB-IX-MAX                                
327700       ADD   1 TO I-TAB-IX                                                
327800       MOVE I-TAB-ADLAGOMR(I-TAB-IX)  TO W-SPAR-ADLAGOMR                  
327900                                         W-JFR-ADLAGOMR                   
328000       MOVE I-TAB-TORG(I-TAB-IX)      TO W-SPAR-TORG                      
328100       MOVE LOPA-6018-IDILIST         TO  W-D-MIN-IDILIST                 
328200                                          W-D-MAX-IDILIST                 
328300       MOVE WS-IDDC                   TO  W-D-MIN-IDDC                    
328400                                          W-D-MAX-IDDC                    
328500                                                                          
328600       PERFORM IMS-GU-INLA11-D                                            
328700***NUMMERSERIEN KAN SLÅ OM                                                
328800       PERFORM UNTIL SEGMENT-SAKNAS                                       
328900         ADD 1 TO W-D-MIN-IDILIST                                         
329000                  W-D-MAX-IDILIST                                         
329100         PERFORM IMS-GU-INLA11-D                                          
329200       END-PERFORM                                                        
329300       MOVE W-D-MIN-IDILIST TO W-IDILIST                                  
329400                               LOPA-6018-IDILIST                          
329500       ADD  1               TO LOPA-6018-IDILIST                          
329600                                                                          
329700       PERFORM HANCA-FYLL-LISTA                                           
329800                                                                          
329900     END-PERFORM                                                          
330000     .                                                                    
330100     EJECT                                                                
330200 HANCA-FYLL-LISTA SECTION.                                                
330300                                                                          
330400     PERFORM UNTIL I-TAB-ADLAGOMR(I-TAB-IX) NOT = W-JFR-ADLAGOMR          
330500             OR    I-TAB-TORG(I-TAB-IX)     NOT = W-SPAR-TORG             
330600       MOVE W-IDILIST TO I-TAB-IDILIST(I-TAB-IX)                          
330700       ADD 1 TO W-KVRADER                                                 
330800       MOVE W-KVRADER    TO I-TAB-IDILIRAD(I-TAB-IX)                      
330900                                                                          
331000       PERFORM HANCAA-REPL-MOT-W6D121                                     
331100                                                                          
331200       ADD 1 TO I-TAB-IX                                                  
331300     END-PERFORM                                                          
331400     SUBTRACT 1 FROM I-TAB-IX                                             
331500                                                                          
331600     PERFORM HANCAB-SKAPA-6199-MID                                        
331700                                                                          
331800     MOVE  0  TO W-KVRADER                                                
331900     .                                                                    
332000     EJECT                                                                
332100 HANCAA-REPL-MOT-W6D121 SECTION.                                          
332200                                                                          
332300     MOVE I-TAB-IDLEVNR(I-TAB-IX)  TO W-D101KY-IDLEVNR                    
332400     MOVE I-TAB-IDFS(I-TAB-IX)     TO W-D101KY-IDFS                       
332500     MOVE I-TAB-TIAVIDAT(I-TAB-IX) TO W-D101KY-TIAVIDAT                   
332600     MOVE I-TAB-IDARTNR(I-TAB-IX)  TO W-IDARTNR                           
332700     MOVE I-TAB-IDRADNR-INL(I-TAB-IX)  TO W-IDRADNR-INL                   
332800     MOVE I-TAB-IDRADNR(I-TAB-IX)  TO W-IDRADNR                           
332900     PERFORM IMS-GHU-INLA1-INLA21                                         
333000     IF SEGMENT-FINNS                                                     
333100       MOVE I-TAB-IDILIRAD(I-TAB-IX) TO RAD-IDILIRAD                      
333200       MOVE I-TAB-IDILIST(I-TAB-IX)  TO RAD-IDILIST                       
333300       MOVE TIDZ-MSGI-TILOKDAT       TO RAD-TIUPPDAT                      
333400       PERFORM IMS-REPL-INLA1-INLA21                                      
333500     ELSE                                                                 
333600       MOVE 'FEL I SECTION HANCAA- ' TO ERRORTEXT                         
333700       CALL ABEND USING RKOD-ABEND                                        
333800     END-IF                                                               
333900     .                                                                    
334000     EJECT                                                                
334100 HANCAB-SKAPA-6199-MID SECTION.                                           
334200                                                                          
334300     MOVE I-TAB-IDPRTLST(I-TAB-IX) TO  MOD6199-MID-IDPRTLST               
334400     MOVE IDPGM                    TO  MOD6199-MID-IDPGM                  
334500                                                                          
334600     ADD   +1               TO  6199-IX                                   
334700                                                                          
334800     MOVE I-TAB-IDLEVNR-KOLLI(I-TAB-IX)                                   
334900                            TO MOD6199-MID-IDLEVNR-KOLLI(6199-IX)         
335000     MOVE I-TAB-IDOKOLLI(I-TAB-IX)                                        
335100                            TO MOD6199-MID-IDOKOLLI(6199-IX)              
335200     MOVE ZERO                                                            
335300                            TO MOD6199-MID-IDINLVGN(6199-IX)              
335400     MOVE SPACE                                                           
335500                            TO MOD6199-MID-ADINLOMR(6199-IX)              
335600                                                                          
335700     IF W-SPAR-TORG = '0000'                                              
335800       MOVE SPACE                                                         
335900                            TO MOD6199-MID-ADINLOMR-TORG(6199-IX)         
336000     ELSE                                                                 
336100       MOVE W-SPAR-TORG                                                   
336200                            TO MOD6199-MID-ADINLOMR-TORG(6199-IX)         
336300     END-IF                                                               
336400                                                                          
336500     MOVE W-SPAR-ADLAGOMR                                                 
336600                            TO MOD6199-MID-ADLAGOMR(6199-IX)              
336700     MOVE I-TAB-IDILIST(I-TAB-IX)                                         
336800                            TO MOD6199-MID-IDILIST(6199-IX)               
336900     MOVE I-TAB-IDILIRAD(I-TAB-IX)                                        
337000                            TO MOD6199-MID-KVRADER(6199-IX)               
337100                                                                          
337200     ADD 1 TO W-KVPOST-I-LISTA                                            
337300                                                                          
337400     IF (6199-IX = 6199-MAX-IX) OR                                        
337500     (I-TAB-IDPRTLST(I-TAB-IX) NOT = I-TAB-IDPRTLST(I-TAB-IX + 1))        
337600       PERFORM S07-STARTA-6199-TRANS                                      
337700     END-IF                                                               
337800     .                                                                    
337900     EJECT                                                                
338000 HB-UPPDATERA-AKTIVERING-BIL     SECTION.                                 
338100                                                                          
338200     PERFORM IMS-GU-LASA-LASA21                                           
338300     IF SEGMENT-FINNS                                                     
338400       PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                       
338500         COMPUTE W-BIL-VLARTNTO = W-BIL-VLARTNTO +                        
338600           (LASA-6110-KVAVIS     * LASA-6110-VLARTNTO)                    
338700             PERFORM HBA-UPPDATERA-ADINLOMR-TAB                           
338800         PERFORM IMS-GNP-LASA-LASA21                                      
338900       END-PERFORM                                                        
339000                                                                          
339100       IF REQU-ADINLOMR-LPL-UPD   = ALL '+'                               
339200         PERFORM HBB-RAEKNA-FRAM-LPL                                      
339300         MOVE +12                TO TABENTRY-LNGD                         
339400         MOVE +2                 TO SORTBGP-LNGD                          
339500         SET ANTAL-ENTRY         TO LPL-IX                                
339600         CALL WINTSOR USING LPL-TABELL    TABENTRY-LNGD                   
339700                                        ANTAL-ENTRY                       
339800                                        W-SUPROC-LPL(1)                   
339900                                        SORTBGP-LNGD                      
340000       ELSE                                                               
340100         MOVE REQU-ADINLOMR-LPL-UPD TO W-SPAR-LPL                         
340200       END-IF                                                             
340300                                                                          
340400       PERFORM HBC-UPPDATERA-LASA11                                       
340500     ELSE                                                                 
340600       MOVE NEJ TO LOSSLISTA-SW                                           
340700     END-IF                                                               
340800     .                                                                    
340900     EJECT                                                                
341000 HBA-UPPDATERA-ADINLOMR-TAB      SECTION.                                 
341100                                                                          
341200     SET ADINLOMR-IX           TO 1                                       
341300     SEARCH W-ADINLOMR-TAB                                                
341400       WHEN W-ADINLOMR (ADINLOMR-IX)  =  SPACE                            
341500         MOVE LASA-6110-ADINLOMR      TO W-ADINLOMR (ADINLOMR-IX)         
341600         COMPUTE W-VLARTNTO-ADINLOMR (ADINLOMR-IX)  =                     
341700                 LASA-6110-KVAVIS * LASA-6110-VLARTNTO                    
341800       WHEN W-ADINLOMR (ADINLOMR-IX)  =  LASA-6110-ADINLOMR               
341900         COMPUTE W-VLARTNTO-ADINLOMR (ADINLOMR-IX)  =                     
342000                 W-VLARTNTO-ADINLOMR (ADINLOMR-IX) +                      
342100                (LASA-6110-KVAVIS * LASA-6110-VLARTNTO)                   
342200         CONTINUE                                                         
342300     END-SEARCH                                                           
342400                                                                          
342500     COMPUTE W-TOT-VLARTNTO ROUNDED = W-TOT-VLARTNTO  +                   
342600           (LASA-6110-KVAVIS * LASA-6110-VLARTNTO)                        
342700     .                                                                    
342800     EJECT                                                                
342900 HBB-RAEKNA-FRAM-LPL             SECTION.                                 
343000                                                                          
343100     SET ADINLOMR-IX                       TO 1                           
343200     PERFORM UNTIL ADINLOMR-IX             >  MAX-ADINLOMR-IX OR          
343300                   W-ADINLOMR(ADINLOMR-IX) = SPACE                        
343400       MOVE W-ADINLOMR (ADINLOMR-IX)       TO W-6006-ADINLOMR             
343500       PERFORM IMS-GU-PLAA-PLAA11-BLANK                                   
343600       IF SEGMENT-FINNS                                                   
343700           PERFORM HBBA-UPPDATERA-LPL-TAB                                 
343800       END-IF                                                             
343900       SET ADINLOMR-IX UP BY 1                                            
344000     END-PERFORM                                                          
344100                                                                          
344200     MOVE ZERO                          TO W-MAX-PROCENT-LPL              
344300     SET LPL-IX                         TO 1                              
344400     PERFORM UNTIL LPL-IX               >  MAX-LPL-IX OR                  
344500                   W-LPL (LPL-IX)       =  SPACE                          
344600       COMPUTE W-SUPROC-LPL (LPL-IX) ROUNDED    =                         
344700               W-VLARTNTO-LPL(LPL-IX) /    W-TOT-VLARTNTO                 
344800       ON SIZE ERROR MOVE ZERO         TO W-SUPROC-LPL (LPL-IX)           
344900       END-COMPUTE                                                        
345000       IF W-SUPROC-LPL (LPL-IX)         >  W-MAX-PROCENT-LPL OR           
345100          LPL-IX                        =  1                              
345200         MOVE W-SUPROC-LPL(LPL-IX)      TO W-MAX-PROCENT-LPL              
345300         MOVE W-LPL (LPL-IX)            TO W-SPAR-LPL                     
345400       END-IF                                                             
345500       SET LPL-IX UP BY 1                                                 
345600     END-PERFORM                                                          
345700     SET LPL-IX DOWN BY 1                                                 
345800     .                                                                    
345900     EJECT                                                                
346000 HBBA-UPPDATERA-LPL-TAB      SECTION.                                     
346100                                                                          
346200     SET LPL-IX             TO 1                                          
346300     SEARCH W-LPL-TAB                                                     
346400       WHEN W-LPL (LPL-IX)      =  SPACE                                  
346500         MOVE PLAA-6006-ADINLOMR-LPL   TO W-LPL (LPL-IX)                  
346600         MOVE W-VLARTNTO-ADINLOMR (ADINLOMR-IX) TO                        
346700              W-VLARTNTO-LPL (LPL-IX)                                     
346800       WHEN W-LPL (LPL-IX)      =  PLAA-6006-ADINLOMR-LPL                 
346900         COMPUTE W-VLARTNTO-LPL (LPL-IX) =                                
347000                 W-VLARTNTO-LPL (LPL-IX) +                                
347100                 W-VLARTNTO-ADINLOMR (ADINLOMR-IX)                        
347200         CONTINUE                                                         
347300     END-SEARCH                                                           
347400     .                                                                    
347500     EJECT                                                                
347600 HBC-UPPDATERA-LASA11       SECTION.                                      
347700                                                                          
347800     PERFORM IMS-GHU-LASA-LASA11                                          
347900     MOVE W-SPAR-LPL           TO LASA-6108-ADINLOMR-LPL                  
348000     COMPUTE LASA-6108-VLLBNTO ROUNDED = W-BIL-VLARTNTO / 1000000         
348100                                                                          
348200     PERFORM IMS-REPL-LASA-LASA11                                         
348300     .                                                                    
348400     EJECT                                                                
348500*WL01TIDZ                                                                 
348600 I-CALL-WL01TIDZ       SECTION.                                           
348700                                                                          
348800*NDC  SKA ANVÄNDA SIG AV SITT LOKALA DATUM VID KONTROLL AV                
348900*     TIAVIDAT OCH ATT AKTUELLT PRIS-FINNS                                
349000                                                                          
349100     MOVE '011'                    TO TIDZ-MSGI-KDCALL                    
349200     MOVE REQU-IDTIDZON            TO TIDZ-MSGI-IDTIDZON                  
349300     MOVE REQU-IDDC-KEY            TO TIDZ-MSGI-IDDC                      
349400     MOVE DAGENS-DATUM             TO TIDZ-MSGI-TILOKDAT                  
349500     MOVE DAGENS-TID               TO TIDZ-MSGI-TILOKTID                  
349600     CALL WL01TIDZ USING              TIDZ-MSGI-WL01TIDZ                  
349700     .                                                                    
349800     EJECT                                                                
349900 S02-FYLL-I-6191MID           SECTION.                                    
350000                                                                          
350100     MOVE 'W6011500'           TO MOD6191-MID-IDPGM                       
350200     MOVE WS-IDDC              TO MOD6191-MID-IDDC                        
350300     MOVE W-IDLOPNRM           TO MOD6191-MID-IDLOPNRM (6191-IX)          
350400     MOVE RAD-IDRADNR          TO MOD6191-MID-IDRADNR  (6191-IX)          
350500     MOVE ART-PRARTSTD         TO MOD6191-MID-PRARTSTD (6191-IX)          
350600     MOVE PRIO-KDINLPRIO       TO MOD6191-MID-KDINLPRIO(6191-IX)          
350700     MOVE +0                   TO MOD6191-MID-KVKOLLI  (6191-IX)          
350800     MOVE 'N'                  TO MOD6191-MID-FLINLI   (6191-IX)          
350900                                                                          
351000     MOVE SPACE                TO MOD6191-MID-ADINLOMR-OLD                
351100                                                   (6191-IX)              
351200     MOVE SPACE                TO MOD6191-MID-ADINLOMR-NXT-OLD            
351300                                                   (6191-IX)              
351400     MOVE RAD-KDINLSTA         TO MOD6191-MID-KDINLSTA-OLD                
351500                                                   (6191-IX)              
351600     MOVE RAD-KVINLART         TO MOD6191-MID-KVINLART-OLD                
351700                                                   (6191-IX)              
351800                                                                          
351900     MOVE SPACE                TO MOD6191-MID-ADINLOMR-NEW                
352000                                                   (6191-IX)              
352100     MOVE SPACE                TO MOD6191-MID-ADINLOMR-NXT-NEW            
352200                                                   (6191-IX)              
352300     MOVE SPACE                TO MOD6191-MID-KDINLSTA-NEW                
352400                                                   (6191-IX)              
352500     MOVE RAD-KVINLART         TO MOD6191-MID-KVINLART-NEW                
352600                                                   (6191-IX)              
352700     ADD +1                    TO 6191-IX                                 
352800                                                                          
352900     IF 6191-IX                >  MAX-6191-IX                             
353000       PERFORM S03-STARTA-6191-TRANS                                      
353100     END-IF                                                               
353200     .                                                                    
353300     EJECT                                                                
353400 S03-STARTA-6191-TRANS         SECTION.                                   
353500                                                                          
353600     COMPUTE MOD6191-MID-KVPOST = 6191-IX - 1                             
353700     COMPUTE P-TO-P-KVLL        =  LNG-P-TO-P-PREFIX +                    
353800                                   17 + (MOD6191-MID-KVPOST * 64)         
353900     MOVE 'W6T191X '            TO P-TO-P-KDTRANS                         
354000     MOVE '6115'                TO P-TO-P-IDTRANS                         
354100     MOVE WS-KDMFSFOR           TO P-TO-P-KDMFSFOR                        
354200                                                                          
354300     MOVE MOD6191-MID-W6I19101  TO P-TO-P-DATA                            
354400                                                                          
354500     IF FOERSTA-6191                                                      
354600       PERFORM IMS-ISRT-ALT-MSG-6191                                      
354700       MOVE NEJ                TO FOERSTA-6191-SW                         
354800     ELSE                                                                 
354900       PERFORM IMS-PURG-ALT-MSG-6191                                      
355000     END-IF                                                               
355100     MOVE +1                   TO 6191-IX                                 
355200     .                                                                    
355300     EJECT                                                                
355400 S04-INIT-LASA21-SEG       SECTION.                                       
355500                                                                          
355600     MOVE SPACE                TO LASA-6110-W6GX6110                      
355700     MOVE INL-IDLEVNR          TO LASA-6110-IDLEVNR                       
355800     MOVE INL-IDFS             TO LASA-6110-IDFS                          
355900     MOVE INL-TIAVIDAT         TO LASA-6110-TIAVIDAT                      
356000     MOVE ART-IDARTNR          TO LASA-6110-IDARTNR                       
356100     MOVE NEJ                  TO LASA-6110-FLKLAR                        
356200     MOVE ZERO                 TO LASA-6110-KVAVIS                        
356300                                  LASA-6110-KVAVIS-PRIO                   
356400                                  LASA-6110-KVAVIS-KIT                    
356500                                  LASA-6110-KDSORT1                       
356600                                  LASA-6110-VLARTNTO                      
356700     .                                                                    
356800     EJECT                                                                
356900 S05-STARTA-6196-TRANS  SECTION.                                          
357000                                                                          
357100     MOVE WS-IDDC              TO MOD6196-MID-IDDC                        
357200                                                                          
357300     COMPUTE MOD6196-MID-KVPOST = 6196-IX - 1                             
357400     COMPUTE P-TO-P-KVLL        = LNG-P-TO-P-PREFIX +                     
357500                                  13 + (MOD6196-MID-KVPOST * 9)           
357600     MOVE 'W6T196X '           TO P-TO-P-KDTRANS                          
357700     MOVE '6115'               TO P-TO-P-IDTRANS                          
357800     MOVE WS-KDMFSFOR          TO P-TO-P-KDMFSFOR                         
357900     MOVE MOD6196-MID-W6I19602 TO P-TO-P-DATA                             
358000     IF FOERSTA-6196                                                      
358100       PERFORM IMS-ISRT-ALT-MSG-6196                                      
358200       MOVE NEJ                TO FOERSTA-6196-SW                         
358300     ELSE                                                                 
358400       PERFORM IMS-PURG-ALT-MSG-6196                                      
358500     END-IF                                                               
358600     ADD 6196-IX               TO W-KVUTSKR-AR                            
358700     SUBTRACT 1              FROM W-KVUTSKR-AR                            
358800     MOVE +1                   TO 6196-IX                                 
358900     .                                                                    
359000     EJECT                                                                
359100 S06-STARTA-6197-TRANS  SECTION.                                          
359200                                                                          
359300     MOVE WS-IDDC               TO MOD6197-MID-IDDC                       
359400                                                                          
359500     MOVE SPACE                 TO MOD6197-MID-IDPRTLST                   
359600     MOVE '6L'                  TO MOD6197-MID-IDPRTLST(1:2)              
359700     MOVE 'TR  '                TO MOD6197-MID-IDPRTLST(3:4)              
359800     MOVE 'W6011500'            TO MOD6197-MID-IDPGM                      
359900     MOVE 'N'                   TO MOD6197-MID-FLSVS                      
360000     COMPUTE MOD6197-MID-KVPOST = 6197-IX - 1                             
360100     COMPUTE P-TO-P-KVLL        = LNG-P-TO-P-PREFIX +                     
360200                                  26 + (MOD6197-MID-KVPOST * 24)          
360300     MOVE 'W6T197X '           TO P-TO-P-KDTRANS                          
360400     MOVE '6115'               TO P-TO-P-IDTRANS                          
360500     MOVE WS-KDMFSFOR          TO P-TO-P-KDMFSFOR                         
360600     MOVE MOD6197-MID-W6I19701 TO P-TO-P-DATA                             
360700     IF FOERSTA-6197                                                      
360800       PERFORM IMS-ISRT-ALT-MSG-6197                                      
360900       MOVE NEJ                TO FOERSTA-6197-SW                         
361000     ELSE                                                                 
361100       PERFORM IMS-PURG-ALT-MSG-6197                                      
361200     END-IF                                                               
361300     MOVE +1                   TO 6197-IX                                 
361400     .                                                                    
361500     EJECT                                                                
361600 S07-STARTA-6199-TRANS SECTION.                                           
361700                                                                          
361800     MOVE W-KVPOST-I-LISTA     TO  MOD6199-MID-KVPOST                     
361900     COMPUTE P-TO-P-KVLL   = LNG-P-TO-P-PREFIX                            
362000                             + 23 + (MOD6199-MID-KVPOST * 35)             
362100                                                                          
362200     MOVE LOW-VALUE            TO P-TO-P-KDZ1                             
362300     MOVE LOW-VALUE            TO P-TO-P-KDZ2                             
362400     MOVE 'W6T199X '           TO P-TO-P-KDTRANS                          
362500     MOVE '6115'               TO P-TO-P-IDTRANS                          
362600     MOVE WS-KDMFSFOR          TO P-TO-P-KDMFSFOR                         
362700                                                                          
362800     MOVE MOD6199-MID-W6I19901 TO P-TO-P-DATA                             
362900     IF  FOERSTA-6199                                                     
363000       PERFORM IMS-ISRT-ALT-MSG-6199                                      
363100       MOVE NEJ                TO FOERSTA-6199-SW                         
363200     ELSE                                                                 
363300       PERFORM IMS-PURG-ALT-MSG-6199                                      
363400     END-IF                                                               
363500                                                                          
363600     MOVE +0                   TO 6199-IX                                 
363700                                  W-KVPOST-I-LISTA                        
363800     .                                                                    
363900     EJECT                                                                
364000 S08-KOLLA-STYRNING    SECTION.                                           
364100                                                                          
364200     IF INL-KDINL          =  'R31'                                       
364300       MOVE ART-IDARTNR    TO STYR-IDARTNR                                
364400       MOVE ART-IDFKNGRP   TO STYR-IDFKNGRP                               
364500       MOVE INL-IDLEVNR    TO STYR-IDLEVNR                                
364600       MOVE ART-BEFT       TO STYR-BEFT                                   
364700       CALL W611STYR USING STYR-W611STYR STYR-HANB-PCB                    
364800                           STYR-PLAA-PCB                                  
364900     END-IF                                                               
365000     .                                                                    
365100     EJECT                                                                
365200 S09-ISRT-REPL-LASA-LASA21 SECTION.                                       
365300                                                                          
365400     PERFORM IMS-ISRT-LASA-LASA21                                         
365500     IF SEGMENT-FINNS-REDAN                                               
365600       IF ART-ADTRDEST(1:2) = 'CD'                                        
365700         PERFORM UNTIL SEGMENT-FINNS                                      
365800           ADD 1  TO LASA-6110-KDSORT1                                    
365900           PERFORM IMS-ISRT-LASA-LASA21                                   
366000         END-PERFORM                                                      
366100       ELSE                                                               
366200                                                                          
366300         MOVE LASA-6110-IDLEVNR     TO W-6110-IDLEVNR                     
366400         MOVE LASA-6110-IDFS        TO W-6110-IDFS                        
366500         MOVE LASA-6110-TIAVIDAT    TO W-6110-TIAVIDAT                    
366600         MOVE LASA-6110-IDARTNR     TO W-6110-IDARTNR                     
366700         MOVE LASA-6110-KDSORT1     TO W-6110-KDSORT1                     
366800                                                                          
366900         MOVE LASA-6110-KVAVIS TO SPAR-6110-KVAVIS                        
367000         MOVE LASA-6110-KVAVIS-PRIO TO SPAR-6110-KVAVIS-PRIO              
367100         MOVE LASA-6110-KVAVIS-KIT  TO SPAR-6110-KVAVIS-KIT               
367200                                                                          
367300         PERFORM IMS-GHU-LASA-LASA21                                      
367400                                                                          
367500         ADD SPAR-6110-KVAVIS TO LASA-6110-KVAVIS                         
367600         ADD SPAR-6110-KVAVIS-PRIO TO LASA-6110-KVAVIS-PRIO               
367700         ADD SPAR-6110-KVAVIS-KIT  TO LASA-6110-KVAVIS-KIT                
367800                                                                          
367900         PERFORM IMS-REPL-LASA-LASA21                                     
368000       END-IF                                                             
368100     END-IF                                                               
368200     .                                                                    
368300     EJECT                                                                
368400                                                                          
368500* --- MFS SEKTIONER ---                                                   
368600                                                                          
368700 MFS-ERASE-FIELD-OUT SECTION.                                             
368800     MOVE +1                   TO INDX                                    
368900                                                                          
369000     PERFORM UNTIL INDX        >  MAX-KVRADER                             
369100       MOVE SPACE              TO RESP-KDCMDVAL-LINE (INDX)               
369200                                  RESP-IDLEVNR-LINE (INDX)                
369300                                  RESP-IDFS-LINE   (INDX)                 
369400                                  RESP-TIAVIDAT-LINE (INDX)               
369500                                  RESP-IDLBBET-LINE (INDX)                
369600                                  RESP-KVPARTI-LINE (INDX)                
369700                                  RESP-FLFEL-LINE  (INDX)                 
369800                                  RESP-ADINLOMR-FB-LINE (INDX)            
369900                                  RESP-IDSHIPM-LINE (INDX)                
370000       ADD +1                  TO INDX                                    
370100     END-PERFORM                                                          
370200     .                                                                    
370300     SKIP2                                                                
370400                                                                          
370500 MFS-FORM-DEFAULT-ATTR     SECTION.                                       
370600                                                                          
370700     MOVE MFS-FORMAT-DEFAULT-ATTR TO RESP-IDLBBET-UPD-ATTR                
370800                                     RESP-FLKLAR-UPD-ATTR                 
370900                                     RESP-ADINLOMR-LPL-UPD-ATTR           
371000     MOVE +1                      TO INDX                                 
371100     PERFORM UNTIL INDX        >  MAX-KVRADER                             
371200       PERFORM MFS-FORM-ATTR-LINE                                         
371300       ADD +1                  TO INDX                                    
371400     END-PERFORM                                                          
371500     .                                                                    
371600     SKIP2                                                                
371700 MFS-FORM-ATTR-LINE        SECTION.                                       
371800                                                                          
371900     MOVE MFS-FORMAT-DEFAULT-ATTR TO RESP-KDCMDVAL-LINE-ATTR(INDX)        
372000     .                                                                    
372100     SKIP3                                                                
372200                                                                          
372300 MFS-ERASE-FIELD-IN SECTION.                                              
372400                                                                          
372500     MOVE SPACE           TO RESP-IDLBBET-UPD                             
372600                             RESP-FLKLAR-UPD                              
372700                             RESP-ADINLOMR-LPL-UPD                        
372800     MOVE +1 TO INDX                                                      
372900     PERFORM UNTIL INDX        >  MAX-KVRADER                             
373000       MOVE SPACE              TO RESP-KDCMDVAL-LINE (INDX)               
373100       ADD +1                  TO INDX                                    
373200     END-PERFORM                                                          
373300     .                                                                    
373400     SKIP3                                                                
373500                                                                          
373600 MFS-LAES-IN-IGEN SECTION.                                                
373700                                                                          
373800     MOVE MFS-ADD-LAES-IN-FAELT TO RESP-IDLBBET-UPD-ATTR                  
373900                                   RESP-FLKLAR-UPD-ATTR                   
374000                                   RESP-ADINLOMR-LPL-UPD-ATTR             
374100     MOVE +1                    TO INDX                                   
374200     PERFORM UNTIL INDX         >  MAX-KVRADER                            
374300       MOVE MFS-ADD-LAES-IN-FAELT                                         
374400                                TO RESP-KDCMDVAL-LINE-ATTR(INDX)          
374500       ADD +1                   TO INDX                                   
374600     END-PERFORM                                                          
374700     .                                                                    
374800     EJECT                                                                
374900                                                                          
375000* --- IMS SEKTIONER ---                                                   
375100* --- IMS SEKTIONER ---                                                   
375200* --- IMS SEKTIONER ---                                                   
375300* --- IMS SEKTIONER ---                                                   
375400                                                                          
375500 IMS-ISRT-ALT-MSG-6191  SECTION.                                          
375600                                                                          
375700     MOVE SPACE TO GODK-STATUSKODER                                       
375800     CALL  CBLTDLI  USING ISRT STAT-PCB P-TO-P-SW                         
375900     MOVE STAT-STATUS-CODE TO STATUS-WS                                   
376000     PERFORM IMS-STATUSKONTROLL                                           
376100     .                                                                    
376200     SKIP3                                                                
376300 IMS-PURG-ALT-MSG-6191  SECTION.                                          
376400                                                                          
376500     MOVE SPACE TO GODK-STATUSKODER                                       
376600     CALL  CBLTDLI  USING PURG STAT-PCB P-TO-P-SW                         
376700     MOVE STAT-STATUS-CODE TO STATUS-WS                                   
376800     PERFORM IMS-STATUSKONTROLL                                           
376900     .                                                                    
377000     SKIP3                                                                
377100 IMS-ISRT-ALT-MSG-6196  SECTION.                                          
377200                                                                          
377300     MOVE SPACE TO GODK-STATUSKODER                                       
377400     CALL  CBLTDLI  USING ISRT AR-PCB P-TO-P-SW                           
377500     MOVE AR-STATUS-CODE TO STATUS-WS                                     
377600     PERFORM IMS-STATUSKONTROLL                                           
377700     .                                                                    
377800     SKIP3                                                                
377900 IMS-PURG-ALT-MSG-6196  SECTION.                                          
378000                                                                          
378100     MOVE SPACE TO GODK-STATUSKODER                                       
378200     CALL  CBLTDLI  USING PURG AR-PCB P-TO-P-SW                           
378300     MOVE AR-STATUS-CODE TO STATUS-WS                                     
378400     PERFORM IMS-STATUSKONTROLL                                           
378500     .                                                                    
378600     EJECT                                                                
378700 IMS-ISRT-ALT-MSG-6197  SECTION.                                          
378800                                                                          
378900     MOVE SPACE TO GODK-STATUSKODER                                       
379000     CALL  CBLTDLI  USING ISRT FR-PCB P-TO-P-SW                           
379100     MOVE FR-STATUS-CODE TO STATUS-WS                                     
379200     PERFORM IMS-STATUSKONTROLL                                           
379300     .                                                                    
379400     SKIP3                                                                
379500 IMS-PURG-ALT-MSG-6197  SECTION.                                          
379600                                                                          
379700     MOVE SPACE TO GODK-STATUSKODER                                       
379800     CALL  CBLTDLI  USING PURG FR-PCB P-TO-P-SW                           
379900     MOVE FR-STATUS-CODE TO STATUS-WS                                     
380000     PERFORM IMS-STATUSKONTROLL                                           
380100     .                                                                    
380200     SKIP3                                                                
380300 IMS-ISRT-ALT-MSG-6199 SECTION.                                           
380400                                                                          
380500     MOVE SPACE TO GODK-STATUSKODER                                       
380600     CALL CBLTDLI USING ISRT ILIST-PCB P-TO-P-SW                          
380700     MOVE ILIST-STATUS-CODE TO STATUS-WS                                  
380800     PERFORM IMS-STATUSKONTROLL                                           
380900     .                                                                    
381000     SKIP2                                                                
381100 IMS-PURG-ALT-MSG-6199 SECTION.                                           
381200                                                                          
381300     MOVE SPACE TO GODK-STATUSKODER                                       
381400     CALL CBLTDLI USING PURG ILIST-PCB P-TO-P-SW                          
381500     MOVE ILIST-STATUS-CODE TO STATUS-WS                                  
381600     PERFORM IMS-STATUSKONTROLL                                           
381700     .                                                                    
381800     SKIP3                                                                
381900 IMS-GU-WDB601    SECTION.                                                
382000     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
382100          DELIMITED BY SIZE INTO SSA1                                     
382200     MOVE '  GE' TO GODK-STATUSKODER                                      
382300     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
382400     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
382500     PERFORM IMS-STATUSKONTROLL                                           
382600     .                                                                    
382700     EJECT                                                                
382800 IMS-GU-ARTC-ARTC01 SECTION.                                              
382900                                                                          
383000     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-K6-X ')'                      
383100          DELIMITED BY SIZE INTO SSA1                                     
383200     MOVE '  GE' TO GODK-STATUSKODER                                      
383300     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-WDK601 SSA1                    
383400     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
383500     PERFORM IMS-STATUSKONTROLL                                           
383600     .                                                                    
383700     SKIP3                                                                
383800 IMS-GU-ARTC-ARTC11 SECTION.                                              
383900                                                                          
384000     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
384100          DELIMITED BY SIZE INTO SSA1                                     
384200     MOVE 'WLARTC11 ' TO SSA2                                             
384300     MOVE '  GE' TO GODK-STATUSKODER                                      
384400     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-WDK611 SSA1 SSA2               
384500     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
384600     PERFORM IMS-STATUSKONTROLL                                           
384700     .                                                                    
384800     SKIP3                                                                
384900 IMS-GET-SATSBEHOV-FIRST SECTION.                                         
385000                                                                          
385100     MOVE 'WLARTC11*F' TO SSA1                                            
385200     MOVE 'WLARTC24  ' TO SSA2                                            
385300     MOVE '  GE' TO GODK-STATUSKODER                                      
385400     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-WDK624 SSA1 SSA2              
385500     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
385600     PERFORM IMS-STATUSKONTROLL                                           
385700     .                                                                    
385800     SKIP3                                                                
385900 IMS-GET-SATSBEHOV-NEXT SECTION.                                          
386000                                                                          
386100     MOVE 'WLARTC11 ' TO SSA1                                             
386200     MOVE 'WLARTC24 ' TO SSA2                                             
386300     MOVE '  GE' TO GODK-STATUSKODER                                      
386400     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-WDK624 SSA1 SSA2              
386500     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
386600     PERFORM IMS-STATUSKONTROLL                                           
386700     .                                                                    
386800     EJECT                                                                
386900 IMS-GU-INLA1-INLA01 SECTION.                                             
387000                                                                          
387100     STRING 'W6INLA01(W6D101KY =' W-W6D101KY-X ')'                        
387200          DELIMITED BY SIZE INTO SSA1                                     
387300     MOVE '  GE' TO GODK-STATUSKODER                                      
387400     CALL CBLTDLI USING GU INLA1-PCB DLI-IO-AREA1 SSA1                    
387500     MOVE INLA1-STATUS-CODE TO STATUS-WS                                  
387600     PERFORM IMS-STATUSKONTROLL                                           
387700     .                                                                    
387800     EJECT                                                                
387900 IMS-GHU-INLA1-INLA01 SECTION.                                            
388000                                                                          
388100     STRING 'W6INLA01(W6D101KY =' W-W6D101KY-X ')'                        
388200          DELIMITED BY SIZE INTO SSA1                                     
388300     MOVE '    ' TO GODK-STATUSKODER                                      
388400     CALL CBLTDLI USING GHU INLA1-PCB DLI-IO-AREA1 SSA1                   
388500     MOVE INLA1-STATUS-CODE TO STATUS-WS                                  
388600     PERFORM IMS-STATUSKONTROLL                                           
388700     .                                                                    
388800     SKIP3                                                                
388900 IMS-REPL-INLA1-INLA01 SECTION.                                           
389000                                                                          
389100     MOVE '    ' TO GODK-STATUSKODER                                      
389200     CALL CBLTDLI USING REPL INLA1-PCB DLI-IO-AREA1                       
389300     MOVE INLA1-STATUS-CODE TO STATUS-WS                                  
389400     PERFORM IMS-STATUSKONTROLL                                           
389500     .                                                                    
389600     SKIP3                                                                
389700 IMS-GNP-INLA1-INLA11 SECTION.                                            
389800                                                                          
389900     MOVE 'W6INLA11'   TO SSA1                                            
390000     MOVE '  GE' TO GODK-STATUSKODER                                      
390100     CALL CBLTDLI USING GNP INLA1-PCB DLI-IO-AREA2 SSA1                   
390200     MOVE INLA1-STATUS-CODE TO STATUS-WS                                  
390300     PERFORM IMS-STATUSKONTROLL                                           
390400     .                                                                    
390500     SKIP3                                                                
390600 IMS-GHNP-INLA1-INLA11-KVAL SECTION.                                      
390700                                                                          
390800     STRING 'W6INLA11(IDRADNRI>=' W-IDRADNR-INL-X ')'                     
390900          DELIMITED BY SIZE INTO SSA1                                     
391000     MOVE '  GE' TO GODK-STATUSKODER                                      
391100     CALL CBLTDLI USING GHNP INLA1-PCB DLI-IO-AREA2 SSA1                  
391200     MOVE INLA1-STATUS-CODE TO STATUS-WS                                  
391300     PERFORM IMS-STATUSKONTROLL                                           
391400     .                                                                    
391500     SKIP3                                                                
391600 IMS-GHNP-INLA1-INLA11 SECTION.                                           
391700                                                                          
391800     MOVE 'W6INLA11'  TO SSA1                                             
391900     MOVE '  GE' TO GODK-STATUSKODER                                      
392000     CALL CBLTDLI USING GHNP INLA1-PCB DLI-IO-AREA2 SSA1                  
392100     MOVE INLA1-STATUS-CODE TO STATUS-WS                                  
392200     PERFORM IMS-STATUSKONTROLL                                           
392300     .                                                                    
392400     SKIP3                                                                
392500 IMS-REPL-INLA1-INLA11 SECTION.                                           
392600                                                                          
392700     MOVE '    ' TO GODK-STATUSKODER                                      
392800     CALL CBLTDLI USING REPL INLA1-PCB DLI-IO-AREA2                       
392900     MOVE INLA1-STATUS-CODE TO STATUS-WS                                  
393000     PERFORM IMS-STATUSKONTROLL                                           
393100     .                                                                    
393200     EJECT                                                                
393300 IMS-ISRT-W6D111 SECTION.                                                 
393400                                                                          
393500     STRING 'W6INLA01(W6D101KY =' W-W6D101KY-X ')'                        
393600          DELIMITED BY SIZE INTO SSA1                                     
393700     MOVE 'W6INLA11'   TO SSA2                                            
393800     MOVE '  II' TO GODK-STATUSKODER                                      
393900     CALL CBLTDLI USING ISRT INLA1-PCB DLI-IO-AREA2 SSA1 SSA2             
394000     MOVE INLA1-STATUS-CODE TO STATUS-WS                                  
394100     PERFORM IMS-STATUSKONTROLL                                           
394200     .                                                                    
394300     SKIP3                                                                
394400 IMS-GNP-INLA1-INLA21-KVAL SECTION.                                       
394500                                                                          
394600     STRING 'W6INLA11(IDRADNRI>=' W-IDRADNR-INL-X ')'                     
394700          DELIMITED BY SIZE INTO SSA1                                     
394800     MOVE 'W6INLA21'   TO SSA2                                            
394900     MOVE '  GE' TO GODK-STATUSKODER                                      
395000     CALL CBLTDLI USING GNP INLA1-PCB DLI-IO-AREA3 SSA1 SSA2              
395100     MOVE INLA1-STATUS-CODE TO STATUS-WS                                  
395200     PERFORM IMS-STATUSKONTROLL                                           
395300     .                                                                    
395400     SKIP3                                                                
395500 IMS-GHNP-INLA1-INLA21 SECTION.                                           
395600                                                                          
395700     STRING 'W6INLA11(IDRADNRI =' W-IDRADNR-INL-X ')'                     
395800          DELIMITED BY SIZE INTO SSA1                                     
395900     MOVE 'W6INLA21'  TO SSA2                                             
396000     MOVE '  GE'      TO GODK-STATUSKODER                                 
396100     CALL CBLTDLI USING GHNP INLA1-PCB DLI-IO-AREA3 SSA1 SSA2             
396200     MOVE INLA1-STATUS-CODE TO STATUS-WS                                  
396300     PERFORM IMS-STATUSKONTROLL                                           
396400     .                                                                    
396500     SKIP3                                                                
396600 IMS-GHU-INLA1-INLA21    SECTION.                                         
396700                                                                          
396800     STRING 'W6INLA01(W6D101KY =' W-W6D101KY-X ')'                        
396900          DELIMITED BY SIZE INTO SSA1                                     
397000     STRING 'W6INLA11(IDRADNRI =' W-IDRADNR-INL-X ')'                     
397100          DELIMITED BY SIZE INTO SSA2                                     
397200     STRING 'W6INLA21(IDRADNR  =' W-IDRADNR-X ')'                         
397300          DELIMITED BY SIZE INTO SSA3                                     
397400     MOVE '  GE' TO GODK-STATUSKODER                                      
397500     CALL CBLTDLI USING GHU INLA1-PCB DLI-IO-AREA3 SSA1 SSA2 SSA3         
397600     MOVE INLA1-STATUS-CODE TO STATUS-WS                                  
397700     PERFORM IMS-STATUSKONTROLL                                           
397800     .                                                                    
397900     SKIP3                                                                
398000 IMS-GHU-INLA1-INLA11    SECTION.                                         
398100                                                                          
398200     STRING 'W6INLA01(W6D101KY =' W-W6D101KY-X ')'                        
398300          DELIMITED BY SIZE INTO SSA1                                     
398400     STRING 'W6INLA11(IDRADNRI =' W-IDRADNR-INL-X ')'                     
398500          DELIMITED BY SIZE INTO SSA2                                     
398600     MOVE '  GE' TO GODK-STATUSKODER                                      
398700     CALL CBLTDLI USING GHU INLA1-PCB DLI-IO-AREA3 SSA1 SSA2              
398800     MOVE INLA1-STATUS-CODE TO STATUS-WS                                  
398900     PERFORM IMS-STATUSKONTROLL                                           
399000     .                                                                    
399100     SKIP3                                                                
399200 IMS-GHU-INLA4-W6D121 SECTION.                                            
399300                                                                          
399400     STRING 'W6INLA01(W6D101KY =' W-W6D101KY-X ')'                        
399500          DELIMITED BY SIZE INTO SSA1                                     
399600     STRING 'W6INLA11(IDRADNRI =' W-IDRADNR-INL-X ')'                     
399700          DELIMITED BY SIZE INTO SSA2                                     
399800     STRING 'W6INLA21(IDRADNR  =' W-IDRADNR-X ')'                         
399900          DELIMITED BY SIZE INTO SSA3                                     
400000     MOVE '  GE' TO GODK-STATUSKODER                                      
400100     CALL CBLTDLI USING GHU INLA4-PCB D121-AREA SSA1 SSA2 SSA3            
400200     MOVE INLA4-STATUS-CODE TO STATUS-WS                                  
400300     PERFORM IMS-STATUSKONTROLL                                           
400400     .                                                                    
400500     SKIP3                                                                
400600 IMS-REPL-INLA1-INLA21 SECTION.                                           
400700                                                                          
400800     MOVE '    '      TO GODK-STATUSKODER                                 
400900     CALL CBLTDLI USING REPL INLA1-PCB DLI-IO-AREA3                       
401000     MOVE INLA1-STATUS-CODE TO STATUS-WS                                  
401100     PERFORM IMS-STATUSKONTROLL                                           
401200     .                                                                    
401300     SKIP3                                                                
401400 IMS-DLET-INLA4-W6D121 SECTION.                                           
401500                                                                          
401600     MOVE '    '      TO GODK-STATUSKODER                                 
401700     CALL CBLTDLI USING DLET INLA4-PCB D121-AREA                          
401800     MOVE INLA4-STATUS-CODE TO STATUS-WS                                  
401900     PERFORM IMS-STATUSKONTROLL                                           
402000     .                                                                    
402100     SKIP3                                                                
402200 IMS-ISRT-W6D121 SECTION.                                                 
402300                                                                          
402400     STRING 'W6INLA01(W6D101KY =' W-W6D101KY-X ')'                        
402500          DELIMITED BY SIZE INTO SSA1                                     
402600     STRING 'W6INLA11(IDRADNRI =' W-IDRADNR-INS-X ')'                     
402700          DELIMITED BY SIZE INTO SSA2                                     
402800     MOVE 'W6INLA21'   TO SSA3                                            
402900     MOVE '  II' TO GODK-STATUSKODER                                      
403000     CALL CBLTDLI USING ISRT INLA1-PCB DLI-IO-AREA3 SSA1 SSA2 SSA3        
403100     MOVE INLA1-STATUS-CODE TO STATUS-WS                                  
403200     PERFORM IMS-STATUSKONTROLL                                           
403300     .                                                                    
403400     SKIP3                                                                
403500 IMS-GU-INLA2-INLA01-KVAL SECTION.                                        
403600                                                                          
403700     STRING 'W6INLA01(W6D1ASEQ>=' W-W6D1ASEQ-X ')'                        
403800          DELIMITED BY SIZE INTO SSA1                                     
403900     MOVE '  GE' TO GODK-STATUSKODER                                      
404000     CALL CBLTDLI USING GU INLA2-PCB DLI-IO-AREA1 SSA1                    
404100     MOVE INLA2-STATUS-CODE TO STATUS-WS                                  
404200     PERFORM IMS-STATUSKONTROLL                                           
404300     .                                                                    
404400     SKIP3                                                                
404500 IMS-GU-INLA2-INLA01 SECTION.                                             
404600                                                                          
404700     STRING 'W6INLA01(W6D1ASEQ>=' W-W6D1ASEQ-MIN-X                        
404800                    '&W6D1ASEQ<=' W-W6D1ASEQ-MAX-X ')'                    
404900          DELIMITED BY SIZE INTO SSA1                                     
405000     MOVE '  GE' TO GODK-STATUSKODER                                      
405100     CALL CBLTDLI USING GU INLA2-PCB DLI-IO-AREA1 SSA1                    
405200     MOVE INLA2-STATUS-CODE TO STATUS-WS                                  
405300     PERFORM IMS-STATUSKONTROLL                                           
405400     .                                                                    
405500     EJECT                                                                
405600 IMS-GN-INLA2-INLA01 SECTION.                                             
405700                                                                          
405800     STRING 'W6INLA01(W6D1ASEQ>=' W-W6D1ASEQ-MIN-X                        
405900                    '&W6D1ASEQ<=' W-W6D1ASEQ-MAX-X ')'                    
406000          DELIMITED BY SIZE INTO SSA1                                     
406100     MOVE '  GEGB' TO GODK-STATUSKODER                                    
406200     CALL CBLTDLI USING GN INLA2-PCB DLI-IO-AREA1 SSA1                    
406300     MOVE INLA2-STATUS-CODE TO STATUS-WS                                  
406400     PERFORM IMS-STATUSKONTROLL                                           
406500     .                                                                    
406600     SKIP3                                                                
406700 IMS-GNP-INLA2-INLA11 SECTION.                                            
406800                                                                          
406900     MOVE 'W6INLA11'   TO SSA1                                            
407000     MOVE '  GE' TO GODK-STATUSKODER                                      
407100     CALL CBLTDLI USING GNP INLA2-PCB DLI-IO-AREA2 SSA1                   
407200     MOVE INLA2-STATUS-CODE TO STATUS-WS                                  
407300     PERFORM IMS-STATUSKONTROLL                                           
407400     .                                                                    
407500     SKIP3                                                                
407600 IMS-GU-INLA3-INLA11 SECTION.                                             
407700                                                                          
407800     STRING 'W6INLA01(W6D101KY =' W-W6D101KY-X ')'                        
407900          DELIMITED BY SIZE INTO SSA1                                     
408000     STRING 'W6INLA11(IDRADNRI =' W-IDRADNR-INL-X ')'                     
408100          DELIMITED BY SIZE INTO SSA2                                     
408200     MOVE '  GE' TO GODK-STATUSKODER                                      
408300     CALL CBLTDLI USING GU INLA3-PCB DLI-IO-AREA2 SSA1 SSA2               
408400     MOVE INLA3-STATUS-CODE TO STATUS-WS                                  
408500     PERFORM IMS-STATUSKONTROLL                                           
408600     .                                                                    
408700     EJECT                                                                
408800 IMS-GU-INLB-INLB01-KVAL  SECTION.                                        
408900                                                                          
409000     STRING 'W6INLB01(W6D1A1KY =' W-W6D1A1KY-X ')'                        
409100          DELIMITED BY SIZE INTO SSA1                                     
409200     MOVE '  GE' TO GODK-STATUSKODER                                      
409300     CALL CBLTDLI USING GU INLB-PCB DLI-IO-AREA1 SSA1                     
409400     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
409500     PERFORM IMS-STATUSKONTROLL                                           
409600     .                                                                    
409700     SKIP3                                                                
409800 IMS-GU-INLB-INLB01 SECTION.                                              
409900                                                                          
410000     STRING 'W6INLB01(W6D1A1KY>=' W-W6D1A1KY-MIN-X                        
410100                    '&W6D1A1KY<=' W-W6D1A1KY-MAX-X                        
410200                    '&IDDC     =' WS-IDDC                                 
410300                    '&IDLBBET  =' W-IDLBBET ')'                           
410400          DELIMITED BY SIZE INTO SSA1                                     
410500     MOVE '  GE' TO GODK-STATUSKODER                                      
410600     CALL CBLTDLI USING GU INLB-PCB DLI-IO-AREA1 SSA1                     
410700     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
410800     PERFORM IMS-STATUSKONTROLL                                           
410900     .                                                                    
411000     SKIP3                                                                
411100 IMS-GN-INLB-INLB01 SECTION.                                              
411200                                                                          
411300     STRING 'W6INLB01(W6D1A1KY>=' W-W6D1A1KY-MIN-X                        
411400                    '&W6D1A1KY<=' W-W6D1A1KY-MAX-X                        
411500                    '&IDDC     =' WS-IDDC                                 
411600                    '&IDLBBET  =' W-IDLBBET ')'                           
411700          DELIMITED BY SIZE INTO SSA1                                     
411800     MOVE '  GEGB' TO GODK-STATUSKODER                                    
411900     CALL CBLTDLI USING GN INLB-PCB DLI-IO-AREA1 SSA1                     
412000     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
412100     PERFORM IMS-STATUSKONTROLL                                           
412200     .                                                                    
412300     EJECT                                                                
412400 IMS-GU-INLA11-D    SECTION.                                              
412500                                                                          
412600     STRING 'W6INLE01(W6D1D1KY>=' W-W6D1D1KY-MIN-X                        
412700                    '&W6D1D1KY< ' W-W6D1D1KY-MAX-X ')'                    
412800          DELIMITED BY SIZE INTO SSA1                                     
412900     MOVE '  GE' TO GODK-STATUSKODER                                      
413000     CALL CBLTDLI USING GU INLA-D-PCB DLI-IO-AREA6 SSA1                   
413100     MOVE INLA-D-STATUS-CODE TO STATUS-WS                                 
413200     PERFORM IMS-STATUSKONTROLL                                           
413300     .                                                                    
413400     EJECT                                                                
413500 IMS-GU-W6D1ISEQ  SECTION.                                                
413600                                                                          
413700     STRING 'W6D111  (W6D1ISEQ =' W-W6D1I1KY-X ')'                        
413800             DELIMITED BY SIZE INTO SSA1                                  
413900     MOVE '  GE'                 TO GODK-STATUSKODER                      
414000     CALL CBLTDLI USING GU       W6D1-I-PCB                               
414100                                 DLI-IO-W6D1I11                           
414200                                 SSA1                                     
414300     MOVE W6D1-I-STATUS-CODE       TO STATUS-WS                           
414400     PERFORM IMS-STATUSKONTROLL                                           
414500     .                                                                    
414600     SKIP3                                                                
414700 IMS-GN-W6D1ISEQ  SECTION.                                                
414800                                                                          
414900     STRING 'W6D111  (W6D1ISEQ =' W-W6D1I1KY-X ')'                        
415000             DELIMITED BY SIZE INTO SSA1                                  
415100     MOVE '  GE'                 TO GODK-STATUSKODER                      
415200     CALL CBLTDLI USING GN       W6D1-I-PCB                               
415300                                 DLI-IO-W6D1I11                           
415400                                 SSA1                                     
415500     MOVE W6D1-I-STATUS-CODE       TO STATUS-WS                           
415600     PERFORM IMS-STATUSKONTROLL                                           
415700     .                                                                    
415800     SKIP3                                                                
415900 IMS-GNP-W6D121 SECTION.                                                  
416000                                                                          
416100     MOVE 'W6D121  '  TO SSA1                                             
416200     MOVE '  GE'      TO GODK-STATUSKODER                                 
416300     CALL CBLTDLI USING GNP W6D1-I-PCB DLI-IO-W6D1I21 SSA1                
416400     MOVE W6D1-I-STATUS-CODE TO STATUS-WS                                 
416500     PERFORM IMS-STATUSKONTROLL                                           
416600     .                                                                    
416700     SKIP3                                                                
416800 IMS-GU-PLAA-PLAA11 SECTION.                                              
416900                                                                          
417000     STRING 'W6PLAA01(W6GXKEY  =' W-W6GXKEY-6005-X ')'                    
417100          DELIMITED BY SIZE INTO SSA1                                     
417200     STRING 'W6PLAA11(W6GXKEY  =' W-W6GXKEY-6006-X ')'                    
417300          DELIMITED BY SIZE INTO SSA2                                     
417400     MOVE '  GE' TO GODK-STATUSKODER                                      
417500     CALL CBLTDLI USING GU PLAA-PCB DLI-IO-AREA4 SSA1 SSA2                
417600     MOVE PLAA-STATUS-CODE TO STATUS-WS                                   
417700     PERFORM IMS-STATUSKONTROLL                                           
417800     .                                                                    
417900     SKIP3                                                                
418000 IMS-GU-PLAA-PLAA11-BLANK SECTION.                                        
418100                                                                          
418200     STRING 'W6PLAA01(W6GXKEY  =' W-W6GXKEY-6005-X ')'                    
418300          DELIMITED BY SIZE INTO SSA1                                     
418400     STRING 'W6PLAA11(W6GXKEY  =' W-W6GXKEY-6006-X ')'                    
418500          DELIMITED BY SIZE INTO SSA2                                     
418600     MOVE '    ' TO GODK-STATUSKODER                                      
418700     CALL CBLTDLI USING GU PLAA-PCB DLI-IO-AREA4 SSA1 SSA2                
418800     MOVE PLAA-STATUS-CODE TO STATUS-WS                                   
418900     PERFORM IMS-STATUSKONTROLL                                           
419000     .                                                                    
419100     SKIP3                                                                
419200 IMS-GU-PLAA-PLAA11-NDC SECTION.                                          
419300                                                                          
419400     STRING 'W6PLAA01(W6GXKEY  =' W-W6GXKEY-6005-X ')'                    
419500          DELIMITED BY SIZE INTO SSA1                                     
419600     STRING 'W6PLAA11(W6GXKEY  =' W-W6GXKEY-6006-X ')'                    
419700          DELIMITED BY SIZE INTO SSA2                                     
419800     MOVE '  GE' TO GODK-STATUSKODER                                      
419900     CALL CBLTDLI USING GU PLAA-PCB DLI-IO-AREA4 SSA1 SSA2                
420000     MOVE PLAA-STATUS-CODE TO STATUS-WS                                   
420100     PERFORM IMS-STATUSKONTROLL                                           
420200     .                                                                    
420300     SKIP3                                                                
420400 IMS-GU-PLAA11-PAR  SECTION.                                              
420500                                                                          
420600     STRING 'W6PLAA01(W6GXKEY  =' W-W6GXKEY-6005-X ')'                    
420700          DELIMITED BY SIZE INTO SSA1                                     
420800     STRING 'W6PLAA11(ADINLOMP =' W-6006-ADINLOMR-PAR-X ')'               
420900          DELIMITED BY SIZE INTO SSA2                                     
421000     MOVE '  GE' TO GODK-STATUSKODER                                      
421100     CALL CBLTDLI USING GU PLAA-PCB DLI-IO-AREA4 SSA1 SSA2                
421200     MOVE PLAA-STATUS-CODE TO STATUS-WS                                   
421300     PERFORM IMS-STATUSKONTROLL                                           
421400     .                                                                    
421500     SKIP3                                                                
421600 IMS-GN-PLAA11-PAR SECTION.                                               
421700                                                                          
421800     STRING 'W6PLAA11(ADINLOMP =' W-6006-ADINLOMR-PAR-X ')'               
421900          DELIMITED BY SIZE INTO SSA1                                     
422000     MOVE '  GEGB' TO GODK-STATUSKODER                                    
422100     CALL CBLTDLI USING GN PLAA-PCB DLI-IO-AREA4  SSA1                    
422200     MOVE PLAA-STATUS-CODE TO STATUS-WS                                   
422300     PERFORM IMS-STATUSKONTROLL                                           
422400     .                                                                    
422500     EJECT                                                                
422600 IMS-GU-LASA-LASA11 SECTION.                                              
422700                                                                          
422800     STRING 'W6LASA01(W6GXKEY  =' W-W6GXKEY-6107-X ')'                    
422900          DELIMITED BY SIZE INTO SSA1                                     
423000     STRING 'W6LASA11(W6GXKEY  =' W-W6GXKEY-6108-X ')'                    
423100          DELIMITED BY SIZE INTO SSA2                                     
423200     MOVE '  GE' TO GODK-STATUSKODER                                      
423300     CALL CBLTDLI USING GU LASA-PCB DLI-IO-AREA4 SSA1 SSA2                
423400     MOVE LASA-STATUS-CODE TO STATUS-WS                                   
423500     PERFORM IMS-STATUSKONTROLL                                           
423600     .                                                                    
423700     SKIP3                                                                
423800 IMS-GHU-LASA-LASA11 SECTION.                                             
423900                                                                          
424000     STRING 'W6LASA01(W6GXKEY  =' W-W6GXKEY-6107-X ')'                    
424100          DELIMITED BY SIZE INTO SSA1                                     
424200     STRING 'W6LASA11(W6GXKEY  =' W-W6GXKEY-6108-X ')'                    
424300          DELIMITED BY SIZE INTO SSA2                                     
424400     MOVE '  GE' TO GODK-STATUSKODER                                      
424500     CALL CBLTDLI USING GHU LASA-PCB DLI-IO-AREA4 SSA1 SSA2               
424600     MOVE LASA-STATUS-CODE TO STATUS-WS                                   
424700     PERFORM IMS-STATUSKONTROLL                                           
424800     .                                                                    
424900     EJECT                                                                
425000 IMS-REPL-LASA-LASA11 SECTION.                                            
425100                                                                          
425200     MOVE '    ' TO GODK-STATUSKODER                                      
425300     CALL CBLTDLI USING REPL LASA-PCB DLI-IO-AREA4                        
425400     MOVE LASA-STATUS-CODE TO STATUS-WS                                   
425500     PERFORM IMS-STATUSKONTROLL                                           
425600     .                                                                    
425700     SKIP3                                                                
425800 IMS-ISRT-LASA-LASA11 SECTION.                                            
425900                                                                          
426000     STRING 'W6LASA01(W6GXKEY  =' W-W6GXKEY-6107-X ')'                    
426100          DELIMITED BY SIZE INTO SSA1                                     
426200     MOVE 'W6LASA11'   TO SSA2                                            
426300     MOVE '    ' TO GODK-STATUSKODER                                      
426400     CALL CBLTDLI USING ISRT LASA-PCB DLI-IO-AREA4 SSA1 SSA2              
426500     MOVE LASA-STATUS-CODE TO STATUS-WS                                   
426600     PERFORM IMS-STATUSKONTROLL                                           
426700     .                                                                    
426800     SKIP3                                                                
426900 IMS-GU-LASA-LASA21 SECTION.                                              
427000                                                                          
427100     STRING 'W6LASA01(W6GXKEY  =' W-W6GXKEY-6107-X ')'                    
427200          DELIMITED BY SIZE INTO SSA1                                     
427300     STRING 'W6LASA11*P(W6GXKEY  =' W-W6GXKEY-6108-X ')'                  
427400          DELIMITED BY SIZE INTO SSA2                                     
427500     MOVE 'W6LASA21'     TO SSA3                                          
427600     MOVE '  GE' TO GODK-STATUSKODER                                      
427700     CALL CBLTDLI USING GU LASA-PCB DLI-IO-AREA4 SSA1 SSA2 SSA3           
427800     MOVE LASA-STATUS-CODE TO STATUS-WS                                   
427900     PERFORM IMS-STATUSKONTROLL                                           
428000     .                                                                    
428100     EJECT                                                                
428200 IMS-GHU-LASA-LASA21 SECTION.                                             
428300                                                                          
428400     STRING 'W6LASA01(W6GXKEY  =' W-W6GXKEY-6107-X ')'                    
428500          DELIMITED BY SIZE INTO SSA1                                     
428600     STRING 'W6LASA11(W6GXKEY  =' W-W6GXKEY-6108-X ')'                    
428700          DELIMITED BY SIZE INTO SSA2                                     
428800     STRING 'W6LASA21(W6GXKEY  =' W-W6GXKEY-6110-X ')'                    
428900          DELIMITED BY SIZE INTO SSA3                                     
429000     MOVE '  ' TO GODK-STATUSKODER                                        
429100     CALL CBLTDLI USING GHU LASA-PCB DLI-IO-AREA4 SSA1 SSA2 SSA3          
429200     MOVE LASA-STATUS-CODE TO STATUS-WS                                   
429300     PERFORM IMS-STATUSKONTROLL                                           
429400     .                                                                    
429500     EJECT                                                                
429600 IMS-REPL-LASA-LASA21 SECTION.                                            
429700                                                                          
429800     MOVE 'W6LASA21'   TO SSA1                                            
429900     MOVE '    ' TO GODK-STATUSKODER                                      
430000     CALL CBLTDLI USING REPL LASA-PCB DLI-IO-AREA4 SSA1                   
430100     MOVE LASA-STATUS-CODE TO STATUS-WS                                   
430200     PERFORM IMS-STATUSKONTROLL                                           
430300     .                                                                    
430400     SKIP3                                                                
430500 IMS-GNP-LASA-LASA21 SECTION.                                             
430600                                                                          
430700     MOVE 'W6LASA21'     TO SSA1                                          
430800     MOVE '  GE' TO GODK-STATUSKODER                                      
430900     CALL CBLTDLI USING GNP LASA-PCB DLI-IO-AREA4 SSA1                    
431000     MOVE LASA-STATUS-CODE TO STATUS-WS                                   
431100     PERFORM IMS-STATUSKONTROLL                                           
431200     .                                                                    
431300     EJECT                                                                
431400 IMS-ISRT-LASA-LASA21 SECTION.                                            
431500                                                                          
431600     MOVE 'W6LASA21'   TO SSA1                                            
431700     MOVE '  II' TO GODK-STATUSKODER                                      
431800     CALL CBLTDLI USING ISRT LASA-PCB DLI-IO-AREA4 SSA1                   
431900     MOVE LASA-STATUS-CODE TO STATUS-WS                                   
432000     PERFORM IMS-STATUSKONTROLL                                           
432100     .                                                                    
432200     SKIP3                                                                
432300 IMS-GHU-LOPA-LOPA11 SECTION.                                             
432400                                                                          
432500     STRING 'W6LOPA01(W6GXKEY  =' W-W6GXKEY-6017-X ')'                    
432600          DELIMITED BY SIZE INTO SSA1                                     
432700     STRING 'W6LOPA11(KDSEGKEY =' W-W6GXKEY-6018-X ')'                    
432800          DELIMITED BY SIZE INTO SSA2                                     
432900     MOVE '    ' TO GODK-STATUSKODER                                      
433000     CALL CBLTDLI USING GHU LOPA-PCB DLI-IO-AREA5 SSA1 SSA2               
433100     MOVE LOPA-STATUS-CODE TO STATUS-WS                                   
433200     PERFORM IMS-STATUSKONTROLL                                           
433300     .                                                                    
433400     SKIP3                                                                
433500 IMS-REPL-LOPA-LOPA11 SECTION.                                            
433600                                                                          
433700     MOVE '    ' TO GODK-STATUSKODER                                      
433800     CALL CBLTDLI USING REPL LOPA-PCB DLI-IO-AREA5                        
433900     MOVE LOPA-STATUS-CODE TO STATUS-WS                                   
434000     PERFORM IMS-STATUSKONTROLL                                           
434100     .                                                                    
434200     SKIP3                                                                
434300 IMS-GET-WDJ1-CSEQ-NEXT SECTION.                                          
434400                                                                          
434500     STRING 'WDJ111  *D(WDJ1CSEQ =' W-WDJ1CSEQ-X ')'                      
434600          DELIMITED BY SIZE INTO SSA1                                     
434700     MOVE 'WDJ101   ' TO SSA2                                             
434800     MOVE '  GE' TO GODK-STATUSKODER                                      
434900     CALL CBLTDLI USING GN WDJ1-PCB DLI-IO-WDJ1 SSA1 SSA2                 
435000     MOVE WDJ1-STATUS-CODE TO STATUS-WS                                   
435100     PERFORM IMS-STATUSKONTROLL                                           
435200     .                                                                    
435300     EJECT                                                                
435400 IMS-GET-WDJ211-ESEQ-FIRST SECTION.                                       
435500                                                                          
435600     STRING 'WDJ211  (WDJ2ESEQ =' W-WDJ2ESEQ-X ')'                        
435700          DELIMITED BY SIZE INTO SSA1                                     
435800     MOVE '  GEGB' TO GODK-STATUSKODER                                    
435900     CALL CBLTDLI USING GU WDJ2-PCB DLI-IO-WDJ211 SSA1                    
436000     MOVE WDJ2-STATUS-CODE TO STATUS-WS                                   
436100     PERFORM IMS-STATUSKONTROLL                                           
436200     .                                                                    
436300     SKIP3                                                                
436400 IMS-GET-WDJ211-ESEQ-NEXT SECTION.                                        
436500                                                                          
436600     STRING 'WDJ211  (WDJ2ESEQ =' W-WDJ2ESEQ-X ')'                        
436700          DELIMITED BY SIZE INTO SSA1                                     
436800     MOVE '  GEGB' TO GODK-STATUSKODER                                    
436900     CALL CBLTDLI USING GN WDJ2-PCB DLI-IO-WDJ211 SSA1                    
437000     MOVE WDJ2-STATUS-CODE TO STATUS-WS                                   
437100     PERFORM IMS-STATUSKONTROLL                                           
437200     .                                                                    
437300     SKIP3                                                                
437400 IMS-GU-WDK901           SECTION.                                         
437500     STRING 'WDK901  (IDARTNR  =' W-IDARTNR-X ')'                         
437600            DELIMITED BY SIZE INTO SSA1                                   
437700     MOVE '  GE' TO GODK-STATUSKODER                                      
437800     CALL CBLTDLI USING GU WDK9-PCB DLI-IO-WDK901 SSA1                    
437900     MOVE WDK9-STATUS-CODE TO STATUS-WS                                   
438000     PERFORM IMS-STATUSKONTROLL                                           
438100     .                                                                    
438200     EJECT                                                                
438300 IMS-GU-WDL201 SECTION.                                                   
438400                                                                          
438500     STRING 'WDL201  (IDARTNR  =' W-IDARTNR-X ')'                         
438600          DELIMITED BY SIZE INTO SSA1                                     
438700     MOVE '  GE' TO GODK-STATUSKODER                                      
438800     CALL CBLTDLI USING GU WDL2-PCB DLI-IO-WDL201 SSA1                    
438900     MOVE WDL2-STATUS-CODE TO STATUS-WS                                   
439000     PERFORM IMS-STATUSKONTROLL                                           
439100     .                                                                    
439200     EJECT                                                                
439300 IMS-GNP-WDL221 SECTION.                                                  
439400                                                                          
439500     STRING 'WDL211     '                                                 
439600          DELIMITED BY SIZE INTO SSA1                                     
439700     STRING 'WDL221     '                                                 
439800          DELIMITED BY SIZE INTO SSA2                                     
439900     MOVE '  GE' TO GODK-STATUSKODER                                      
440000     CALL CBLTDLI USING GNP WDL2-PCB DLI-IO-WDL221 SSA1 SSA2              
440100     MOVE WDL2-STATUS-CODE TO STATUS-WS                                   
440200     PERFORM IMS-STATUSKONTROLL                                           
440300     .                                                                    
440400     EJECT                                                                
440500 IMS-GU-WDD901 SECTION.                                                   
440600     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
440700          DELIMITED BY SIZE INTO SSA1                                     
440800     MOVE '  GE' TO GODK-STATUSKODER                                      
440900     CALL CBLTDLI USING GU WDD9-PCB DLI-IO-WDD901 SSA1                    
441000     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
441100     PERFORM IMS-STATUSKONTROLL                                           
441200     .                                                                    
441300     EJECT                                                                
441400 IMS-GNP-WDD905 SECTION.                                                  
441500     STRING 'WDD905  (KDAVROP  =' W-KDAVROP-D9-X ')'                      
441600          DELIMITED BY SIZE INTO SSA1                                     
441700     MOVE '  GE' TO GODK-STATUSKODER                                      
441800     CALL CBLTDLI USING GNP WDD9-PCB DLI-IO-WDD905 SSA1                   
441900     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
442000     PERFORM IMS-STATUSKONTROLL                                           
442100     .                                                                    
442200     EJECT                                                                
442300 IMS-ROLLBACK    SECTION.                                                 
442400                                                                          
442500     CALL CBLTDLI USING ROLB    MSG-PCB                                   
442600     .                                                                    
442700     SKIP2                                                                
442800 IMS-GU-W6D201 SECTION.                                                   
442900     STRING 'W6D201  (IDARTNR  =' W-IDARTNR-X ')'                         
443000             DELIMITED BY SIZE INTO SSA1                                  
443100     MOVE '  GE' TO GODK-STATUSKODER                                      
443200     CALL CBLTDLI USING GU W6D2-PCB DLI-IO-W6D211 SSA1                    
443300     MOVE W6D2-STATUS-CODE TO STATUS-WS                                   
443400     PERFORM IMS-STATUSKONTROLL                                           
443500     .                                                                    
443600     SKIP3                                                                
443700 IMS-GNP-W6D211 SECTION.                                                  
443800     MOVE 'W6D211  ' TO SSA1                                              
443900     MOVE '  GE' TO GODK-STATUSKODER                                      
444000     CALL CBLTDLI USING GNP W6D2-PCB DLI-IO-W6D211 SSA1                   
444100     MOVE W6D2-STATUS-CODE TO STATUS-WS                                   
444200     PERFORM IMS-STATUSKONTROLL                                           
444300     .                                                                    
444400     SKIP3                                                                
444500 IMS-GU-WDD801 SECTION.                                                   
444600                                                                          
444700     STRING 'WDD801  (IDARTNR  =' W-IDARTNR-X ')'                         
444800             DELIMITED BY SIZE INTO SSA1                                  
444900     MOVE '  GE' TO GODK-STATUSKODER                                      
445000     CALL CBLTDLI USING GU WDD8-PCB DLI-IO-WDD801 SSA1                    
445100     MOVE WDD8-STATUS-CODE TO STATUS-WS                                   
445200     PERFORM IMS-STATUSKONTROLL                                           
445300     EJECT                                                                
445400     .                                                                    
445500 IMS-GNP-WDD811 SECTION.                                                  
445600                                                                          
445700     STRING 'WDD811  (IDDC     =' W-IDDC-X ')'                            
445800             DELIMITED BY SIZE INTO SSA1                                  
445900     MOVE '  GEGB' TO GODK-STATUSKODER                                    
446000     CALL CBLTDLI USING GNP WDD8-PCB DLI-IO-WDD811 SSA1                   
446100     MOVE WDD8-STATUS-CODE TO STATUS-WS                                   
446200     PERFORM IMS-STATUSKONTROLL                                           
446300                                                                          
446400     EJECT                                                                
446500     .                                                                    
446600 IMS-STATUSKONTROLL SECTION.                                              
446700                                                                          
446800     SET STATUS-IX TO 1                                                   
446900     SEARCH GODK-STATUS                                                   
447000       AT END                                                             
447100         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
447200         DELIMITED BY SIZE INTO ERRORTEXT                                 
447300         CALL FELLOG                                                      
447400       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
447500         CONTINUE                                                         
447600     END-SEARCH                                                           
447700     .                                                                    
