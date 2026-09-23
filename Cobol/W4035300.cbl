000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4035300.                                                
000300 AUTHOR.         ROGER OLSSON.                                            
000400 DATE-WRITTEN.   90/04/25.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION.                                                            
000800*        HÄMTAR ORDERDELAR TILL EN PLOCKSATS.                             
000900*        ORDERDELAR KAN LÄSAS PÅ FÖLJANDE SÄTT:                           
001000*           1) VIA PRC-KANAL                                              
001100*           2) VIA PRC-KANAL & TRANSPORT-ID                               
001200*           3) VIA PRC-KANAL & ORDER-ID=(DISTR, KUNDNR, KUNDRF)           
001300*           4) ENBART ORDER-ID=(DISTR, KUNDNR, KUNDRF)                    
001400*              PRINTER FÖR PU & PLE MÅSTE FYLLAS I                        
001500*        LÄGGER UPP ORDERDELAR PÅ PLOCKSATSREGISTER (WDR4)                
001600*                                                                         
001700*        PROGRAMMET ÄR EN UPPDATERINGS-MPP                                
001800*        PROGRAMMET UPPDATERAR WLORQA (WDQ3)                              
001900*                              WLXXKQ (WDR4)                              
002000*        PROGRAMMET LÄSER      WLXXKH (WDR1)                              
002100*                              WLORQI (WDQ2)                              
002200*                                                                         
002300*    E-TRACKER:7898645 ADDITION OF NEW FIELDS TO WDGX4002                 
002400*                      4002-IDMSG3IV 4002-IDSNO3IV 4002-ADDISPXTRA        
002500*                                                                         
002600*                                                                         
002700*    INDATA.                                                              
002800*        TRANSAKTION: W4T353 W4T353X                                      
002900*        MID:         W4I35301                                            
003000*                                                                         
003100*    UTDATA.                                                              
003200*        MOD:         W4O35301                                            
003300                                                                          
003400     SKIP3                                                                
003500 ENVIRONMENT DIVISION.                                                    
003600     EJECT                                                                
003700 DATA DIVISION.                                                           
003800 WORKING-STORAGE SECTION.                                                 
003900                                                                          
004000*    -- CHECKED BY WY2000                                                 
004100 77  IDPGM                       PIC X(08)   VALUE 'W4035300'.            
004200                                                                          
004300 77  JA                          PIC X       VALUE 'J'.                   
004400 77  YES                         PIC X       VALUE 'Y'.                   
004500 77  NEJ                         PIC X       VALUE 'N'.                   
004600 77  WS-IDDC                     PIC X(2)    VALUE SPACE.                 
004700 77  FELTEXT                     PIC X(32).                               
004800                                                                          
004900 77  IX1                         PIC S9(9)  VALUE +0    COMP SYNC.        
005000 77  PLOCK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
005100 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
005200 77  ANT-PLKSATS                 PIC S9(9)  VALUE +0    COMP SYNC.        
005300 77  ANT-ODEL                    PIC S9(9)  VALUE +0    COMP SYNC.        
005400 77  MAX-MOD-LAENGD              PIC S9(4)  VALUE +180  COMP SYNC.        
005500 77  MAX-ANTPLK                  PIC S9(3)  VALUE +25.                    
005600                                                                          
005700 01      WS-KLOCKAN.                                                      
005800   03    WS-TIHHMMSS             PIC 9(6).                                
005900   03    FILLER                  PIC X(2).                                
006000                                                                          
006100 01      WS-KLOCKAN-LOK.                                                  
006200   03    WS-TIHHMMSS-LOK         PIC 9(6).                                
006300   03    FILLER                  PIC X(2).                                
006400                                                                          
006500 77      WS-PRC-KVORDER          PIC S9(7)      COMP-3 VALUE 0.           
006600 77      WS-PRC-KVRADER          PIC S9(5)      COMP-3 VALUE 0.           
006700 77      WS-PRC-VKORDNTO         PIC S9(6)V9(1) COMP-3 VALUE 0.           
006800 77      WS-PRC-VLORDNTO         PIC S9(4)V9(3) COMP-3 VALUE 0.           
006900 77      WS-PRC-KVPLSRAD         PIC S9(5)      COMP-3 VALUE 0.           
007000 77      WS-PRC-VKPLSNTO         PIC S9(6)V9(1) COMP-3 VALUE 0.           
007100 77      WS-PRC-VLPLSNTO         PIC S9(4)V9(3) COMP-3 VALUE 0.           
007200 77      WS-PRC-SPLITGRANS       PIC S9(7)V9(3) COMP-3.                   
007300 77      WS-PRC-RESPLIT          PIC S9(1)V9(2) COMP-3.                   
007400                                                                          
007500 77      WS-KVORDER              PIC S9(7)      COMP-3 VALUE 0.           
007600 77      WS-KVRADER              PIC S9(5)      COMP-3 VALUE 0.           
007700 77      WS-VKORDNTO             PIC S9(6)V9(1) COMP-3 VALUE 0.           
007800 77      WS-VLORDNTO             PIC S9(4)V9(3) COMP-3 VALUE 0.           
007900 77      WS-SPLITGRANS           PIC S9(7)V9(3) COMP-3.                   
008000 77      WS-SPLITREST            PIC S9(7)V9(3) COMP-3.                   
008100 77      WS-RESPLIT              PIC S9(1)V9(2) COMP-3.                   
008200 77      WS-PROC-RAD             PIC S9(8)V9    COMP-3.                   
008300 77      WS-PROC-VIKT            PIC S9(8)V9    COMP-3.                   
008400 77      WS-PROC-VOLYM           PIC S9(8)V9    COMP-3.                   
008500 77      WS-ANTSPLIT             PIC  9(8)V99.                            
008600 77      WS-KDSORT               PIC  X(2)      VALUE SPACE.              
008700 77      WS-SPAR-IDPRCVAR        PIC  X(1).                               
008800 77      WS-PRCTYP               PIC  X(1)      VALUE SPACE.              
008900     88  PRC-DAG                 VALUE '1'.                               
009000     88  PRC-BULK                VALUE '2'.                               
009100                                                                          
009200 77      WS-DATUM                PIC 9(6).                                
009300 77      WS-DATUM-LOK            PIC 9(6).                                
009400                                                                          
009500 01      MED-ANTPLOCK.                                                    
009600   03    MED-PLKSATS             PIC ZZ9.                                 
009700   03    FILLER                  PIC X(1) VALUE SPACE.                    
009800   03    MED-TEXT                PIC X(50).                               
009900                                                                          
010000*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
010100*                                                                         
010200 01      WS-IDPRC.                                                        
010300   03    WS-IDPRCBAS             PIC X(03).                               
010400   03    WS-IDPRCVAR             PIC X(01).                               
010500                                                                          
010600 01      WS-PLKSATS              PIC X(03).                               
010700                                                                          
010800 01      WS-BEST-PLKSATS         PIC S9(3) COMP-3.                        
010900                                                                          
011000 01      WS-IDTRP.                                                        
011100   03    WS-IDTRPLOS             PIC X(03).                               
011200   03    WS-IDTRPVAR             PIC X(02).                               
011300                                                                          
011400                                                                          
011500 01      WS-TITRPAVT-LOKAL.                                               
011600   03    WS-TIAAMMDD-LOK         PIC X(06).                               
011700   03    WS-TIAAMMDD-LOK-N REDEFINES WS-TIAAMMDD-LOK                      
011800                                 PIC 9(06).                               
011900   03    WS-TIHHMM-LOK           PIC X(04).                               
012000                                                                          
012100 01      WS-ORDERID.                                                      
012200   03    WS-IDDISTR              PIC X(04).                               
012300   03    WS-IDKUNDNR             PIC X(06).                               
012400   03    WS-IDORDNR7             PIC X(07).                               
012500                                                                          
012600 01  WS-IDPRTLST.                                                         
012700     03 WS-SYSTDEL               PIC X(1).                                
012800     03 WS-LISTTYP               PIC X(2).                                
012900     03 WS-KDPRT                 PIC X(3).                                
013000     03 FILLER                   PIC X(2)    VALUE SPACE.                 
013100                                                                          
013200 77  INDATA-SW                   PIC X       VALUE 'J'.                   
013300     88  INDATA-OK                           VALUE 'J'.                   
013400     88  INDATA-FEL                          VALUE 'N'.                   
013500                                                                          
013600 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
013700     88  NYCKLAR-OK                          VALUE 'J'.                   
013800     88  NYCKLAR-FEL                         VALUE 'N'.                   
013900                                                                          
014000 77  PRC-FEL-SW                  PIC X       VALUE 'J'.                   
014100     88  PRC-FEL                             VALUE 'N'.                   
014200                                                                          
014300 77  LDC-PRC-FEL-SW              PIC X       VALUE 'N'.                   
014400     88  LDC-PRC-FEL                         VALUE 'J'.                   
014500                                                                          
014600 77  OHUV-SW                     PIC X.                                   
014700     88  ORDERHUVUD-OK                       VALUE 'J'.                   
014800     88  ORDERHUVUD-SAKNAS                   VALUE 'N'.                   
014900                                                                          
015000 77  LAES-SW                     PIC X.                                   
015100     88  LAES-OK                             VALUE 'J'.                   
015200                                                                          
015300 77  LAES-PRC-SW                 PIC X       VALUE 'N'.                   
015400     88  LAES-PRC                            VALUE 'J'.                   
015500     88  LAES-EJ-PRC                         VALUE 'N'.                   
015600                                                                          
015700 77  PLOCKGRANS-SW               PIC X       VALUE 'N'.                   
015800     88  PLOCKGRANS                          VALUE 'J'.                   
015900                                                                          
016000 77  SPLIT-SW                    PIC X       VALUE 'N'.                   
016100     88  SPLIT                               VALUE 'J'.                   
016200     88  EJ-SPLIT                            VALUE 'N'.                   
016300                                                                          
016400 77  PRC-SW                      PIC X       VALUE 'N'.                   
016500     88  PRC-OK                              VALUE 'J'.                   
016600     88  PRC-EJ-OK                           VALUE 'N'.                   
016700                                                                          
016800 77  ORDERDEL-SW                 PIC X       VALUE 'N'.                   
016900     88  ORDERDEL-FINNS                      VALUE 'N'.                   
017000     88  ORDERDEL-SLUT                       VALUE 'J'.                   
017100                                                                          
017200 77  OMSTART-SW                  PIC X       VALUE 'N'.                   
017300     88  OMSTART                             VALUE 'J'.                   
017400                                                                          
017500 77  FORSTA-SW                   PIC X       VALUE 'J'.                   
017600     88  FORSTA-GANG                         VALUE 'J'.                   
017700                                                                          
017800 77  PRC-BRYT-SW                 PIC X       VALUE 'N'.                   
017900     88  PRC-BRYTNING                        VALUE 'J'.                   
018000                                                                          
018100 77  LAES-WAY                    PIC X.                                   
018200     88  PRC                                 VALUE '1'.                   
018300     88  TRANSPORT                           VALUE '2'.                   
018400     88  PRC-ORDERID                         VALUE '3'.                   
018500     88  ORDERID                             VALUE '4'.                   
018600                                                                          
018700 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
018800     88  EGEN-MID                            VALUE '4353'.                
018900     88  GODK-MID                            VALUE '4353'                 
019000                                                   '4293'.                
019100     EJECT                                                                
019200*                                                                         
019300*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
019400 01  GENERELLA-SUBPROGRAM.                                                
019500     03  W006PRT                 PIC X(8)    VALUE 'W006PRT '.            
019600     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
019700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
019800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
019900     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
020000     EJECT                                                                
020100*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
020200*01 -COPY WMSGINIT                                                        
020300     EJECT                                                                
020400*    --- PARAMETRAR TILL SUBPROGRAM W006PRT                               
020500                                                                          
020600*   -COPY W006PRT                                                         
020700     EJECT                                                                
020800*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
020900*   -COPY WMEDAREA                                                        
021000     EJECT                                                                
021100*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
021200*                                                                         
021300 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
021400     SKIP3                                                                
021500*01  MID -COPY W4I35301                                                   
021600     EJECT                                                                
021700 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
021800     SKIP3                                                                
021900*01  -COPY WMSGAREA                                                       
022000     EJECT                                                                
022100*    03  MOD -COPY W4O35301   -RED MSG-AREA.                              
022200     EJECT                                                                
022300 01  FILLER                      PIC X(16)   VALUE 'P-TO-P-SW1'.          
022400 01  P-TO-P-SW1.                                                          
022500     03  PTOP1-LL                PIC S9(4)   VALUE 28 COMP SYNC.          
022600     03  PTOP1-Z1                PIC  X(1)   VALUE LOW-VALUE.             
022700     03  PTOP1-Z2                PIC  X(1)   VALUE LOW-VALUE.             
022800     03  PTOP1-TRANSKOD          PIC  X(7)   VALUE 'W4T375Y'.             
022900     03  FILLER                  PIC  X(1)   VALUE SPACE.                 
023000     03  FILLER                  PIC  X(4)   VALUE '4353'.                
023100     03  PTOP1-KDMFSFOR          PIC  X(1).                               
023200*    03  -COPY W4I37501  -PRE PTOP1-                                      
023300                                                                          
023400 01  FILLER                      PIC X(16)   VALUE 'P-TO-P-SW2'.          
023500 01  P-TO-P-SW2.                                                          
023600     03  PTOP2-LL                PIC S9(4)   VALUE 187 COMP SYNC.         
023700     03  PTOP2-Z1                PIC  X(1)   VALUE LOW-VALUE.             
023800     03  PTOP2-Z2                PIC  X(1)   VALUE LOW-VALUE.             
023900     03  PTOP2-TRANSKOD          PIC  X(7)   VALUE 'W4T353U'.             
024000     03  FILLER                  PIC  X(1)   VALUE SPACE.                 
024100     03  FILLER                  PIC  X(4)   VALUE '4353'.                
024200     03  PTOP2-KDMFSFOR          PIC  X(1).                               
024300*    03  -COPY W4I35301  -PRE PTOP2-                                      
024400     EJECT                                                                
024500 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
024600                                                                          
024700*01  -COPY WMFSAREA                                                       
024800     EJECT                                                                
024900*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
025000*                                                                         
025100 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
025200     SKIP3                                                                
025300 01  NYCKLAR-TILL-DLI.                                                    
025400                                                                          
025500*----> DIREKTNYCKEL TILL ORDERHUVUD.                                      
025600                                                                          
025700     03  W-IDORDER-X.                                                     
025800         05  W-IDORDER               PIC S9(7)  COMP-3.                   
025900                                                                          
026000     03  W-IDDC-X.                                                        
026100         05  W-IDDC                  PIC X(2).                            
026200                                                                          
026300*----> DIREKTNYCKEL TILL ORDERDEL.                                        
026400                                                                          
026500     03  W-WDQ301KY-X.                                                    
026600         05  W-Q301KY-IDORDER        PIC S9(7)  COMP-3.                   
026700         05  W-Q301KY-IDDC           PIC X(2).                            
026800         05  W-Q301KY-IDPRODNR       PIC S9(7)  COMP-3.                   
026900         05  W-Q301KY-IDPLKLST       PIC S9(3)  COMP-3.                   
027000                                                                          
027100*----> DIREKTNYCKEL MIN & MAX TILL ORDERDEL.                              
027200                                                                          
027300     03  W-WDQ301KY-MIN-X.                                                
027400         05  W-Q301KY-MIN-IDORDER    PIC S9(7)  COMP-3.                   
027500         05  W-Q301KY-MIN-IDDC       PIC X(2).                            
027600         05  W-Q301KY-MIN-IDPRODNR   PIC S9(7)  COMP-3.                   
027700         05  W-Q301KY-MIN-IDPLKLST   PIC S9(3)  COMP-3.                   
027800                                                                          
027900     03  W-WDQ301KY-MAX-X.                                                
028000         05  W-Q301KY-MAX-IDORDER    PIC S9(7)  COMP-3.                   
028100         05  W-Q301KY-MAX-IDDC       PIC X(2).                            
028200         05  W-Q301KY-MAX-IDPRODNR   PIC S9(7)  COMP-3.                   
028300         05  W-Q301KY-MAX-IDPLKLST   PIC S9(3)  COMP-3.                   
028400                                                                          
028500*----> SEKUNDÄR INDEX TRANSPORT-ID TILL ORDERDEL.                         
028600                                                                          
028700     03  W-WDQ3ASEQ-MIN-X.                                                
028800         05  W-Q3ASEQ-MIN-IDDC       PIC X(2).                            
028900         05  W-Q3ASEQ-MIN-IDTRP.                                          
029000          07 W-Q3ASEQ-MIN-IDTRPLOS   PIC X(3).                            
029100          07 W-Q3ASEQ-MIN-IDTRPVAR   PIC X(2).                            
029200         05  W-Q3ASEQ-MIN-DATRPAVT.                                       
029300          07 W-Q3ASEQ-MIN-DATRPAVD   PIC 9(8).                            
029400          07 W-Q3ASEQ-MIN-TIHHMM     PIC S9(5)  COMP-3.                   
029500         05  W-Q3ASEQ-MIN-DARFS      PIC 9(12).                           
029600         05  W-Q3ASEQ-MIN-DALSTORD   PIC 9(12).                           
029700         05  FILLER                  PIC X(04).                           
029800                                                                          
029900     03      W-Q3ASEQ-MIN-IDPRC.                                          
030000         05  W-Q3ASEQ-MIN-IDPRCBAS   PIC X(3).                            
030100         05  W-Q3ASEQ-MIN-IDPRCVAR   PIC X(1).                            
030200                                                                          
030300     03  W-WDQ3ASEQ-MAX-X.                                                
030400         05  W-Q3ASEQ-MAX-IDDC       PIC X(2).                            
030500         05  W-Q3ASEQ-MAX-IDTRP.                                          
030600          07 W-Q3ASEQ-MAX-IDTRPLOS   PIC X(3).                            
030700          07 W-Q3ASEQ-MAX-IDTRPVAR   PIC X(2).                            
030800         05  W-Q3ASEQ-MAX-DATRPAVT.                                       
030900          07 W-Q3ASEQ-MAX-DATRPAVD   PIC 9(8).                            
031000          07 W-Q3ASEQ-MAX-TIHHMM     PIC S9(5)  COMP-3.                   
031100         05  W-Q3ASEQ-MAX-DARFS      PIC 9(12).                           
031200         05  W-Q3ASEQ-MAX-DALSTORD   PIC 9(12).                           
031300         05  FILLER                  PIC X(04).                           
031400                                                                          
031500     03      W-Q3ASEQ-MAX-IDPRC.                                          
031600         05  W-Q3ASEQ-MAX-IDPRCBAS   PIC X(3).                            
031700         05  W-Q3ASEQ-MAX-IDPRCVAR   PIC X(1).                            
031800                                                                          
031900*----> SEKUNDÄR INDEX PRC-KANAL TILL ORDERDEL.                            
032000                                                                          
032100     03  W-WDQ3BSEQ-MIN-X.                                                
032200         05  W-Q3BSEQ-MIN-IDDC       PIC X(2).                            
032300         05  W-Q3BSEQ-MIN-IDPRCBAS   PIC X(3).                            
032400         05  W-Q3BSEQ-MIN-DAUTSKR    PIC 9(8).                            
032500         05  W-Q3BSEQ-MIN-TIUTSTID   PIC S9(7)  COMP-3.                   
032600         05  W-Q3BSEQ-MIN-DARFS      PIC 9(12).                           
032700         05  W-Q3BSEQ-MIN-DALSTORD   PIC 9(12).                           
032800         05  FILLER                  PIC X(1).                            
032900                                                                          
033000     03      W-Q3BSEQ-MIN-IDPRCVAR   PIC X(1).                            
033100                                                                          
033200     03  W-WDQ3BSEQ-MAX-X.                                                
033300         05  W-Q3BSEQ-MAX-IDDC       PIC X(2).                            
033400         05  W-Q3BSEQ-MAX-IDPRCBAS   PIC X(3).                            
033500         05  W-Q3BSEQ-MAX-DAUTSKR    PIC 9(8).                            
033600         05  W-Q3BSEQ-MAX-TIUTSTID   PIC S9(7)  COMP-3.                   
033700         05  W-Q3BSEQ-MAX-DARFS      PIC 9(12).                           
033800         05  W-Q3BSEQ-MAX-DALSTORD   PIC 9(12).                           
033900         05  FILLER                  PIC X(1).                            
034000                                                                          
034100     03      W-Q3BSEQ-MAX-IDPRCVAR   PIC X(1).                            
034200                                                                          
034300*----> SEKUNDÄR INDEX IDGMTREF TILL ORDERHUVUD.                           
034400                                                                          
034500     03  W-WDQ2CSEQ-X.                                                    
034600         05  W-Q2CSEQ-IDDISTR        PIC S9(5)  COMP-3.                   
034700         05  W-Q2CSEQ-IDKUNDNR       PIC S9(7)  COMP-3.                   
034800         05  W-Q2CSEQ-IDKUNDRF       PIC X(10).                           
034900                                                                          
035000*----> PRC-KANALEN.                                                       
035100                                                                          
035200     03  W-4447-IDHTYP-X.                                                 
035300         05  W-4447-IDHTYP       PIC  X(04) VALUE '4447'.                 
035400         05  W-4447-IDDC         PIC  X(02).                              
035500         05  W-4447-LOW-VALUE    PIC  X(24) VALUE LOW-VALUE.              
035600                                                                          
035700     03  W-4448-IDPRC-X.                                                  
035800         05  W-4448-IDPRC        PIC  X(04).                              
035900         05  W-4448-LOW-VALUE    PIC  X(01) VALUE LOW-VALUE.              
036000                                                                          
036100*----> PLOCKSATSENS LÖPNR INOM PRCGRUPP.                                  
036200                                                                          
036300     03  W-4461-IDHTYP-X.                                                 
036400         05  W-4461-IDHTYP       PIC  X(04) VALUE '4461'.                 
036500         05  W-4461-IDDC         PIC  X(02).                              
036600         05  W-4461-LOW-VALUE    PIC  X(24) VALUE LOW-VALUE.              
036700                                                                          
036800     03  W-4462-KDPRCGRP-X       PIC  X(05).                              
036900                                                                          
037000*----> PLOCKSATS.                                                         
037100                                                                          
037200     03  W-4001-IDHTYP-X.                                                 
037300         05  W-4001-IDHTYP       PIC  X(04) VALUE '4001'.                 
037400         05  W-4001-IDPRODNR     PIC  9(07).                              
037500         05  W-4001-IDPLKLST     PIC  9(03).                              
037600         05  W-LOW-VALUE         PIC  X(16) VALUE LOW-VALUE.              
037700                                                                          
037800     03  W-IDDC-B6-X.                                                     
037900         05 W-IDDC-B6            PIC X(2).                                
038000                                                                          
038100*    --- STATUS-KOD FRÅN IMS                                              
038200                                                                          
038300 01  STATUS-WS                   PIC  X(02).                              
038400     88  SEGMENT-FINNS                       VALUE '  '.                  
038500     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
038600     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
038700     88  END-OF-DATA                         VALUE 'GB'.                  
038800     SKIP2                                                                
038900 01  GODK-STATUSKODER.                                                    
039000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
039100     SKIP3                                                                
039200 01  SSA1                        PIC X(160).                              
039300 01  SSA2                        PIC X(128).                              
039400     EJECT                                                                
039500*******************************                                           
039600*  ARBETSAREA FÖR ORDERDEL    *                                           
039700*******************************                                           
039800 01  W-ORDERDEL.                                                          
039900*    03  -COPY WDQ301     -PRE W-                                         
040000     EJECT                                                                
040100*    --- IMS FUNKTIONSKODER                                               
040200*01  -COPY W0003                                                          
040300     EJECT                                                                
040400*    ---  DLI INPUT-OUTPUT AREA                                           
040500 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
040600                                                                          
040700 01  DLI-IO-AREA.                                                         
040800     03  IO-AREA                 PIC X(300)  VALUE SPACE.                 
040900                                                                          
041000     03  WLXXKH11 REDEFINES IO-AREA.                                      
041100*        05  -COPY WDGX4448   -PRE XXKH-                                  
041200     EJECT                                                                
041300     03  WLXXKO01 REDEFINES IO-AREA.                                      
041400*        05  -COPY WDGX4461   -PRE XXKO-                                  
041500     EJECT                                                                
041600     03  WLXXKO11 REDEFINES IO-AREA.                                      
041700*        05  -COPY WDGX4462   -PRE XXKO-                                  
041800     EJECT                                                                
041900     03  WL400101 REDEFINES IO-AREA.                                      
042000*        05  -COPY WDGX4001   -PRE 4001-                                  
042100     EJECT                                                                
042200 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA1'.        
042300                                                                          
042400 01  DLI-IO-AREA1.                                                        
042500     03  WLORQA01.                                                        
042600*    05  -COPY WDQ301     -PRE ORQA-                                      
042700     EJECT                                                                
042800 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA2'.        
042900                                                                          
043000 01  DLI-IO-AREA2.                                                        
043100*    03  -COPY WDQ201     -PRE ORQI-                                      
043200*    03  -COPY WDQ212     -PRE ORQI-                                      
043300     EJECT                                                                
043400 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA3'.        
043500                                                                          
043600 01  DLI-IO-AREA3.                                                        
043700     03 WL400111.                                                         
043800*       05  -COPY WDGX4002                                                
043900                                                                          
044000 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
044100 01   DLI-IO-AREA-B601.                                                   
044200*     03  -COPY WDB601                                                    
044300     EJECT                                                                
044400 LINKAGE SECTION.                                                         
044500                                                                          
044600*01  -COPY W0009      -PRE MSG-                                           
044700     EJECT                                                                
044800*01  -COPY W0009      -PRE ALT1-                                          
044900     EJECT                                                                
045000*01  -COPY W0009      -PRE ALT2-                                          
045100     EJECT                                                                
045200*01  -COPY W0008      -PRE USEA-                                          
045300     05  FILLER                  PIC X.                                   
045400     EJECT                                                                
045500*01  -COPY W0008      -PRE ORQ-                                           
045600     05  FILLER                  PIC X.                                   
045700     EJECT                                                                
045800*01  -COPY W0008      -PRE ORQA-                                          
045900     05  FILLER                  PIC X.                                   
046000     EJECT                                                                
046100*01  -COPY W0008      -PRE ORQB-                                          
046200     05  FILLER                  PIC X.                                   
046300     EJECT                                                                
046400*01  -COPY W0008      -PRE ORQL-                                          
046500     05  FILLER                  PIC X.                                   
046600     EJECT                                                                
046700*01  -COPY W0008      -PRE XXKH-                                          
046800     05  FILLER                  PIC X.                                   
046900     EJECT                                                                
047000*01  -COPY W0008      -PRE 4001-                                          
047100     05  FILLER                  PIC X.                                   
047200     EJECT                                                                
047300*01  -COPY W0008      -PRE XXKO-                                          
047400     05  FILLER                  PIC X.                                   
047500     EJECT                                                                
047600*01  -COPY W0008      -PRE ORQI-                                          
047700     05  FILLER                  PIC X.                                   
047800     EJECT                                                                
047900*01  -COPY W0008      -PRE WDB6-                                          
048000     05  FILLER                  PIC X.                                   
048100     EJECT                                                                
048200 PROCEDURE DIVISION  USING MSG-PCB                                        
048300                           ALT1-PCB                                       
048400                           ALT2-PCB                                       
048500                           USEA-PCB                                       
048600                           ORQ-PCB                                        
048700                           ORQA-PCB                                       
048800                           ORQB-PCB                                       
048900                           ORQL-PCB                                       
049000                           XXKH-PCB                                       
049100                           4001-PCB                                       
049200                           XXKO-PCB                                       
049300                           ORQI-PCB                                       
049400                           WDB6-PCB.                                      
049500                                                                          
049600     ENTRY 'DLITCBL' USING MSG-PCB                                        
049700                           ALT1-PCB                                       
049800                           ALT2-PCB                                       
049900                           USEA-PCB                                       
050000                           ORQ-PCB                                        
050100                           ORQA-PCB                                       
050200                           ORQB-PCB                                       
050300                           ORQL-PCB                                       
050400                           XXKH-PCB                                       
050500                           4001-PCB                                       
050600                           XXKO-PCB                                       
050700                           ORQI-PCB                                       
050800                           WDB6-PCB.                                      
050900                                                                          
051000     PERFORM IMS-GU-MSG                                                   
051100     IF SEGMENT-FINNS                                                     
051200        PERFORM A-INIT                                                    
051300        PERFORM B-KOLLA-NYCKLAR                                           
051400        IF NYCKLAR-OK                                                     
051500*          IF MSG-SIGNON-USERID = 'R013343 '                              
051600*             PERFORM FIXA-ORDERDEL                                       
051700*          END-IF                                                         
051800           PERFORM C-KOLLA-INPUT                                          
051900           IF INDATA-OK                                                   
052000              PERFORM D-SKAPA-PLOCKSATSER                                 
052100           END-IF                                                         
052200        END-IF                                                            
052300        IF MFS-UPD-X                                                      
052400           CONTINUE                                                       
052500        ELSE                                                              
052600           IF OMSTART                                                     
052700              PERFORM E-OMSTART-W40353                                    
052800           ELSE                                                           
052900              MOVE MAX-MOD-LAENGD TO MSG-KVLL                             
053000              PERFORM IMS-ISRT-MSG                                        
053100           END-IF                                                         
053200        END-IF                                                            
053300     END-IF                                                               
053400                                                                          
053500     MOVE ZERO TO RETURN-CODE                                             
053600     GOBACK                                                               
053700     .                                                                    
053800     EJECT                                                                
053900 FIXA-ORDERDEL SECTION.                                                   
054000                                                                          
054100*    MOVE 688861  TO W-Q301KY-IDORDER                                     
054200*    MOVE 2       TO W-Q301KY-IDDC                                        
054300*    MOVE 673954  TO W-Q301KY-IDPRODNR                                    
054400*    MOVE 10      TO W-Q301KY-IDPLKLST                                    
054500*                                                                         
054600*    PERFORM IMS-GHU-ORQ-WLORQA01                                         
054700*                                                                         
054800*    MOVE 'R'     TO ORQA-ODEL-KDODELSTA                                  
054900*    MOVE  2      TO ORQA-ODEL-KVRADER                                    
055000*                    ORQA-ODEL-VKORDNTO                                   
055100*    MOVE   2     TO ORQA-ODEL-KVPACKRAD-OD                               
055200*                    ORQA-ODEL-VLORDNTO                                   
055300*                    ORQA-ODEL-SUORDV                                     
055400*                    ORQA-ODEL-SUORDV-LOC                                 
055500*                    ORQA-ODEL-SUORDV-LOCPREL                             
055600*    MOVE 920724  TO ORQA-ODEL-TIPACKN                                    
055700*    MOVE 091219  TO ORQA-ODEL-TIPACTID                                   
055800*                                                                         
055900*    PERFORM IMS-REPL-ORQ-WLORQA01                                        
056000     CONTINUE                                                             
056100     .                                                                    
056200     EJECT                                                                
056300 A-INIT SECTION.                                                          
056400                                                                          
056500     IF MSG-DUBBLA-TRANSKODER                                             
056600       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I35301                 
056700       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
056800       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
056900     ELSE                                                                 
057000       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I35301                  
057100       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
057200       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
057300     END-IF                                                               
057400                                                                          
057500     MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                             
057600     MOVE MSG-IDPFK TO MFS-IDPFK                                          
057700     MOVE MFS-IDTRANS TO W-IDTRANS                                        
057800                                                                          
057900     MOVE LOW-VALUE TO MSG-AREA                                           
058000     MOVE 'W4O353N1' TO MFS-IDMOD                                         
058100     MOVE '4353' TO MOD-IDTRANS                                           
058200     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
058300                                                                          
058400     IF ENGLISH-TEXT                                                      
058500        MOVE +2 TO SPRAK-IX                                               
058600        MOVE 'GB ' TO MED-IDSKYLT                                         
058700     ELSE                                                                 
058800        MOVE +1 TO SPRAK-IX                                               
058900        MOVE 'S  ' TO MED-IDSKYLT                                         
059000     END-IF                                                               
059100                                                                          
059200     IF MFS-UPD-X                                                         
059300        MOVE 'S  '             TO MED-IDSKYLT                             
059400     ELSE                                                                 
059500        IF NOT EGEN-MID                                                   
059600           MOVE SPACE TO MFS-KDTRTYP                                      
059700           MOVE '7' TO MFS-IDPFK                                          
059800        END-IF                                                            
059900     END-IF                                                               
060000                                                                          
060100     MOVE NEJ          TO PTOP1-MID-FLSVAR                                
060200     MOVE MFS-KDMFSFOR TO PTOP1-KDMFSFOR                                  
060300                          PTOP2-KDMFSFOR                                  
060400                                                                          
060500     ACCEPT WS-DATUM   FROM DATE                                          
060600     ACCEPT WS-KLOCKAN FROM TIME                                          
060700     MOVE NEJ      TO MOD-MIXAT                                           
060800     .                                                                    
060900     EJECT                                                                
061000 B-KOLLA-NYCKLAR SECTION.                                                 
061100                                                                          
061200     IF MFS-UPD-X                                                         
061300*UPDATING TRANSACTION FROM ANOTHER PROGRAM.                               
061400       CONTINUE                                                           
061500     ELSE                                                                 
061600       MOVE ALL '+'         TO MSGI-WMSGINIT                              
061700       MOVE '001'           TO MSGI-KDCALL                                
061800       MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                              
061900       MOVE '4353'            TO MSGI-IDTRANS                             
062000       MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                        
062100       CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                         
062200     END-IF                                                               
062300                                                                          
062400     MOVE JA  TO NYCKLAR-SW                                               
062500     MOVE 0   TO LAES-WAY                                                 
062600                                                                          
062700     MOVE LOW-VALUE  TO W-WDQ3ASEQ-MIN-X                                  
062800                        W-WDQ3BSEQ-MIN-X                                  
062900                        W-WDQ301KY-MIN-X                                  
063000                        W-Q3ASEQ-MIN-IDPRC                                
063100                        W-Q3BSEQ-MIN-IDPRCVAR                             
063200                                                                          
063300     MOVE HIGH-VALUE TO W-WDQ3ASEQ-MAX-X                                  
063400                        W-WDQ3BSEQ-MAX-X                                  
063500                        W-WDQ301KY-MAX-X                                  
063600                        W-Q3ASEQ-MAX-IDPRC                                
063700                        W-Q3BSEQ-MAX-IDPRCVAR                             
063800                                                                          
063900     MOVE MFS-RENSA-FAELT TO MOD-IDPRC-IN                                 
064000                             MOD-IDPLKLST-IN                              
064100                             MOD-IDDC-IN                                  
064200                             MOD-IDTRP-IN                                 
064300                             MOD-TIAAMMDD-IN                              
064400                             MOD-TIHHMM-IN                                
064500                             MOD-IDDISTR-IN                               
064600                             MOD-IDKUNDNR-IN                              
064700                             MOD-IDORDNR7-IN                              
064800                                                                          
064900     PERFORM BE-KOLLA-IDDC                                                
065000                                                                          
065100     MOVE '011'                TO MSGI-KDCALL                             
065200     MOVE MSG-SIGNON-USERID    TO MSGI-IDUSER                             
065300                                  MSGI-IDLTERM-USER                       
065400     MOVE WS-DATUM             TO MSGI-TILOKDAT                           
065500     MOVE WS-TIHHMMSS(1:4)     TO MSGI-TILOKTID                           
065600     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
065700     MOVE MSGI-TILOKDAT        TO WS-DATUM-LOK                            
065800     MOVE WS-TIHHMMSS          TO WS-TIHHMMSS-LOK                         
065900     MOVE MSGI-TILOKTID        TO WS-TIHHMMSS-LOK(1:4)                    
066000                                                                          
066100     PERFORM BA-KOLLA-PRC                                                 
066200     PERFORM BB-KOLLA-TRANSPORT                                           
066300     PERFORM BC-KOLLA-ORDERID                                             
066400     PERFORM BD-KOLLA-ANTAL-PLOCKSATSER                                   
066500                                                                          
066600     IF NOT GODK-MID                                                      
066700        OR                                                                
066800        LAES-WAY = 0                                                      
066900        OR                                                                
067000       (LAES-WAY = 3 AND WS-IDPRCVAR = SPACE)                             
067100        MOVE NEJ TO NYCKLAR-SW                                            
067200     END-IF                                                               
067300                                                                          
067400     IF NYCKLAR-OK                                                        
067500        PERFORM BE-KOLLA-OMSTART                                          
067600     END-IF                                                               
067700                                                                          
067800     IF GODK-MID OR NYCKLAR-OK                                            
067900        MOVE WS-IDPRC        TO MOD-IDPRC-UT                              
068000        MOVE WS-IDDC         TO MOD-IDDC-UT                               
068100        MOVE WS-PLKSATS      TO MOD-IDPLKLST-UT                           
068200        MOVE WS-IDTRP        TO MOD-IDTRP-UT                              
068300                                                                          
068400        MOVE WS-TIAAMMDD-LOK TO MOD-TIAAMMDD-UT                           
068500                                                                          
068600        MOVE WS-TIHHMM-LOK   TO MOD-TIHHMM-UT                             
068700                                                                          
068800        MOVE WS-IDDISTR      TO MOD-IDDISTR-UT                            
068900        MOVE WS-IDKUNDNR     TO MOD-IDKUNDNR-UT                           
069000        MOVE WS-IDORDNR7     TO MOD-IDORDNR7-UT                           
069100        INSPECT MOD-IDPRC-UT    REPLACING LEADING ZERO BY SPACE           
069200        INSPECT MOD-IDPLKLST-UT REPLACING LEADING ZERO BY SPACE           
069300        INSPECT MOD-IDTRP-UT    REPLACING LEADING ZERO BY SPACE           
069400        INSPECT MOD-TIAAMMDD-UT REPLACING LEADING ZERO BY SPACE           
069500        INSPECT MOD-TIHHMM-UT   REPLACING LEADING ZERO BY SPACE           
069600        INSPECT MOD-IDDISTR-UT  REPLACING LEADING ZERO BY SPACE           
069700        INSPECT MOD-IDKUNDNR-UT REPLACING LEADING ZERO BY SPACE           
069800        INSPECT MOD-IDORDNR7-UT REPLACING LEADING ZERO BY SPACE           
069900     ELSE                                                                 
070000        MOVE MFS-RENSA-FAELT TO MOD-IDPRC-UT                              
070100                                MOD-IDDC-UT                               
070200                                MOD-IDPLKLST-UT                           
070300                                MOD-IDTRP-UT                              
070400                                MOD-TIAAMMDD-UT                           
070500                                MOD-TIHHMM-UT                             
070600                                MOD-IDDISTR-UT                            
070700                                MOD-IDKUNDNR-UT                           
070800                                MOD-IDORDNR7-UT                           
070900     END-IF                                                               
071000                                                                          
071100     IF NYCKLAR-FEL                                                       
071200        IF GODK-MID                                                       
071300          EVALUATE TRUE                                                   
071400          WHEN LDC-PRC-FEL                                                
071500             MOVE '338' TO MED-IDMFSFEL                                   
071600          WHEN PRC-FEL                                                    
071700             MOVE '324' TO MED-IDMFSFEL                                   
071800          WHEN OTHER                                                      
071900             MOVE '401' TO MED-IDMFSFEL                                   
072000          END-EVALUATE                                                    
072100          PERFORM S06-FEL-MEDDELANDE                                      
072200          PERFORM MFS-RENSA-FAELT-UT                                      
072300        END-IF                                                            
072400     END-IF                                                               
072500     .                                                                    
072600     EJECT                                                                
072700 BE-KOLLA-IDDC  SECTION.                                                  
072800                                                                          
072900                                                                          
073000     IF MFS-UPD-X                                                         
073100*UPDATING TRANSACTION FROM ANOTHER PROGRAM.                               
073200                                                                          
073300       IF MID-IDDC-IN = ALL '+'                                           
073400         IF MID-IDDC-UT = SPACE                                           
073500           MOVE NEJ                   TO NYCKLAR-SW                       
073600         ELSE                                                             
073700           MOVE MID-IDDC-UT           TO WS-IDDC                          
073800         END-IF                                                           
073900       ELSE                                                               
074000          MOVE MID-IDDC-IN TO WS-IDDC                                     
074100       END-IF                                                             
074200     ELSE                                                                 
074300                                                                          
074400       MOVE MSGI-IDDC               TO WS-IDDC                            
074500     END-IF                                                               
074600                                                                          
074700     IF WS-IDDC > SPACE                                                   
074800       CONTINUE                                                           
074900     ELSE                                                                 
075000       MOVE NEJ                     TO NYCKLAR-SW                         
075100     END-IF                                                               
075200                                                                          
075300     IF NYCKLAR-OK                                                        
075400       MOVE WS-IDDC                 TO W-IDDC                             
075500                                       W-Q3ASEQ-MIN-IDDC                  
075600                                       W-Q3ASEQ-MAX-IDDC                  
075700                                       W-Q3BSEQ-MIN-IDDC                  
075800                                       W-Q3BSEQ-MAX-IDDC                  
075900                                                                          
076000     END-IF                                                               
076100     .                                                                    
076200     EJECT                                                                
076300 BA-KOLLA-PRC SECTION.                                                    
076400     MOVE NEJ          TO LDC-PRC-FEL-SW                                  
076500                                                                          
076600     IF MID-IDPRC-IN = ALL '+'                                            
076700        MOVE MID-IDPRC-UT TO WS-IDPRC                                     
076800     ELSE                                                                 
076900        MOVE MID-IDPRC-IN TO WS-IDPRC                                     
077000     END-IF                                                               
077100                                                                          
077200     MOVE WS-IDDC         TO W-IDDC-B6                                    
077300     PERFORM IMS-GU-WDB601                                                
077400                                                                          
077500     INSPECT WS-IDPRC REPLACING LEADING SPACE BY ZERO                     
077600                                                                          
077700     IF WS-IDPRCBAS NUMERIC AND WS-IDPRCBAS > ZERO                        
077800*PRC 9998 = DIREKTLEV UTSKRIFT. SKALL SKRIVAS UT AV PGM 4695.             
077900       IF WS-IDPRCBAS = '999' AND WS-IDPRCVAR = '8'                       
078000          MOVE NEJ        TO NYCKLAR-SW                                   
078100                             PRC-FEL-SW                                   
078200       ELSE                                                               
078300         MOVE WS-IDPRCBAS TO W-Q3ASEQ-MIN-IDPRCBAS                        
078400                              W-Q3ASEQ-MAX-IDPRCBAS                       
078500                              W-Q3BSEQ-MIN-IDPRCBAS                       
078600                              W-Q3BSEQ-MAX-IDPRCBAS                       
078700         MOVE 1           TO LAES-WAY                                     
078800       END-IF                                                             
078900     END-IF                                                               
079000                                                                          
079100     IF WS-IDPRCVAR NOT = SPACE                                           
079200        MOVE WS-IDPRCVAR  TO W-Q3ASEQ-MIN-IDPRCVAR                        
079300                             W-Q3ASEQ-MAX-IDPRCVAR                        
079400                             W-Q3BSEQ-MIN-IDPRCVAR                        
079500                             W-Q3BSEQ-MAX-IDPRCVAR                        
079600     END-IF                                                               
079700     .                                                                    
079800     EJECT                                                                
079900 BB-KOLLA-TRANSPORT SECTION.                                              
080000                                                                          
080100     IF MID-IDTRP-IN = ALL '+'                                            
080200        MOVE MID-IDTRP-UT TO WS-IDTRP                                     
080300     ELSE                                                                 
080400        MOVE MID-IDTRP-IN TO WS-IDTRP                                     
080500     END-IF                                                               
080600                                                                          
080700     IF WS-IDTRP NOT = SPACE                                              
080800        IF WS-IDTRPLOS NOT NUMERIC                                        
080900           MOVE NEJ TO NYCKLAR-SW                                         
081000        END-IF                                                            
081100     END-IF                                                               
081200                                                                          
081300     IF WS-IDTRPLOS NUMERIC                                               
081400        IF LAES-WAY = 1                                                   
081500           MOVE 2      TO LAES-WAY                                        
081600        END-IF                                                            
081700        IF WS-IDTRP NUMERIC                                               
081800           MOVE WS-IDTRP  TO W-Q3ASEQ-MIN-IDTRP                           
081900                             W-Q3ASEQ-MAX-IDTRP                           
082000        ELSE                                                              
082100           MOVE WS-IDTRPLOS TO W-Q3ASEQ-MIN-IDTRPLOS                      
082200                               W-Q3ASEQ-MAX-IDTRPLOS                      
082300           MOVE SPACE       TO W-Q3ASEQ-MIN-IDTRPVAR                      
082400           MOVE '99'        TO W-Q3ASEQ-MAX-IDTRPVAR                      
082500        END-IF                                                            
082600     END-IF                                                               
082700                                                                          
082800     IF MID-TIAAMMDD-IN = ALL '+'                                         
082900        MOVE MID-TIAAMMDD-UT TO WS-TIAAMMDD-LOK                           
083000     ELSE                                                                 
083100        MOVE MID-TIAAMMDD-IN TO WS-TIAAMMDD-LOK                           
083200     END-IF                                                               
083300                                                                          
083400     IF MID-TIHHMM-IN = ALL '+'                                           
083500        MOVE MID-TIHHMM-UT TO WS-TIHHMM-LOK                               
083600     ELSE                                                                 
083700        MOVE MID-TIHHMM-IN TO WS-TIHHMM-LOK                               
083800     END-IF                                                               
083900                                                                          
084000     INSPECT WS-TIAAMMDD-LOK REPLACING LEADING SPACE BY ZERO              
084100                                                                          
084200     INSPECT WS-TIHHMM-LOK REPLACING LEADING SPACE BY ZERO                
084300                                                                          
084400     IF  WS-TIAAMMDD-LOK NUMERIC                                          
084500     AND WS-TIHHMM-LOK NUMERIC                                            
084600        IF  WS-TIAAMMDD-LOK > ZERO                                        
084700        AND WS-TIHHMM-LOK > ZERO                                          
084800            MOVE '011'              TO MSGI-KDCALL                        
084900            MOVE MSG-SIGNON-USERID  TO MSGI-IDUSER                        
085000                                       MSGI-IDLTERM-USER                  
085100            MOVE WS-TIAAMMDD-LOK    TO MSGI-TILOKDAT                      
085200            MOVE WS-TIHHMM-LOK      TO MSGI-TILOKTID                      
085300            CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                    
085400            IF  MSGI-KDSVAR NOT = SPACE                                   
085500                MOVE NEJ       TO NYCKLAR-SW                              
085600                MOVE ZERO      TO WS-TIAAMMDD-LOK                         
085700                                  WS-TIHHMM-LOK                           
085800            ELSE                                                          
085900                MOVE MSGI-TILOKDAT    TO WS-TIAAMMDD-LOK                  
086000                MOVE MSGI-TILOKTID    TO WS-TIHHMM-LOK                    
086100            END-IF                                                        
086200        ELSE                                                              
086300            MOVE ZERO      TO WS-TIAAMMDD-LOK                             
086400                              WS-TIHHMM-LOK                               
086500        END-IF                                                            
086600     ELSE                                                                 
086700        MOVE ZERO      TO WS-TIAAMMDD-LOK                                 
086800                          WS-TIHHMM-LOK                                   
086900     END-IF                                                               
087000                                                                          
087100     IF  WS-TIAAMMDD-LOK NUMERIC                                          
087200                                                                          
087300        IF WS-TIAAMMDD-LOK > ZERO                                         
087400           IF LAES-WAY = 2                                                
087500             MOVE WS-TIAAMMDD-LOK TO W-Q3ASEQ-MIN-DATRPAVD                
087600                                     W-Q3ASEQ-MAX-DATRPAVD                
087700             IF WS-TIAAMMDD-LOK NOT = ZERO                                
087800               IF WS-TIAAMMDD-LOK < 500000                                
087900                 MOVE 20        TO W-Q3ASEQ-MIN-DATRPAVD (1:2)            
088000                                   W-Q3ASEQ-MAX-DATRPAVD (1:2)            
088100               ELSE                                                       
088200                 IF WS-TIAAMMDD-LOK < 999999                              
088300                   MOVE 19       TO W-Q3ASEQ-MIN-DATRPAVD (1:2)           
088400                                    W-Q3ASEQ-MAX-DATRPAVD (1:2)           
088500                 ELSE                                                     
088600                   MOVE 99999999  TO W-Q3ASEQ-MIN-DATRPAVD                
088700                                     W-Q3ASEQ-MAX-DATRPAVD                
088800                 END-IF                                                   
088900               END-IF                                                     
089000             END-IF                                                       
089100           ELSE                                                           
089200              MOVE NEJ         TO NYCKLAR-SW                              
089300           END-IF                                                         
089400        END-IF                                                            
089500     ELSE                                                                 
089600        MOVE NEJ         TO NYCKLAR-SW                                    
089700     END-IF                                                               
089800                                                                          
089900     IF WS-TIHHMM-LOK NUMERIC                                             
090000        IF WS-TIHHMM-LOK > ZERO                                           
090100           IF  WS-TIAAMMDD-LOK NUMERIC                                    
090200           AND WS-TIAAMMDD-LOK > ZERO                                     
090300                                                                          
090400              IF LAES-WAY = 2                                             
090500                 MOVE WS-TIHHMM-LOK TO W-Q3ASEQ-MIN-TIHHMM                
090600                                       W-Q3ASEQ-MAX-TIHHMM                
090700              ELSE                                                        
090800                 MOVE NEJ         TO NYCKLAR-SW                           
090900              END-IF                                                      
091000           ELSE                                                           
091100              MOVE NEJ            TO NYCKLAR-SW                           
091200           END-IF                                                         
091300        END-IF                                                            
091400     ELSE                                                                 
091500        MOVE NEJ                  TO NYCKLAR-SW                           
091600     END-IF                                                               
091700     .                                                                    
091800     EJECT                                                                
091900 BC-KOLLA-ORDERID SECTION.                                                
092000                                                                          
092100     IF MID-IDDISTR-IN = ALL '+'                                          
092200        MOVE MID-IDDISTR-UT TO WS-IDDISTR                                 
092300     ELSE                                                                 
092400        MOVE MID-IDDISTR-IN TO WS-IDDISTR                                 
092500     END-IF                                                               
092600                                                                          
092700     INSPECT WS-IDDISTR REPLACING LEADING SPACE BY ZERO                   
092800                                                                          
092900     IF MID-IDKUNDNR-IN = ALL '+'                                         
093000        MOVE MID-IDKUNDNR-UT TO WS-IDKUNDNR                               
093100     ELSE                                                                 
093200        MOVE MID-IDKUNDNR-IN TO WS-IDKUNDNR                               
093300     END-IF                                                               
093400                                                                          
093500     INSPECT WS-IDKUNDNR REPLACING LEADING SPACE BY ZERO                  
093600                                                                          
093700     IF MID-IDORDNR7-IN = ALL '+'                                         
093800        MOVE MID-IDORDNR7-UT TO WS-IDORDNR7                               
093900     ELSE                                                                 
094000        MOVE MID-IDORDNR7-IN TO WS-IDORDNR7                               
094100     END-IF                                                               
094200                                                                          
094300     INSPECT WS-IDORDNR7 REPLACING LEADING SPACE BY ZERO                  
094400                                                                          
094500     IF WS-ORDERID NUMERIC AND WS-ORDERID > ZERO                          
094600        MOVE WS-IDDISTR  TO W-Q2CSEQ-IDDISTR                              
094700        MOVE WS-IDKUNDNR TO W-Q2CSEQ-IDKUNDNR                             
094800        MOVE WS-IDORDNR7 TO W-Q2CSEQ-IDKUNDRF                             
094900        IF LAES-WAY = 1 OR 2                                              
095000           MOVE 3      TO LAES-WAY                                        
095100        ELSE                                                              
095200           MOVE 4      TO LAES-WAY                                        
095300        END-IF                                                            
095400     END-IF                                                               
095500     .                                                                    
095600     EJECT                                                                
095700 BD-KOLLA-ANTAL-PLOCKSATSER SECTION.                                      
095800                                                                          
095900     MOVE ZERO TO WS-PLKSATS                                              
096000                                                                          
096100     IF PRC OR TRANSPORT                                                  
096200        IF MID-IDPLKLST-IN = ALL '+'                                      
096300           MOVE NEJ   TO NYCKLAR-SW                                       
096400        ELSE                                                              
096500           MOVE MID-IDPLKLST-IN TO WS-PLKSATS                             
096600        END-IF                                                            
096700     ELSE                                                                 
096800        IF PRC-ORDERID OR ORDERID                                         
096900           MOVE MAX-ANTPLK TO WS-PLKSATS                                  
097000        END-IF                                                            
097100     END-IF                                                               
097200                                                                          
097300     INSPECT WS-PLKSATS REPLACING LEADING SPACE BY ZERO                   
097400                                                                          
097500     IF WS-PLKSATS NOT NUMERIC                                            
097600        MOVE NEJ TO NYCKLAR-SW                                            
097700     ELSE                                                                 
097800        IF WS-PLKSATS > MAX-ANTPLK                                        
097900           MOVE MAX-ANTPLK TO WS-PLKSATS                                  
098000        END-IF                                                            
098100        MOVE WS-PLKSATS    TO WS-BEST-PLKSATS                             
098200     END-IF                                                               
098300     .                                                                    
098400     EJECT                                                                
098500 BE-KOLLA-OMSTART SECTION.                                                
098600                                                                          
098700     IF MID-WDQ3BSEQ NOT = ALL '+'                                        
098800        IF MID-ANTPLK     NOT = ALL '+'                                   
098900          MOVE MID-ANTPLK        TO ANT-PLKSATS                           
099000        END-IF                                                            
099100        IF MID-IDDC-BSEQ       NOT = ALL '+'                              
099200          MOVE MID-IDDC-BSEQ     TO W-Q3BSEQ-MIN-IDDC                     
099300        END-IF                                                            
099400        IF MID-IDPRCBAS-BSEQ   NOT = ALL '+'                              
099500          MOVE MID-IDPRCBAS-BSEQ TO W-Q3BSEQ-MIN-IDPRCBAS                 
099600        END-IF                                                            
099700        IF MID-TIUTSKR-BSEQ    NOT = ALL '+'                              
099800          MOVE MID-TIUTSKR-BSEQ  TO W-Q3BSEQ-MIN-DAUTSKR                  
099900          IF MID-TIUTSKR-BSEQ NOT = ZERO                                  
100000            IF MID-TIUTSKR-BSEQ < 500000                                  
100100              MOVE 20            TO W-Q3BSEQ-MIN-DAUTSKR (1:2)            
100200            ELSE                                                          
100300              IF MID-TIUTSKR-BSEQ < 999999                                
100400                MOVE 19          TO W-Q3BSEQ-MIN-DAUTSKR (1:2)            
100500              ELSE                                                        
100600                MOVE 99999999    TO W-Q3BSEQ-MIN-DAUTSKR                  
100700              END-IF                                                      
100800            END-IF                                                        
100900          END-IF                                                          
101000        END-IF                                                            
101100        IF MID-TIUTSTID-BSEQ   NOT = ALL '+'                              
101200          MOVE MID-TIUTSTID-BSEQ TO W-Q3BSEQ-MIN-TIUTSTID                 
101300        END-IF                                                            
101400        IF MID-TIRFS-BSEQ      NOT = ALL '+'                              
101500          MOVE MID-TIRFS-BSEQ    TO W-Q3BSEQ-MIN-DARFS                    
101600          IF MID-TIRFS-BSEQ NOT = ZERO                                    
101700            IF MID-TIRFS-BSEQ < 5000000000                                
101800              MOVE 20            TO W-Q3BSEQ-MIN-DARFS (1:2)              
101900            ELSE                                                          
102000              IF MID-TIRFS-BSEQ < 9999999999                              
102100                MOVE 19          TO W-Q3BSEQ-MIN-DARFS (1:2)              
102200              ELSE                                                        
102300                MOVE 999999999999 TO W-Q3BSEQ-MIN-DARFS                   
102400              END-IF                                                      
102500            END-IF                                                        
102600          END-IF                                                          
102700        END-IF                                                            
102800        IF MID-TILST-O-BSEQ    NOT = ALL '+'                              
102900          MOVE MID-TILST-O-BSEQ  TO W-Q3BSEQ-MIN-DALSTORD                 
103000          IF MID-TILST-O-BSEQ NOT = ZERO                                  
103100            IF MID-TILST-O-BSEQ < 5000000000                              
103200              MOVE 20            TO W-Q3BSEQ-MIN-DALSTORD (1:2)           
103300            ELSE                                                          
103400              IF MID-TILST-O-BSEQ < 9999999999                            
103500                MOVE 19          TO W-Q3BSEQ-MIN-DALSTORD (1:2)           
103600              ELSE                                                        
103700                MOVE 999999999999 TO W-Q3BSEQ-MIN-DALSTORD                
103800              END-IF                                                      
103900            END-IF                                                        
104000          END-IF                                                          
104100        END-IF                                                            
104200        IF MID-IDPRCVAR-BSEQ   NOT = ALL '+'                              
104300          MOVE MID-IDPRCVAR-BSEQ TO W-Q3BSEQ-MIN-IDPRCVAR                 
104400        END-IF                                                            
104500     ELSE                                                                 
104600        IF MID-WDQ3ASEQ NOT = ALL '+'                                     
104700           IF MID-ANTPLK NOT = ALL '+'                                    
104800             MOVE MID-ANTPLK        TO ANT-PLKSATS                        
104900           END-IF                                                         
105000           IF MID-IDDC-ASEQ     NOT = ALL '+'                             
105100             MOVE MID-IDDC-ASEQ     TO W-Q3ASEQ-MIN-IDDC                  
105200           END-IF                                                         
105300           IF MID-IDTRP-ASEQ    NOT = ALL '+'                             
105400             MOVE MID-IDTRP-ASEQ    TO W-Q3ASEQ-MIN-IDTRP                 
105500           END-IF                                                         
105600           IF MID-TIAAMMDD-ASEQ NOT = ALL '+'                             
105700             MOVE MID-TIAAMMDD-ASEQ TO W-Q3ASEQ-MIN-DATRPAVD              
105800             IF MID-TIAAMMDD-ASEQ NOT = ZERO                              
105900               IF MID-TIAAMMDD-ASEQ < 500000                              
106000                 MOVE 20          TO W-Q3ASEQ-MIN-DATRPAVD (1:2)          
106100               ELSE                                                       
106200                 IF MID-TIAAMMDD-ASEQ < 999999                            
106300                   MOVE 19        TO W-Q3ASEQ-MIN-DATRPAVD (1:2)          
106400                 ELSE                                                     
106500                   MOVE 99999999  TO W-Q3ASEQ-MIN-DATRPAVD                
106600                 END-IF                                                   
106700               END-IF                                                     
106800             END-IF                                                       
106900           END-IF                                                         
107000           IF MID-TIHHMM-ASEQ   NOT = ALL '+'                             
107100             MOVE MID-TIHHMM-ASEQ   TO W-Q3ASEQ-MIN-TIHHMM                
107200           END-IF                                                         
107300           IF MID-TIRFS-ASEQ    NOT = ALL '+'                             
107400             MOVE MID-TIRFS-ASEQ    TO W-Q3ASEQ-MIN-DARFS                 
107500             IF MID-TIRFS-ASEQ NOT = ZERO                                 
107600               IF MID-TIRFS-ASEQ < 5000000000                             
107700                 MOVE 20            TO W-Q3ASEQ-MIN-DARFS (1:2)           
107800               ELSE                                                       
107900                 IF MID-TIRFS-ASEQ < 9999999999                           
108000                   MOVE 19          TO W-Q3ASEQ-MIN-DARFS (1:2)           
108100                 ELSE                                                     
108200                   MOVE 999999999999 TO W-Q3ASEQ-MIN-DARFS                
108300                 END-IF                                                   
108400               END-IF                                                     
108500             END-IF                                                       
108600           END-IF                                                         
108700           IF MID-TILST-O-ASEQ  NOT = ALL '+'                             
108800             MOVE MID-TILST-O-ASEQ  TO W-Q3ASEQ-MIN-DALSTORD              
108900             IF MID-TILST-O-ASEQ NOT = ZERO                               
109000               IF MID-TILST-O-ASEQ < 5000000000                           
109100                 MOVE 20            TO W-Q3ASEQ-MIN-DALSTORD (1:2)        
109200               ELSE                                                       
109300                 IF MID-TILST-O-ASEQ < 9999999999                         
109400                   MOVE 19          TO W-Q3ASEQ-MIN-DALSTORD (1:2)        
109500                 ELSE                                                     
109600                   MOVE 999999999999 TO W-Q3ASEQ-MIN-DALSTORD             
109700                 END-IF                                                   
109800               END-IF                                                     
109900             END-IF                                                       
110000           END-IF                                                         
110100           IF MID-IDPRC-ASEQ    NOT = ALL '+'                             
110200             MOVE MID-IDPRC-ASEQ    TO W-Q3ASEQ-MIN-IDPRC                 
110300           END-IF                                                         
110400        END-IF                                                            
110500     END-IF                                                               
110600     .                                                                    
110700     EJECT                                                                
110800 C-KOLLA-INPUT SECTION.                                                   
110900                                                                          
111000     MOVE JA TO INDATA-SW                                                 
111100                                                                          
111200     IF MFS-UPD-X                                                         
111300        CONTINUE                                                          
111400     ELSE                                                                 
111500       IF ORDERID OR MID-KDPRT-PU NOT = ALL '+'                           
111600          MOVE '4'          TO WS-SYSTDEL                                 
111700          MOVE 'PU'         TO WS-LISTTYP                                 
111800          MOVE MID-KDPRT-PU TO WS-KDPRT                                   
111900                                                                          
112000          MOVE 1            TO PRT-KDCALL                                 
112100          MOVE WS-IDPRTLST  TO PRT-IDPRTLST                               
112200          CALL W006PRT  USING PRT-W006PRT                                 
112300                                                                          
112400          IF PRT-IDLTERM = 'SAKNAS  '                                     
112500             MOVE MFS-ALFA-FAELT-FEL TO MOD-KDPRT-PU-ATTR                 
112600             MOVE '772' TO MED-IDMFSFEL                                   
112700             MOVE NEJ   TO INDATA-SW                                      
112800          ELSE                                                            
112900             MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDPRT-PU-ATTR               
113000             MOVE MID-KDPRT-PU         TO MOD-KDPRT-PU                    
113100          END-IF                                                          
113200       END-IF                                                             
113300       IF ORDERID OR MID-KDPRT-PLE NOT = ALL '+'                          
113400          MOVE '4'           TO WS-SYSTDEL                                
113500          MOVE 'PE'          TO WS-LISTTYP                                
113600          MOVE MID-KDPRT-PLE TO WS-KDPRT                                  
113700                                                                          
113800          MOVE 1             TO PRT-KDCALL                                
113900          MOVE WS-IDPRTLST   TO PRT-IDPRTLST                              
114000          CALL W006PRT  USING PRT-W006PRT                                 
114100                                                                          
114200          IF PRT-IDLTERM = 'SAKNAS  '                                     
114300             MOVE MFS-ALFA-FAELT-FEL TO MOD-KDPRT-PLE-ATTR                
114400             MOVE '772' TO MED-IDMFSFEL                                   
114500             MOVE NEJ   TO INDATA-SW                                      
114600          ELSE                                                            
114700             MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDPRT-PLE-ATTR              
114800             MOVE MID-KDPRT-PLE        TO MOD-KDPRT-PLE                   
114900          END-IF                                                          
115000       END-IF                                                             
115100     END-IF                                                               
115200                                                                          
115300     IF MID-MIXAT = JA OR NEJ OR YES                                      
115400        IF MID-MIXAT = YES                                                
115500           MOVE JA     TO MID-MIXAT                                       
115600        END-IF                                                            
115700        MOVE MID-MIXAT TO MOD-MIXAT                                       
115800     ELSE                                                                 
115900        MOVE MFS-ALFA-FAELT-FEL TO MOD-MIXAT-ATTR                         
116000        MOVE '001' TO MED-IDMFSFEL                                        
116100        MOVE NEJ   TO INDATA-SW                                           
116200     END-IF                                                               
116300                                                                          
116400     IF INDATA-FEL                                                        
116500        PERFORM S06-FEL-MEDDELANDE                                        
116600        PERFORM MFS-ROR-EJ-FAELT-UT                                       
116700     END-IF                                                               
116800     .                                                                    
116900     EJECT                                                                
117000 D-SKAPA-PLOCKSATSER SECTION.                                             
117100                                                                          
117200     MOVE 0   TO ANT-ODEL                                                 
117300                 WS-KVORDER                                               
117400                 WS-KVRADER                                               
117500                 WS-VKORDNTO                                              
117600                 WS-VLORDNTO                                              
117700     MOVE NEJ TO ORDERDEL-SW                                              
117800                 OHUV-SW                                                  
117900     MOVE SPACE TO MED-IDMFSFEL                                           
118000                   MED-IDMFSINF                                           
118100                                                                          
118200     PERFORM S01-LAES-NASTA-ORDERDEL                                      
118300                                                                          
118400     IF ORDERDEL-FINNS                                                    
118500        PERFORM DA-HAMTA-PRC                                              
118600        IF PRC-OK                                                         
118700           IF PRC-BULK                                                    
118800              OR                                                          
118900              PRC-ORDERID                                                 
119000              OR                                                          
119100              ORDERID                                                     
119200              PERFORM DB-ANTAL-PLOCKSATSER-OVRIGA                         
119300           ELSE                                                           
119400              PERFORM DC-PLOCKSATS-DAG                                    
119500           END-IF                                                         
119600        ELSE                                                              
119700           MOVE '023' TO MED-IDMFSFEL                                     
119800           PERFORM S06-FEL-MEDDELANDE                                     
119900           PERFORM MFS-RENSA-FAELT-UT                                     
120000        END-IF                                                            
120100     ELSE                                                                 
120200        IF MED-IDMFSINF = SPACE                                           
120300           MOVE '036' TO MED-IDMFSINF                                     
120400        END-IF                                                            
120500        PERFORM S07-INFO-MEDDELANDE                                       
120600        IF ORDERID                                                        
120700           PERFORM MFS-ROR-EJ-FAELT-UT                                    
120800        ELSE                                                              
120900           PERFORM MFS-RENSA-FAELT-UT                                     
121000        END-IF                                                            
121100     END-IF                                                               
121200                                                                          
121300     IF OMSTART                                                           
121400        CONTINUE                                                          
121500     ELSE                                                                 
121600        IF ANT-PLKSATS > 0                                                
121700           MOVE ANT-PLKSATS  TO MED-PLKSATS                               
121800           IF ANT-PLKSATS = 1                                             
121900              IF ENGLISH-TEXT                                             
122000                 MOVE 'PICKING UNIT ON PRINTER QUEUE'  TO MED-TEXT        
122100              ELSE                                                        
122200                 MOVE 'PLOCKSATS KÖAD FÖR UTSKRIFT'    TO MED-TEXT        
122300              END-IF                                                      
122400           ELSE                                                           
122500              IF ENGLISH-TEXT                                             
122600                 MOVE 'PICKING UNIT ON PRINTER QUEUE'  TO MED-TEXT        
122700              ELSE                                                        
122800                 MOVE 'PLOCKSATSER KÖADE FÖR UTSKRIFT' TO MED-TEXT        
122900              END-IF                                                      
123000           END-IF                                                         
123100           MOVE MED-ANTPLOCK TO MOD-TEMFSINF                              
123200        ELSE                                                              
123300           IF MED-IDMFSINF = SPACE                                        
123400              AND                                                         
123500              MED-IDMFSFEL = SPACE                                        
123600              MOVE '039' TO MED-IDMFSINF                                  
123700              PERFORM S07-INFO-MEDDELANDE                                 
123800           END-IF                                                         
123900        END-IF                                                            
124000     END-IF                                                               
124100     .                                                                    
124200     EJECT                                                                
124300 DA-HAMTA-PRC SECTION.                                                    
124400                                                                          
124500     IF PRC OR TRANSPORT OR PRC-ORDERID                                   
124600        PERFORM S08-LAES-PRC                                              
124700        IF PRC OR TRANSPORT                                               
124800           IF PRC-BULK                                                    
124900              MOVE JA TO LAES-PRC-SW                                      
125000           END-IF                                                         
125100        END-IF                                                            
125200     END-IF                                                               
125300     .                                                                    
125400     EJECT                                                                
125500 DB-ANTAL-PLOCKSATSER-OVRIGA SECTION.                                     
125600                                                                          
125700     MOVE 1 TO PLOCK-IX                                                   
125800                                                                          
125900     PERFORM UNTIL PLOCK-IX > WS-BEST-PLKSATS                             
126000        MOVE NEJ TO PLOCKGRANS-SW                                         
126100                    SPLIT-SW                                              
126200        PERFORM S09-INIT-PLOCKSATSREG                                     
126300        ADD 1                  TO WS-KVORDER                              
126400        ADD ORQA-ODEL-KVRADER  TO WS-KVRADER                              
126500        ADD ORQA-ODEL-VKORDNTO TO WS-VKORDNTO                             
126600        ADD ORQA-ODEL-VLORDNTO TO WS-VLORDNTO                             
126700        PERFORM S10-KOLLA-PLOCKGRANS                                      
126800        PERFORM S11-SPARA-I-PLOCKSATSREG                                  
126900        PERFORM S12-UPPDAT-ORDERDEL                                       
127000        ADD 1                  TO PLOCK-IX                                
127100        ADD 1                  TO ANT-PLKSATS                             
127200        PERFORM DBA-LAGG-UPP-PLOCKSATS-OVRIGA                             
127300        MOVE 0                 TO WS-KVORDER                              
127400                                  WS-KVRADER                              
127500                                  WS-VKORDNTO                             
127600                                  WS-VLORDNTO                             
127700        IF PRC-ORDERID                                                    
127800           MOVE 999 TO PLOCK-IX                                           
127900        ELSE                                                              
128000           PERFORM S01-LAES-NASTA-ORDERDEL                                
128100        END-IF                                                            
128200                                                                          
128300        IF ORDERDEL-SLUT                                                  
128400           OR                                                             
128500           PRC-EJ-OK                                                      
128600           MOVE 999 TO PLOCK-IX                                           
128700        END-IF                                                            
128800     END-PERFORM                                                          
128900     .                                                                    
129000     EJECT                                                                
129100 DBA-LAGG-UPP-PLOCKSATS-OVRIGA SECTION.                                   
129200                                                                          
129300     IF MID-KDPRT-PU NOT = ALL '+'                                        
129400        MOVE MID-KDPRT-PU  TO 4002-KDPRT-PU                               
129500     END-IF                                                               
129600     IF MID-KDPRT-PLE NOT = ALL '+'                                       
129700        MOVE MID-KDPRT-PLE TO 4002-KDPRT-PLE                              
129800     END-IF                                                               
129900     MOVE 4002-IDPRODNR (1) TO W-4001-IDPRODNR                            
130000                               PTOP1-MID-IDPRODNR                         
130100     MOVE 4002-IDPLKLST (1) TO W-4001-IDPLKLST                            
130200                               PTOP1-MID-IDPLKLST                         
130300     PERFORM S13-HAMTA-PLOCKSATSNR                                        
130400     IF SPLIT                                                             
130500        PERFORM DBAA-HAMTA-SPLITGRANS                                     
130600     END-IF                                                               
130700     MOVE ZERO             TO 4002-IXHEL                                  
130800     PERFORM S15-ISRT-PLOCKSATS                                           
130900     IF ANT-PLKSATS = 1                                                   
131000        PERFORM IMS-ISRT-MSG-ALT1                                         
131100     ELSE                                                                 
131200        PERFORM IMS-PURG-MSG-ALT1                                         
131300     END-IF                                                               
131400                                                                          
131500     IF SPLIT                                                             
131600        PERFORM UNTIL WS-ANTSPLIT = 0                                     
131700                      OR                                                  
131800                      PLOCK-IX > WS-BEST-PLKSATS                          
131900           IF 4002-ORDDEL (98) NOT = LOW-VALUE                            
132000              MOVE 4002-ORDDEL (98) TO 4002-ORDDEL (1)                    
132100              MOVE LOW-VALUE        TO 4002-ORDDEL (98)                   
132200           ELSE                                                           
132300              MOVE 4002-ORDDEL (99) TO 4002-ORDDEL (1)                    
132400              MOVE LOW-VALUE        TO 4002-ORDDEL (99)                   
132500           END-IF                                                         
132600           MOVE 4002-IDPLKLST (1) TO W-4001-IDPLKLST                      
132700                                     PTOP1-MID-IDPLKLST                   
132800           ADD 1                  TO PLOCK-IX                             
132900                                     ANT-PLKSATS                          
133000           PERFORM S13-HAMTA-PLOCKSATSNR                                  
133100           PERFORM DBAA-HAMTA-SPLITGRANS                                  
133200           PERFORM S15-ISRT-PLOCKSATS                                     
133300           PERFORM IMS-PURG-MSG-ALT1                                      
133400        END-PERFORM                                                       
133500        IF PRC OR TRANSPORT                                               
133600           PERFORM IMS-REPL-ORQI-WLORQI12                                 
133700        ELSE                                                              
133800           PERFORM IMS-REPL-ORQL-WLORQI12                                 
133900        END-IF                                                            
134000     END-IF                                                               
134100     .                                                                    
134200     EJECT                                                                
134300 DBAA-HAMTA-SPLITGRANS SECTION.                                           
134400                                                                          
134500     IF WS-ANTSPLIT > 1                                                   
134600        MOVE WS-PRC-SPLITGRANS TO 4002-KVORDSPL                           
134700        MOVE JA                TO 4002-FLORDSPL                           
134800        MOVE WS-KDSORT         TO 4002-KDSORT                             
134900        PERFORM DBAAA-HAMTA-NASTA-PLOCKLISTNR                             
135000     ELSE                                                                 
135100        MOVE NEJ               TO 4002-FLORDSPL                           
135200        MOVE SPACE             TO 4002-KDSORT                             
135300        MOVE ZERO              TO 4002-KVORDSPL                           
135400     END-IF                                                               
135500     SUBTRACT 1 FROM WS-ANTSPLIT                                          
135600     .                                                                    
135700     EJECT                                                                
135800 DBAAA-HAMTA-NASTA-PLOCKLISTNR SECTION.                                   
135900***************************************************************           
136000* OM PLOCKSATSEN INTE ÄR DEN SISTA AV ANTAL BESTÄLLDA LÄGGS   *           
136100* IDENTITETEN PÅ DEN SPLITADE PLOCKSATSEN I ELEMENT 98,       *           
136200* ANNARS I ELEMENT 99. DETTA GÖRS FÖR ATT MAN I PGM W40375    *           
136300* SKALL KUNNA AVGÖRA OM DEN NYA ORDERDELEN SKALL HA STATUS    *           
136400* 'U' ELLER 'R'.                                              *           
136500***************************************************************           
136600                                                                          
136700     ADD 1 TO ORQI-ARB-IDPLKLST-SISTA                                     
136800                                                                          
136900     IF ANT-PLKSATS < WS-BEST-PLKSATS                                     
137000        MOVE 4002-ORDDEL (1)         TO 4002-ORDDEL     (98)              
137100        MOVE ORQI-ARB-IDPLKLST-SISTA TO 4002-IDPLKLST (98)                
137200     ELSE                                                                 
137300        MOVE 4002-ORDDEL (1)         TO 4002-ORDDEL     (99)              
137400        MOVE ORQI-ARB-IDPLKLST-SISTA TO 4002-IDPLKLST (99)                
137500     END-IF                                                               
137600     .                                                                    
137700     EJECT                                                                
137800 DC-PLOCKSATS-DAG SECTION.                                                
137900                                                                          
138000     MOVE ORQA-ODEL-IDPRCVAR TO WS-SPAR-IDPRCVAR                          
138100     MOVE NEJ                TO PLOCKGRANS-SW                             
138200                                                                          
138300     IF W-Q3ASEQ-MAX-IDPRCVAR = HIGH-VALUE                                
138400        MOVE LOW-VALUE TO W-Q3ASEQ-MIN-IDPRCVAR                           
138500     END-IF                                                               
138600                                                                          
138700     IF W-Q3BSEQ-MAX-IDPRCVAR = HIGH-VALUE                                
138800        MOVE LOW-VALUE TO W-Q3BSEQ-MIN-IDPRCVAR                           
138900     END-IF                                                               
139000                                                                          
139100     PERFORM S09-INIT-PLOCKSATSREG                                        
139200                                                                          
139300     PERFORM UNTIL PLOCKGRANS                                             
139400                   OR                                                     
139500                   ORDERDEL-SLUT                                          
139600                   OR                                                     
139700                   PRC-BRYTNING                                           
139800        IF (MID-MIXAT = JA)                                               
139900            OR                                                            
140000           (TRANSPORT)                                                    
140100            OR                                                            
140200           (MID-MIXAT = NEJ                                               
140300            AND                                                           
140400            PRC                                                           
140500            AND                                                           
140600            ORQA-ODEL-IDPRCVAR = WS-SPAR-IDPRCVAR)                        
140700           ADD 1                  TO WS-KVORDER                           
140800           ADD ORQA-ODEL-KVRADER  TO WS-KVRADER                           
140900           ADD ORQA-ODEL-VKORDNTO TO WS-VKORDNTO                          
141000           ADD ORQA-ODEL-VLORDNTO TO WS-VLORDNTO                          
141100           PERFORM S10-KOLLA-PLOCKGRANS                                   
141200           PERFORM S11-SPARA-I-PLOCKSATSREG                               
141300           PERFORM S12-UPPDAT-ORDERDEL                                    
141400           PERFORM S01-LAES-NASTA-ORDERDEL                                
141500        ELSE                                                              
141600           MOVE JA TO PRC-BRYT-SW                                         
141700        END-IF                                                            
141800     END-PERFORM                                                          
141900                                                                          
142000     IF PLOCKGRANS                                                        
142100        OR                                                                
142200       (ORDERDEL-SLUT AND TRANSPORT)                                      
142300        ADD 1                 TO ANT-PLKSATS                              
142400        PERFORM DCA-LAGG-UPP-PLOCKSATS-DAG                                
142500     ELSE                                                                 
142600        IF ORDERDEL-FINNS                                                 
142700           MOVE WLORQA01   TO W-ORDERDEL                                  
142800           PERFORM S14-ATERSTALL-ORDERDELAR                               
142900           MOVE W-ORDERDEL TO WLORQA01                                    
143000        ELSE                                                              
143100           PERFORM S14-ATERSTALL-ORDERDELAR                               
143200        END-IF                                                            
143300     END-IF                                                               
143400                                                                          
143500     IF ANT-PLKSATS < WS-BEST-PLKSATS                                     
143600        IF ORDERDEL-FINNS                                                 
143700           MOVE JA TO OMSTART-SW                                          
143800        END-IF                                                            
143900     END-IF                                                               
144000     .                                                                    
144100     EJECT                                                                
144200 DCA-LAGG-UPP-PLOCKSATS-DAG SECTION.                                      
144300                                                                          
144400     IF MID-KDPRT-PU NOT = ALL '+'                                        
144500        MOVE MID-KDPRT-PU  TO 4002-KDPRT-PU                               
144600     END-IF                                                               
144700     IF MID-KDPRT-PLE NOT = ALL '+'                                       
144800        MOVE MID-KDPRT-PLE TO 4002-KDPRT-PLE                              
144900     END-IF                                                               
145000     MOVE 4002-IDPRODNR (1) TO W-4001-IDPRODNR                            
145100                               PTOP1-MID-IDPRODNR                         
145200     MOVE 4002-IDPLKLST (1) TO W-4001-IDPLKLST                            
145300                               PTOP1-MID-IDPLKLST                         
145400     PERFORM S13-HAMTA-PLOCKSATSNR                                        
145500     MOVE ZERO             TO 4002-IXHEL                                  
145600     PERFORM S15-ISRT-PLOCKSATS                                           
145700     PERFORM IMS-ISRT-MSG-ALT1                                            
145800     .                                                                    
145900     EJECT                                                                
146000 E-OMSTART-W40353 SECTION.                                                
146100                                                                          
146200     MOVE ANT-PLKSATS    TO MID-ANTPLK                                    
146300     IF PRC                                                               
146400        MOVE ORQA-ODEL-IDDC        TO MID-IDDC-BSEQ                       
146500        MOVE ORQA-ODEL-IDPRCBAS    TO MID-IDPRCBAS-BSEQ                   
146600        MOVE ORQA-ODEL-DAUTSKR (3:6) TO MID-TIUTSKR-BSEQ                  
146700        MOVE ORQA-ODEL-TIUTSTID    TO MID-TIUTSTID-BSEQ                   
146800        MOVE ORQA-ODEL-DARFS (3:10) TO MID-TIRFS-BSEQ                     
146900        MOVE ORQA-ODEL-DALSTORD (3:10) TO MID-TILST-O-BSEQ                
147000        MOVE ORQA-ODEL-IDPRCVAR    TO MID-IDPRCVAR-BSEQ                   
147100     ELSE                                                                 
147200        IF TRANSPORT                                                      
147300           MOVE ORQA-ODEL-IDDC        TO MID-IDDC-ASEQ                    
147400           MOVE ORQA-ODEL-IDTRP       TO MID-IDTRP-ASEQ                   
147500           MOVE ORQA-ODEL-DATRPAVD (3:6) TO MID-TIAAMMDD-ASEQ             
147600           MOVE ORQA-ODEL-TIHHMM      TO MID-TIHHMM-ASEQ                  
147700           MOVE ORQA-ODEL-DARFS (3:10) TO MID-TIRFS-ASEQ                  
147800           MOVE ORQA-ODEL-DALSTORD (3:10) TO MID-TILST-O-ASEQ             
147900           MOVE ORQA-ODEL-IDPRC       TO MID-IDPRC-ASEQ                   
148000        END-IF                                                            
148100     END-IF                                                               
148200                                                                          
148300     MOVE MID-W4I35301   TO PTOP2-MID-W4I35301                            
148400     PERFORM IMS-ISRT-MSG-ALT2                                            
148500     .                                                                    
148600     EJECT                                                                
148700 S01-LAES-NASTA-ORDERDEL SECTION.                                         
148800*BA                                                                       
148900     MOVE NEJ TO LAES-SW                                                  
149000                                                                          
149100     IF PRC                                                               
149200        PERFORM S02-LAES-VIA-PRC                                          
149300     ELSE                                                                 
149400        IF TRANSPORT                                                      
149500           PERFORM S03-LAES-VIA-TRANSPORT                                 
149600        ELSE                                                              
149700           IF PRC-ORDERID                                                 
149800              PERFORM S04-LAES-VIA-PRC-ORDERID                            
149900           ELSE                                                           
150000              IF ORDERID                                                  
150100                 PERFORM S05-LAES-VIA-ORDERID                             
150200              END-IF                                                      
150300           END-IF                                                         
150400        END-IF                                                            
150500     END-IF                                                               
150600                                                                          
150700     IF ORDERDEL-FINNS                                                    
150800        ADD 1 TO ANT-ODEL                                                 
150900     END-IF                                                               
151000     .                                                                    
151100     EJECT                                                                
151200 S02-LAES-VIA-PRC SECTION.                                                
151300                                                                          
151400     PERFORM UNTIL LAES-OK                                                
151500        IF FORSTA-GANG                                                    
151600           MOVE NEJ TO FORSTA-SW                                          
151700           PERFORM IMS-GHU-ORQB-WLORQA01                                  
151800        ELSE                                                              
151900           PERFORM IMS-GHN-ORQB-WLORQA01                                  
152000        END-IF                                                            
152100        IF SEGMENT-SAKNAS OR END-OF-DATA                                  
152200           MOVE JA TO LAES-SW                                             
152300                      ORDERDEL-SW                                         
152400        END-IF                                                            
152500        IF SEGMENT-FINNS                                                  
152600           IF ORQA-ODEL-IDDC = WS-IDDC                                    
152700             IF ANT-ODEL > 0                                              
152800                IF (ORQA-ODEL-KVRADER <= WS-PRC-KVPLSRAD                  
152900                    AND                                                   
153000                    ORQA-ODEL-VKORDNTO <= WS-PRC-VKPLSNTO                 
153100                    AND                                                   
153200                    ORQA-ODEL-VLORDNTO <= WS-PRC-VLPLSNTO)                
153300                    OR                                                    
153400                   (MID-MIXAT = NEJ                                       
153500                    AND                                                   
153600                    ORQA-ODEL-IDPRCVAR NOT = WS-SPAR-IDPRCVAR)            
153700                   MOVE JA TO LAES-SW                                     
153800                END-IF                                                    
153900             ELSE                                                         
154000                MOVE JA               TO LAES-SW                          
154100             END-IF                                                       
154200           ELSE                                                           
154300             MOVE '401' TO MED-IDMFSINF                                   
154400             MOVE NEJ                 TO LAES-SW                          
154500           END-IF                                                         
154600           IF LAES-OK                                                     
154700              MOVE ORQA-ODEL-IDORDER  TO W-IDORDER                        
154800              MOVE ORQA-ODEL-IDDC     TO W-IDDC                           
154900              PERFORM IMS-GHU-ORQI-WLORQI01-WLORQI12                      
155000              IF SEGMENT-FINNS                                            
155100                 IF ORQI-OHUV-FLKLAR = JA                                 
155200                    IF LAES-PRC                                           
155300                       PERFORM S08-LAES-PRC                               
155400                       IF PRC-OK                                          
155500                          AND                                             
155600                          PRC-DAG                                         
155700                          MOVE NEJ TO PRC-SW                              
155800                       END-IF                                             
155900                    END-IF                                                
156000                 ELSE                                                     
156100                    MOVE NEJ TO LAES-SW                                   
156200                 END-IF                                                   
156300              ELSE                                                        
156400                 MOVE NEJ TO LAES-SW                                      
156500              END-IF                                                      
156600           END-IF                                                         
156700        END-IF                                                            
156800     END-PERFORM                                                          
156900     .                                                                    
157000     EJECT                                                                
157100 S03-LAES-VIA-TRANSPORT SECTION.                                          
157200                                                                          
157300     PERFORM UNTIL LAES-OK                                                
157400        IF FORSTA-GANG                                                    
157500           MOVE NEJ TO FORSTA-SW                                          
157600           PERFORM IMS-GHU-ORQA-WLORQA01                                  
157700        ELSE                                                              
157800           PERFORM IMS-GHN-ORQA-WLORQA01                                  
157900        END-IF                                                            
158000        IF SEGMENT-SAKNAS OR END-OF-DATA                                  
158100           MOVE JA TO LAES-SW                                             
158200                      ORDERDEL-SW                                         
158300        END-IF                                                            
158400        IF SEGMENT-FINNS                                                  
158500           IF ORQA-ODEL-IDDC = WS-IDDC                                    
158600             IF ANT-ODEL > 0                                              
158700                IF ORQA-ODEL-KVRADER  <= WS-PRC-KVPLSRAD                  
158800                   AND                                                    
158900                   ORQA-ODEL-VKORDNTO <= WS-PRC-VKPLSNTO                  
159000                   AND                                                    
159100                   ORQA-ODEL-VLORDNTO <= WS-PRC-VLPLSNTO                  
159200                   MOVE JA TO LAES-SW                                     
159300                END-IF                                                    
159400             ELSE                                                         
159500                MOVE JA TO LAES-SW                                        
159600             END-IF                                                       
159700           ELSE                                                           
159800             MOVE '401' TO MED-IDMFSINF                                   
159900             MOVE NEJ                 TO LAES-SW                          
160000           END-IF                                                         
160100           IF LAES-OK                                                     
160200              MOVE ORQA-ODEL-IDORDER  TO W-IDORDER                        
160300              MOVE ORQA-ODEL-IDDC     TO W-IDDC                           
160400              PERFORM IMS-GHU-ORQI-WLORQI01-WLORQI12                      
160500              IF SEGMENT-FINNS                                            
160600                 IF ORQI-OHUV-FLKLAR = JA                                 
160700                    IF LAES-PRC                                           
160800                       PERFORM S08-LAES-PRC                               
160900                       IF PRC-OK                                          
161000                          AND                                             
161100                          PRC-DAG                                         
161200                          MOVE NEJ TO PRC-SW                              
161300                       END-IF                                             
161400                    END-IF                                                
161500                 ELSE                                                     
161600                    MOVE NEJ TO LAES-SW                                   
161700                 END-IF                                                   
161800              ELSE                                                        
161900                 MOVE NEJ TO LAES-SW                                      
162000              END-IF                                                      
162100           END-IF                                                         
162200        END-IF                                                            
162300     END-PERFORM                                                          
162400     .                                                                    
162500     EJECT                                                                
162600 S04-LAES-VIA-PRC-ORDERID SECTION.                                        
162700                                                                          
162800     IF ORDERHUVUD-SAKNAS                                                 
162900        MOVE WS-IDDC  TO W-IDDC                                           
163000        PERFORM IMS-GHU-ORQL-WLORQI01-WLORQI12                            
163100        IF SEGMENT-FINNS                                                  
163200           IF ORQI-OHUV-FLKLAR = JA                                       
163300              MOVE JA TO OHUV-SW                                          
163400              MOVE ORQI-OHUV-IDORDER TO W-Q301KY-MIN-IDORDER              
163500                                        W-Q301KY-MAX-IDORDER              
163600              MOVE WS-IDDC           TO W-Q301KY-MIN-IDDC                 
163700                                        W-Q301KY-MAX-IDDC                 
163800           ELSE                                                           
163900              MOVE 'GE'              TO STATUS-WS                         
164000           END-IF                                                         
164100        END-IF                                                            
164200     END-IF                                                               
164300                                                                          
164400     IF ORDERHUVUD-OK                                                     
164500        PERFORM UNTIL LAES-OK                                             
164600           PERFORM IMS-GHN-ORQ-WLORQA01                                   
164700           IF SEGMENT-SAKNAS OR END-OF-DATA                               
164800              MOVE JA TO LAES-SW                                          
164900                         ORDERDEL-SW                                      
165000           END-IF                                                         
165100           IF SEGMENT-FINNS                                               
165200              IF ORQA-ODEL-IDPRC = WS-IDPRC                               
165300                 AND                                                      
165400                 ORQA-ODEL-KDODELSTA = 'R'                                
165500                 MOVE JA TO LAES-SW                                       
165600              END-IF                                                      
165700           END-IF                                                         
165800        END-PERFORM                                                       
165900     END-IF                                                               
166000     .                                                                    
166100     EJECT                                                                
166200 S05-LAES-VIA-ORDERID SECTION.                                            
166300                                                                          
166400     IF ORDERHUVUD-SAKNAS                                                 
166500        MOVE WS-IDDC        TO W-IDDC                                     
166600        PERFORM IMS-GHU-ORQL-WLORQI01-WLORQI12                            
166700        IF SEGMENT-FINNS                                                  
166800           IF ORQI-OHUV-FLKLAR = JA                                       
166900              MOVE JA TO OHUV-SW                                          
167000              MOVE ORQI-OHUV-IDORDER TO W-Q301KY-MIN-IDORDER              
167100                                        W-Q301KY-MAX-IDORDER              
167200              MOVE WS-IDDC           TO W-Q301KY-MIN-IDDC                 
167300                                        W-Q301KY-MAX-IDDC                 
167400           ELSE                                                           
167500              MOVE 'GE'              TO STATUS-WS                         
167600           END-IF                                                         
167700        END-IF                                                            
167800     END-IF                                                               
167900                                                                          
168000     IF ORDERHUVUD-OK                                                     
168100        PERFORM UNTIL LAES-OK                                             
168200           PERFORM IMS-GHN-ORQ-WLORQA01                                   
168300           IF SEGMENT-SAKNAS OR                                           
168400              END-OF-DATA                                                 
168500              MOVE JA TO LAES-SW                                          
168600                         ORDERDEL-SW                                      
168700           END-IF                                                         
168800           IF SEGMENT-FINNS                                               
168900              AND                                                         
169000              ORQA-ODEL-KDODELSTA = 'R'                                   
169100              PERFORM S08-LAES-PRC                                        
169200              IF PRC-OK                                                   
169300                 MOVE JA TO LAES-SW                                       
169400              END-IF                                                      
169500           END-IF                                                         
169600        END-PERFORM                                                       
169700     END-IF                                                               
169800     .                                                                    
169900     EJECT                                                                
170000 S06-FEL-MEDDELANDE SECTION.                                              
170100                                                                          
170200     CALL WMEDKONV USING MED-WMEDAREA                                     
170300     MOVE MED-MFSFEL TO MOD-TEMFSFEL                                      
170400     .                                                                    
170500                                                                          
170600                                                                          
170700 S07-INFO-MEDDELANDE SECTION.                                             
170800                                                                          
170900     CALL WMEDKONV USING MED-WMEDAREA                                     
171000     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
171100     .                                                                    
171200     EJECT                                                                
171300 S08-LAES-PRC SECTION.                                                    
171400                                                                          
171500     MOVE WS-IDDC         TO W-4447-IDDC                                  
171600     MOVE ORQA-ODEL-IDPRC TO W-4448-IDPRC                                 
171700                                                                          
171800     PERFORM IMS-GU-XXKH-WLXXKH11                                         
171900                                                                          
172000     IF SEGMENT-FINNS                                                     
172100        MOVE XXKH-4448-KVORDER  TO WS-PRC-KVORDER                         
172200        MOVE XXKH-4448-KVRADER  TO WS-PRC-KVRADER                         
172300        MOVE XXKH-4448-VKORDNTO TO WS-PRC-VKORDNTO                        
172400        MOVE XXKH-4448-VLORDNTO TO WS-PRC-VLORDNTO                        
172500        MOVE XXKH-4448-KVPLSRAD TO WS-PRC-KVPLSRAD                        
172600        MOVE XXKH-4448-VKPLSNTO TO WS-PRC-VKPLSNTO                        
172700        MOVE XXKH-4448-VLPLSNTO TO WS-PRC-VLPLSNTO                        
172800        MOVE XXKH-4448-RESPLIT  TO WS-PRC-RESPLIT                         
172900        MOVE XXKH-4448-KDPRCGRP TO W-4462-KDPRCGRP-X                      
173000        MOVE JA                 TO PRC-SW                                 
173100     ELSE                                                                 
173200        MOVE NEJ                TO PRC-SW                                 
173300     END-IF                                                               
173400                                                                          
173500     IF PRC-OK                                                            
173600        IF WS-PRC-KVORDER > 1                                             
173700           MOVE 1 TO WS-PRCTYP                                            
173800        ELSE                                                              
173900           MOVE 2 TO WS-PRCTYP                                            
174000        END-IF                                                            
174100     END-IF                                                               
174200     .                                                                    
174300     EJECT                                                                
174400 S09-INIT-PLOCKSATSREG SECTION.                                           
174500                                                                          
174600     MOVE 1          TO 4002-KDSEGKEY                                     
174700     MOVE NEJ        TO 4002-FLORDSPL                                     
174800                        4002-FLORDKNY                                     
174900     MOVE SPACE      TO 4002-IDBORD                                       
175000                        4002-KDPRT-PLE                                    
175100                        4002-KDPRT-PU                                     
175200                        4002-KDSORT                                       
175300                        4002-IDMSG3IV                                     
175310                        4002-IDSNO3IV                                     
175320                        4002-ADDISPXTRA                                   
175400                                                                          
175500     INITIALIZE         4002-DEAL-PR-SUM                                  
175600     MOVE ZERO       TO 4002-IXHEL                                        
175700                        4002-KVORDSPL                                     
175800                        4002-IDUSER                                       
175900                        4002-KVRADER                                      
176000                        4002-VKORDNTO                                     
176100                        4002-VLORDNTO                                     
176200                        4002-SUORDV                                       
176300                        4002-IDLOPNR-PL                                   
176400                                                                          
176500     MOVE 1 TO IX1                                                        
176600     PERFORM UNTIL IX1 > 99                                               
176700        MOVE LOW-VALUE TO 4002-ORDDEL (IX1)                               
176800        ADD 1 TO IX1                                                      
176900     END-PERFORM                                                          
177000     .                                                                    
177100     EJECT                                                                
177200 S10-KOLLA-PLOCKGRANS SECTION.                                            
177300                                                                          
177400     IF PRC OR TRANSPORT                                                  
177500        IF WS-KVORDER    = WS-PRC-KVORDER     OR                          
177600           WS-KVRADER   >= WS-PRC-KVRADER     OR                          
177700           WS-VLORDNTO  >= WS-PRC-VLORDNTO    OR                          
177800           WS-VKORDNTO  >= WS-PRC-VKORDNTO                                
177900           MOVE JA TO PLOCKGRANS-SW                                       
178000           MOVE 0  TO ANT-ODEL                                            
178100        END-IF                                                            
178200     ELSE                                                                 
178300        IF PRC-ORDERID OR ORDERID                                         
178400           MOVE JA TO PLOCKGRANS-SW                                       
178500        END-IF                                                            
178600     END-IF                                                               
178700                                                                          
178800                                                                          
178900     IF PLOCKGRANS                                                        
179000        IF WS-PRC-KVORDER = 1                                             
179100           IF WS-KVRADER    > WS-PRC-KVRADER     OR                       
179200              WS-VLORDNTO   > WS-PRC-VLORDNTO    OR                       
179300              WS-VKORDNTO   > WS-PRC-VKORDNTO                             
179400              MOVE JA TO SPLIT-SW                                         
179500           END-IF                                                         
179600        END-IF                                                            
179700     END-IF                                                               
179800     .                                                                    
179900     EJECT                                                                
180000 S11-SPARA-I-PLOCKSATSREG SECTION.                                        
180100                                                                          
180200     IF SPLIT                                                             
180300        PERFORM S11A-BERAKNA-ANTAL-SPLITSATSER                            
180400     END-IF                                                               
180500                                                                          
180600     PERFORM S11B-LAGRA-ORDERDEL                                          
180700     .                                                                    
180800     EJECT                                                                
180900 S11A-BERAKNA-ANTAL-SPLITSATSER SECTION.                                  
181000                                                                          
181100     MOVE 0 TO WS-PROC-RAD                                                
181200               WS-PROC-VIKT                                               
181300               WS-PROC-VOLYM                                              
181400                                                                          
181500     IF WS-KVRADER > WS-PRC-KVRADER                                       
181600        COMPUTE WS-PROC-RAD =                                             
181700                WS-KVRADER * 100 / WS-PRC-KVRADER                         
181800                ON SIZE ERROR MOVE 0 TO WS-PROC-RAD                       
181900        END-COMPUTE                                                       
182000     END-IF                                                               
182100                                                                          
182200     IF WS-VKORDNTO > WS-PRC-VKORDNTO                                     
182300        COMPUTE WS-PROC-VIKT =                                            
182400                WS-VKORDNTO * 100 / WS-PRC-VKORDNTO                       
182500                ON SIZE ERROR MOVE 0 TO WS-PROC-VIKT                      
182600        END-COMPUTE                                                       
182700     END-IF                                                               
182800                                                                          
182900     IF WS-VLORDNTO > WS-PRC-VLORDNTO                                     
183000        COMPUTE WS-PROC-VOLYM =                                           
183100                WS-VLORDNTO * 100 / WS-PRC-VLORDNTO                       
183200                ON SIZE ERROR MOVE 0 TO WS-PROC-VOLYM                     
183300        END-COMPUTE                                                       
183400     END-IF                                                               
183500                                                                          
183600     IF WS-PROC-RAD NOT < WS-PROC-VIKT                                    
183700        AND                                                               
183800        WS-PROC-RAD NOT < WS-PROC-VOLYM                                   
183900        MOVE WS-KVRADER     TO WS-SPLITGRANS                              
184000        MOVE WS-PRC-KVRADER TO WS-PRC-SPLITGRANS                          
184100        MOVE 'RA'           TO WS-KDSORT                                  
184200     ELSE                                                                 
184300        IF WS-PROC-VIKT NOT < WS-PROC-RAD                                 
184400           AND                                                            
184500           WS-PROC-VIKT NOT < WS-PROC-VOLYM                               
184600           MOVE WS-VKORDNTO     TO WS-SPLITGRANS                          
184700           MOVE WS-PRC-VKORDNTO TO WS-PRC-SPLITGRANS                      
184800           MOVE 'KG'            TO WS-KDSORT                              
184900        ELSE                                                              
185000           IF WS-PROC-VOLYM NOT < WS-PROC-RAD                             
185100              AND                                                         
185200              WS-PROC-VOLYM NOT < WS-PROC-VIKT                            
185300              MOVE WS-VLORDNTO     TO WS-SPLITGRANS                       
185400              MOVE WS-PRC-VLORDNTO TO WS-PRC-SPLITGRANS                   
185500              MOVE 'M3'            TO WS-KDSORT                           
185600           END-IF                                                         
185700        END-IF                                                            
185800     END-IF                                                               
185900                                                                          
186000     COMPUTE WS-ANTSPLIT = WS-SPLITGRANS / WS-PRC-SPLITGRANS              
186100             ON SIZE ERROR MOVE ZERO TO WS-ANTSPLIT                       
186200     END-COMPUTE                                                          
186300                                                                          
186400     IF WS-ANTSPLIT (9:2) > ZERO                                          
186500        MOVE ZERO              TO WS-ANTSPLIT (9:2)                       
186600        COMPUTE WS-SPLITREST = WS-SPLITGRANS -                            
186700                              (WS-ANTSPLIT * WS-PRC-SPLITGRANS)           
186800        END-COMPUTE                                                       
186900        COMPUTE WS-RESPLIT =  WS-SPLITREST / WS-PRC-SPLITGRANS            
187000                ON SIZE ERROR MOVE ZERO TO WS-RESPLIT                     
187100        END-COMPUTE                                                       
187200        IF WS-RESPLIT >= WS-PRC-RESPLIT                                   
187300           ADD 1 TO WS-ANTSPLIT                                           
187400        END-IF                                                            
187500     END-IF                                                               
187600     .                                                                    
187700     EJECT                                                                
187800 S11B-LAGRA-ORDERDEL SECTION.                                             
187900                                                                          
188000     ADD 1                   TO 4002-IXHEL                                
188100     MOVE ORQA-ODEL-IDORDER  TO 4002-IDORDER  (4002-IXHEL)                
188200     MOVE ORQA-ODEL-IDDC     TO 4002-IDDC     (4002-IXHEL)                
188300     MOVE ORQA-ODEL-IDPRODNR TO 4002-IDPRODNR (4002-IXHEL)                
188400     MOVE ORQA-ODEL-IDPLKLST TO 4002-IDPLKLST (4002-IXHEL)                
188500     .                                                                    
188600     EJECT                                                                
188700 S12-UPPDAT-ORDERDEL SECTION.                                             
188800                                                                          
188900     MOVE WS-DATUM-LOK    TO ORQA-ODEL-DAUTSKR                            
189000     IF WS-DATUM-LOK NOT = ZERO                                           
189100       IF WS-DATUM-LOK < 500000                                           
189200         MOVE 20          TO ORQA-ODEL-DAUTSKR (1:2)                      
189300       ELSE                                                               
189400         IF WS-DATUM-LOK < 999999                                         
189500           MOVE 19        TO ORQA-ODEL-DAUTSKR (1:2)                      
189600         ELSE                                                             
189700           MOVE 99999999  TO ORQA-ODEL-DAUTSKR                            
189800         END-IF                                                           
189900       END-IF                                                             
190000     END-IF                                                               
190100     MOVE WS-TIHHMMSS-LOK TO ORQA-ODEL-TIUTSTID                           
190200     MOVE 'U'             TO ORQA-ODEL-KDODELSTA                          
190300     IF PRC                                                               
190400        PERFORM IMS-REPL-ORQB-WLORQA01                                    
190500     END-IF                                                               
190600     IF TRANSPORT                                                         
190700        PERFORM IMS-REPL-ORQA-WLORQA01                                    
190800     END-IF                                                               
190900     IF PRC-ORDERID OR ORDERID                                            
191000        PERFORM IMS-REPL-ORQ-WLORQA01                                     
191100     END-IF                                                               
191200     .                                                                    
191300     EJECT                                                                
191400 S13-HAMTA-PLOCKSATSNR SECTION.                                           
191500                                                                          
191600     MOVE WS-IDDC             TO W-4461-IDDC                              
191700     PERFORM IMS-GHU-XXKO-WLXXKO11                                        
191800     IF SEGMENT-FINNS                                                     
191900        IF XXKO-4462-IDLOPNR-PL = 999                                     
192000           MOVE WS-DATUM-LOK TO XXKO-4462-TIDATUM                         
192100           MOVE 1            TO XXKO-4462-IDLOPNR-PL                      
192200        ELSE                                                              
192300           ADD  1            TO XXKO-4462-IDLOPNR-PL                      
192400        END-IF                                                            
192500        MOVE XXKO-4462-IDLOPNR-PL TO 4002-IDLOPNR-PL                      
192600        PERFORM IMS-REPL-XXKO-WLXXKO11                                    
192700     ELSE                                                                 
192800        MOVE 1                    TO 4002-IDLOPNR-PL                      
192900     END-IF                                                               
193000     .                                                                    
193100     EJECT                                                                
193200 S14-ATERSTALL-ORDERDELAR SECTION.                                        
193300                                                                          
193400     MOVE 1 TO IX1                                                        
193500                                                                          
193600     PERFORM UNTIL IX1 > 99                                               
193700       PERFORM S14A-LAES-ORDERDEL                                         
193800       IF SEGMENT-FINNS                                                   
193900          PERFORM IMS-REPL-ORQ-WLORQA01                                   
194000          ADD 1 TO IX1                                                    
194100       END-IF                                                             
194200     END-PERFORM                                                          
194300     .                                                                    
194400     EJECT                                                                
194500 S14A-LAES-ORDERDEL SECTION.                                              
194600                                                                          
194700     IF 4002-ORDDEL (IX1) NOT = LOW-VALUE                                 
194800        MOVE 4002-IDORDER  (IX1) TO W-Q301KY-IDORDER                      
194900        MOVE 4002-IDDC     (IX1) TO W-Q301KY-IDDC                         
195000        MOVE 4002-IDPRODNR (IX1) TO W-Q301KY-IDPRODNR                     
195100        MOVE 4002-IDPLKLST (IX1) TO W-Q301KY-IDPLKLST                     
195200        PERFORM IMS-GHU-ORQ-WLORQA01                                      
195300        IF SEGMENT-FINNS                                                  
195400           MOVE ZERO    TO ORQA-ODEL-DAUTSKR                              
195500                           ORQA-ODEL-TIUTSTID                             
195600           MOVE 'R'     TO ORQA-ODEL-KDODELSTA                            
195700        END-IF                                                            
195800     ELSE                                                                 
195900        MOVE 'GE'    TO STATUS-WS                                         
196000        MOVE 100     TO IX1                                               
196100     END-IF                                                               
196200     .                                                                    
196300     EJECT                                                                
196400 S15-ISRT-PLOCKSATS SECTION.                                              
196500                                                                          
196600     MOVE W-4001-IDHTYP-X  TO WL400101                                    
196700                                                                          
196800     PERFORM IMS-ISRT-4001-WL400101                                       
196900                                                                          
197000     PERFORM IMS-ISRT-4001-WL400111                                       
197100     .                                                                    
197200     EJECT                                                                
197300 MFS-RENSA-FAELT-UT SECTION.                                              
197400                                                                          
197500*    --- ALLA UTDATA-FÄLT                                                 
197600     MOVE MFS-RENSA-FAELT TO MOD-KDPRT-PU                                 
197700                             MOD-KDPRT-PLE                                
197800     .                                                                    
197900     EJECT                                                                
198000 MFS-ROR-EJ-FAELT-UT  SECTION.                                            
198100                                                                          
198200*    --- ALLA UTDATA-FÄLT                                                 
198300                                                                          
198400     MOVE MFS-ROER-EJ-FAELT TO MOD-KDPRT-PU                               
198500                               MOD-KDPRT-PLE                              
198600                               MOD-MIXAT                                  
198700     .                                                                    
198800     EJECT                                                                
198900* --- IMS SEKTIONER ---                                                   
199000     SKIP3                                                                
199100 IMS-GU-MSG SECTION.                                                      
199200                                                                          
199300     MOVE '  QC' TO GODK-STATUSKODER                                      
199400     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
199500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
199600     PERFORM IMS-STATUSKONTROLL                                           
199700     .                                                                    
199800     SKIP3                                                                
199900 IMS-ISRT-MSG SECTION.                                                    
200000                                                                          
200100     IF NOT ENGLISH-TEXT                                                  
200200       MOVE '0' TO MFS-KDHUVOMR                                           
200300     END-IF                                                               
200400     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
200500     MOVE SPACE TO GODK-STATUSKODER                                       
200600     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
200700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
200800     PERFORM IMS-STATUSKONTROLL                                           
200900     .                                                                    
201000     SKIP3                                                                
201100 IMS-ISRT-MSG-ALT1 SECTION.                                               
201200                                                                          
201300     MOVE SPACE TO GODK-STATUSKODER                                       
201400     CALL CBLTDLI USING ISRT ALT1-PCB P-TO-P-SW1                          
201500     MOVE ALT1-STATUS-CODE TO STATUS-WS                                   
201600     PERFORM IMS-STATUSKONTROLL                                           
201700     .                                                                    
201800     SKIP3                                                                
201900 IMS-PURG-MSG-ALT1 SECTION.                                               
202000                                                                          
202100     MOVE SPACE TO GODK-STATUSKODER                                       
202200     CALL CBLTDLI USING PURG ALT1-PCB P-TO-P-SW1                          
202300     MOVE ALT1-STATUS-CODE TO STATUS-WS                                   
202400     PERFORM IMS-STATUSKONTROLL                                           
202500     .                                                                    
202600     EJECT                                                                
202700 IMS-ISRT-MSG-ALT2 SECTION.                                               
202800                                                                          
202900     MOVE SPACE TO GODK-STATUSKODER                                       
203000     CALL CBLTDLI USING ISRT ALT2-PCB P-TO-P-SW2                          
203100     MOVE ALT2-STATUS-CODE TO STATUS-WS                                   
203200     PERFORM IMS-STATUSKONTROLL                                           
203300     .                                                                    
203400     EJECT                                                                
203500 IMS-GHU-ORQ-WLORQA01 SECTION.                                            
203600                                                                          
203700     STRING 'WLORQA01(WDQ301KY =' W-WDQ301KY-X ')'                        
203800          DELIMITED BY SIZE INTO SSA1                                     
203900     MOVE '  GE' TO GODK-STATUSKODER                                      
204000     CALL CBLTDLI USING GHU ORQ-PCB DLI-IO-AREA1 SSA1                     
204100     MOVE ORQ-STATUS-CODE TO STATUS-WS                                    
204200     PERFORM IMS-STATUSKONTROLL                                           
204300     .                                                                    
204400                                                                          
204500                                                                          
204600 IMS-GHN-ORQ-WLORQA01 SECTION.                                            
204700                                                                          
204800     STRING 'WLORQA01(WDQ301KY>=' W-WDQ301KY-MIN-X                        
204900                    '&WDQ301KY<=' W-WDQ301KY-MAX-X ')'                    
205000          DELIMITED BY SIZE INTO SSA1                                     
205100     MOVE '  GBGE' TO GODK-STATUSKODER                                    
205200     CALL CBLTDLI USING GHN ORQ-PCB DLI-IO-AREA1 SSA1                     
205300     MOVE ORQ-STATUS-CODE TO STATUS-WS                                    
205400     PERFORM IMS-STATUSKONTROLL                                           
205500     .                                                                    
205600     EJECT                                                                
205700 IMS-GHU-ORQA-WLORQA01 SECTION.                                           
205800                                                                          
205900     STRING 'WLORQA01(WDQ3ASEQ>=' W-WDQ3ASEQ-MIN-X                        
206000                    '&WDQ3ASEQ<=' W-WDQ3ASEQ-MAX-X                        
206100                    '&IDPRC   >=' W-Q3ASEQ-MIN-IDPRC                      
206200                    '&IDPRC   <=' W-Q3ASEQ-MAX-IDPRC ')'                  
206300          DELIMITED BY SIZE INTO SSA1                                     
206400     MOVE '  GE' TO GODK-STATUSKODER                                      
206500     CALL CBLTDLI USING GHU ORQA-PCB DLI-IO-AREA1 SSA1                    
206600     MOVE ORQA-STATUS-CODE TO STATUS-WS                                   
206700     PERFORM IMS-STATUSKONTROLL                                           
206800     .                                                                    
206900                                                                          
207000 IMS-GHN-ORQA-WLORQA01 SECTION.                                           
207100                                                                          
207200     STRING 'WLORQA01(WDQ3ASEQ>=' W-WDQ3ASEQ-MIN-X                        
207300                    '&WDQ3ASEQ<=' W-WDQ3ASEQ-MAX-X                        
207400                    '&IDPRC   >=' W-Q3ASEQ-MIN-IDPRC                      
207500                    '&IDPRC   <=' W-Q3ASEQ-MAX-IDPRC ')'                  
207600          DELIMITED BY SIZE INTO SSA1                                     
207700     MOVE '  GBGE' TO GODK-STATUSKODER                                    
207800     CALL CBLTDLI USING GHN ORQA-PCB DLI-IO-AREA1 SSA1                    
207900     MOVE ORQA-STATUS-CODE TO STATUS-WS                                   
208000     PERFORM IMS-STATUSKONTROLL                                           
208100     .                                                                    
208200     EJECT                                                                
208300 IMS-GHU-ORQB-WLORQA01 SECTION.                                           
208400                                                                          
208500     STRING 'WLORQA01(WDQ3BSEQ>=' W-WDQ3BSEQ-MIN-X                        
208600                    '&WDQ3BSEQ<=' W-WDQ3BSEQ-MAX-X                        
208700                    '&IDPRCVAR>=' W-Q3BSEQ-MIN-IDPRCVAR                   
208800                    '&IDPRCVAR<=' W-Q3BSEQ-MAX-IDPRCVAR ')'               
208900          DELIMITED BY SIZE INTO SSA1                                     
209000     MOVE '  GE' TO GODK-STATUSKODER                                      
209100     CALL CBLTDLI USING GHU ORQB-PCB DLI-IO-AREA1 SSA1                    
209200     MOVE ORQB-STATUS-CODE TO STATUS-WS                                   
209300     PERFORM IMS-STATUSKONTROLL                                           
209400     .                                                                    
209500                                                                          
209600 IMS-GHN-ORQB-WLORQA01 SECTION.                                           
209700                                                                          
209800     STRING 'WLORQA01(WDQ3BSEQ>=' W-WDQ3BSEQ-MIN-X                        
209900                    '&WDQ3BSEQ<=' W-WDQ3BSEQ-MAX-X                        
210000                    '&IDPRCVAR>=' W-Q3BSEQ-MIN-IDPRCVAR                   
210100                    '&IDPRCVAR<=' W-Q3BSEQ-MAX-IDPRCVAR ')'               
210200          DELIMITED BY SIZE INTO SSA1                                     
210300     MOVE '  GBGE' TO GODK-STATUSKODER                                    
210400     CALL CBLTDLI USING GHN ORQB-PCB DLI-IO-AREA1 SSA1                    
210500     MOVE ORQB-STATUS-CODE TO STATUS-WS                                   
210600     PERFORM IMS-STATUSKONTROLL                                           
210700     .                                                                    
210800     EJECT                                                                
210900 IMS-REPL-ORQ-WLORQA01 SECTION.                                           
211000                                                                          
211100     MOVE '  ' TO GODK-STATUSKODER                                        
211200     CALL CBLTDLI USING REPL ORQ-PCB DLI-IO-AREA1                         
211300     MOVE ORQ-STATUS-CODE TO STATUS-WS                                    
211400     PERFORM IMS-STATUSKONTROLL                                           
211500     .                                                                    
211600                                                                          
211700                                                                          
211800 IMS-REPL-ORQA-WLORQA01 SECTION.                                          
211900                                                                          
212000     MOVE '  ' TO GODK-STATUSKODER                                        
212100     CALL CBLTDLI USING REPL ORQA-PCB DLI-IO-AREA1                        
212200     MOVE ORQA-STATUS-CODE TO STATUS-WS                                   
212300     PERFORM IMS-STATUSKONTROLL                                           
212400     .                                                                    
212500                                                                          
212600                                                                          
212700 IMS-REPL-ORQB-WLORQA01 SECTION.                                          
212800                                                                          
212900     MOVE '  ' TO GODK-STATUSKODER                                        
213000     CALL CBLTDLI USING REPL ORQB-PCB DLI-IO-AREA1                        
213100     MOVE ORQB-STATUS-CODE TO STATUS-WS                                   
213200     PERFORM IMS-STATUSKONTROLL                                           
213300     .                                                                    
213400     EJECT                                                                
213500 IMS-GHU-ORQL-WLORQI01-WLORQI12 SECTION.                                  
213600                                                                          
213700     STRING 'WLORQI01*D(WDQ2CSEQ =' W-WDQ2CSEQ-X ')'                      
213800          DELIMITED BY SIZE INTO SSA1                                     
213900     STRING 'WLORQI12(IDDC     =' W-IDDC-X ')'                            
214000          DELIMITED BY SIZE INTO SSA2                                     
214100     MOVE '  GBGE' TO GODK-STATUSKODER                                    
214200     CALL CBLTDLI USING GHU ORQL-PCB DLI-IO-AREA2 SSA1 SSA2               
214300     MOVE ORQL-STATUS-CODE TO STATUS-WS                                   
214400     PERFORM IMS-STATUSKONTROLL                                           
214500     .                                                                    
214600                                                                          
214700                                                                          
214800 IMS-REPL-ORQL-WLORQI12 SECTION.                                          
214900                                                                          
215000     MOVE 'WLORQI01*N' TO SSA1                                            
215100     MOVE '    ' TO GODK-STATUSKODER                                      
215200     CALL CBLTDLI USING REPL ORQL-PCB DLI-IO-AREA2 SSA1                   
215300     MOVE ORQL-STATUS-CODE TO STATUS-WS                                   
215400     PERFORM IMS-STATUSKONTROLL                                           
215500     .                                                                    
215600     EJECT                                                                
215700 IMS-GHU-ORQI-WLORQI01-WLORQI12 SECTION.                                  
215800                                                                          
215900     STRING 'WLORQI01*D(IDORDER  =' W-IDORDER-X ')'                       
216000          DELIMITED BY SIZE INTO SSA1                                     
216100     STRING 'WLORQI12(IDDC     =' W-IDDC-X ')'                            
216200          DELIMITED BY SIZE INTO SSA2                                     
216300     MOVE '  GE' TO GODK-STATUSKODER                                      
216400     CALL CBLTDLI USING GHU ORQI-PCB DLI-IO-AREA2 SSA1 SSA2               
216500     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
216600     PERFORM IMS-STATUSKONTROLL                                           
216700     .                                                                    
216800                                                                          
216900                                                                          
217000 IMS-REPL-ORQI-WLORQI12 SECTION.                                          
217100                                                                          
217200     MOVE 'WLORQI01*N' TO SSA1                                            
217300     MOVE '    ' TO GODK-STATUSKODER                                      
217400     CALL CBLTDLI USING REPL ORQI-PCB DLI-IO-AREA2 SSA1                   
217500     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
217600     PERFORM IMS-STATUSKONTROLL                                           
217700     .                                                                    
217800     EJECT                                                                
217900 IMS-GU-XXKH-WLXXKH11 SECTION.                                            
218000                                                                          
218100     STRING 'WLXXKH01(WDGXKEY  =' W-4447-IDHTYP-X ')'                     
218200          DELIMITED BY SIZE INTO SSA1                                     
218300     STRING 'WLXXKH11(WDGXKEY  =' W-4448-IDPRC-X ')'                      
218400          DELIMITED BY SIZE INTO SSA2                                     
218500     MOVE '  GE' TO GODK-STATUSKODER                                      
218600     CALL CBLTDLI USING GU XXKH-PCB DLI-IO-AREA SSA1 SSA2                 
218700     MOVE XXKH-STATUS-CODE TO STATUS-WS                                   
218800     PERFORM IMS-STATUSKONTROLL                                           
218900     .                                                                    
219000     EJECT                                                                
219100 IMS-GHU-XXKO-WLXXKO11 SECTION.                                           
219200                                                                          
219300     STRING 'WLXXKO01(WDGXKEY  =' W-4461-IDHTYP-X ')'                     
219400          DELIMITED BY SIZE INTO SSA1                                     
219500     STRING 'WLXXKO11(WDGXKEY  =' W-4462-KDPRCGRP-X ')'                   
219600          DELIMITED BY SIZE INTO SSA2                                     
219700     MOVE '  GE' TO GODK-STATUSKODER                                      
219800     CALL CBLTDLI USING GHU XXKO-PCB DLI-IO-AREA SSA1 SSA2                
219900     MOVE XXKO-STATUS-CODE TO STATUS-WS                                   
220000     PERFORM IMS-STATUSKONTROLL                                           
220100     .                                                                    
220200                                                                          
220300                                                                          
220400 IMS-REPL-XXKO-WLXXKO11 SECTION.                                          
220500                                                                          
220600     MOVE '    ' TO GODK-STATUSKODER                                      
220700     CALL CBLTDLI USING REPL XXKO-PCB DLI-IO-AREA                         
220800     MOVE XXKO-STATUS-CODE TO STATUS-WS                                   
220900     PERFORM IMS-STATUSKONTROLL                                           
221000     .                                                                    
221100     EJECT                                                                
221200 IMS-ISRT-4001-WL400101 SECTION.                                          
221300                                                                          
221400     MOVE 'WL400101 ' TO SSA1                                             
221500     MOVE '    ' TO GODK-STATUSKODER                                      
221600     CALL CBLTDLI USING ISRT 4001-PCB DLI-IO-AREA SSA1                    
221700     MOVE 4001-STATUS-CODE TO STATUS-WS                                   
221800     PERFORM IMS-STATUSKONTROLL                                           
221900     .                                                                    
222000                                                                          
222100 IMS-ISRT-4001-WL400111 SECTION.                                          
222200                                                                          
222300     STRING 'WL400101(WDGXKEY  =' W-4001-IDHTYP-X ')'                     
222400          DELIMITED BY SIZE INTO SSA1                                     
222500     MOVE 'WL400111 ' TO SSA2                                             
222600     MOVE '    ' TO GODK-STATUSKODER                                      
222700     CALL CBLTDLI USING ISRT 4001-PCB DLI-IO-AREA3 SSA1 SSA2              
222800     MOVE 4001-STATUS-CODE TO STATUS-WS                                   
222900     PERFORM IMS-STATUSKONTROLL                                           
223000     .                                                                    
223100                                                                          
223200 IMS-GU-WDB601    SECTION.                                                
223300     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
223400          DELIMITED BY SIZE INTO SSA1                                     
223500     MOVE '  ' TO GODK-STATUSKODER                                        
223600     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
223700     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
223800     PERFORM IMS-STATUSKONTROLL                                           
223900     .                                                                    
224000     EJECT                                                                
224100 IMS-STATUSKONTROLL SECTION.                                              
224200                                                                          
224300     SET STATUS-IX TO 1                                                   
224400     SEARCH GODK-STATUS                                                   
224500       AT END CALL FELLOG                                                 
224600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
224700     END-SEARCH                                                           
224800     .                                                                    
