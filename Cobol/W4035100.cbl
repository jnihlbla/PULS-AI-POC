000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4035100.                                                
000300 AUTHOR.         ROGER OLSSON.                                            
000400 DATE-WRITTEN.   90/03/20.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION.                                                            
000800*        HÄMTAR ORDERDELAR TILL EN PLOCKSATS.                             
000900*        LÄSER PRC-KANAL FÖR ATT KOLLA PLOCKGRÄNS.                        
001000*        OM ENDAST EN ORDERDEL INGÅR I PLOCKSATSEN                        
001100*        HÄMTAS FÖRRÅDSDATATEXTEN.                                        
001200*        LÄGGER UPP ORDERDELAR PÅ ARBETSTABELL.                           
001300*                                                                         
001400*        PROGRAMMET ÄR EN UPPDATERINGS-MPP                                
001500*        PROGRAMMET UPPDATERAR WLORQA (WDQ3)                              
001600*                              WLXXKQ (WDR4)                              
001700*        PROGRAMMET LÄSER      WLXXKH (WDR1)                              
001800*                                                                         
001900*                                                                         
002000*    INDATA.                                                              
002100*        TRANSAKTION: W4T351                                              
002200*        MID:         W4I35101                                            
002300*                                                                         
002400*    UTDATA.                                                              
002500*        MOD:         W4O35101                                            
002600*                                                                         
002700*    E-TRACKER:7898645 ADDITION OF NEW FIELDS TO WDGX4002                 
002800*                      4002-IDMSG3IV 4002-IDSNO3IV 4002-ADDISPXTRA        
002900*                                                                         
003000                                                                          
003100     SKIP3                                                                
003200 ENVIRONMENT DIVISION.                                                    
003300     EJECT                                                                
003400 DATA DIVISION.                                                           
003500 WORKING-STORAGE SECTION.                                                 
003600                                                                          
003700*    -COPY WY2000W1                                                       
003800     SKIP3                                                                
003900 77  IDPGM                       PIC X(08)   VALUE 'W4035100'.            
004000                                                                          
004100 77  JA                          PIC X       VALUE 'J'.                   
004200 77  YES                         PIC X       VALUE 'Y'.                   
004300 77  NEJ                         PIC X       VALUE 'N'.                   
004400                                                                          
004500 77  IX1                         PIC S9(9)  VALUE +0    COMP SYNC.        
004600 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
004700 77  MAX-MOD-LAENGD              PIC S9(4)  VALUE +335  COMP SYNC.        
004800                                                                          
004900 01      WS-KLOCKAN.                                                      
005000   03    WS-TIHHMMSS             PIC 9(6).                                
005100   03    FILLER                  PIC X(2).                                
005200                                                                          
005300 01      WS-KLOCKAN-LOK.                                                  
005400   03    WS-TIHHMMSS-LOK         PIC 9(6).                                
005500   03    FILLER                  PIC X(2).                                
005600                                                                          
005700 01      WS-DARFS                PIC 9(12).                               
005800 01      FILLER REDEFINES WS-DARFS.                                       
005900   03    FILLER                  PIC 9(2).                                
006000   03    WS-RFS-DATUM            PIC 9(6).                                
006100   03    FILLER                  PIC 9(4).                                
006200                                                                          
006300 77      WS-PRC-KVORDER          PIC S9(7)      COMP-3 VALUE 0.           
006400 77      WS-PRC-KVRADER          PIC S9(5)      COMP-3 VALUE 0.           
006500 77      WS-PRC-VKORDNTO         PIC S9(6)V9(1) COMP-3 VALUE 0.           
006600 77      WS-PRC-VLORDNTO         PIC S9(4)V9(3) COMP-3 VALUE 0.           
006700 77      WS-PRC-KVPLSRAD         PIC S9(5)      COMP-3 VALUE 0.           
006800 77      WS-PRC-VKPLSNTO         PIC S9(6)V9(1) COMP-3 VALUE 0.           
006900 77      WS-PRC-VLPLSNTO         PIC S9(4)V9(3) COMP-3 VALUE 0.           
007000 77      WS-PRC-SPLITGRANS       PIC S9(7)V9(3) COMP-3.                   
007100 77      WS-PRC-RESPLIT          PIC S9(1)V9(2) COMP-3.                   
007200                                                                          
007300 77      WS-KVORDER              PIC S9(7)      COMP-3 VALUE 0.           
007400 77      WS-KVRADER              PIC S9(5)      COMP-3 VALUE 0.           
007500 77      WS-VKORDNTO             PIC S9(6)V9(1) COMP-3 VALUE 0.           
007600 77      WS-VLORDNTO             PIC S9(4)V9(3) COMP-3 VALUE 0.           
007700 77      WS-SPLITGRANS           PIC S9(7)V9(3) COMP-3.                   
007800 77      WS-SPLITREST            PIC S9(7)V9(3) COMP-3.                   
007900 77      WS-RESPLIT              PIC S9(1)V9(2) COMP-3.                   
008000 77      WS-PROC-RAD             PIC S9(8)V9    COMP-3.                   
008100 77      WS-PROC-VIKT            PIC S9(8)V9    COMP-3.                   
008200 77      WS-PROC-VOLYM           PIC S9(8)V9    COMP-3.                   
008300 77      WS-ANTSPLIT             PIC  9(8)V99.                            
008400 77      WS-KDSORT               PIC  X(2)      VALUE SPACE.              
008500 77      WS-SPAR-IDPRCVAR        PIC  X(1).                               
008600 77  WS-KDMATT                   PIC X(1).                                
008700     88 US-MATT                  VALUE 'U'.                               
008800                                                                          
008900 77      WS-DATUM                PIC 9(6).                                
009000 77      WS-DATUM-LOK            PIC 9(6).                                
009100                                                                          
009200*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
009300*                                                                         
009400*      --- VALID IDDC CODES                                               
009500*                                                                         
009600*01    -COPY WWDC99                                                       
009700       EJECT                                                              
009800*                                                                         
009900 01      WS-IDPRC.                                                        
010000   03    WS-IDPRCBAS             PIC X(03).                               
010100   03    WS-IDPRCVAR             PIC X(01).                               
010200                                                                          
010300 01      WS-IDUSER               PIC X(08).                               
010400                                                                          
010500 01      WS-IDBORD               PIC X(03).                               
010600                                                                          
010700 77  INDATA-SW                   PIC X       VALUE 'J'.                   
010800     88  INDATA-OK                           VALUE 'J'.                   
010900     88  INDATA-FEL                          VALUE 'N'.                   
011000                                                                          
011100 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
011200     88  NYCKLAR-OK                          VALUE 'J'.                   
011300     88  NYCKLAR-FEL                         VALUE 'N'.                   
011400                                                                          
011500 77  PRC-FEL-SW                  PIC X       VALUE 'J'.                   
011600     88  PRC-FEL                             VALUE 'N'.                   
011700                                                                          
011800 77  ALLT-SW                     PIC X       VALUE 'J'.                   
011900     88  ALLT-OK                             VALUE 'J'.                   
012000                                                                          
012100 77  PLOCKGRANS-SW               PIC X       VALUE 'N'.                   
012200     88  PLOCKGRANS                          VALUE 'J'.                   
012300                                                                          
012400 77  SPLIT-SW                    PIC X       VALUE 'N'.                   
012500     88  SPLIT                               VALUE 'J'.                   
012600     88  EJ-SPLIT                            VALUE 'N'.                   
012700                                                                          
012800 77  ORDERDEL-SW                 PIC X       VALUE 'N'.                   
012900     88 ORDERDEL-FINNS                       VALUE 'N'.                   
013000     88 ORDERDEL-SLUT                        VALUE 'J'.                   
013100                                                                          
013200 77  OMSTART-SW                  PIC X       VALUE 'N'.                   
013300     88  OMSTART                             VALUE 'J'.                   
013400                                                                          
013500 77  FORSTA-SW                   PIC X       VALUE 'J'.                   
013600     88  FORSTA-GANG                         VALUE 'J'.                   
013700                                                                          
013800 77  PRC-BRYT-SW                 PIC X       VALUE 'N'.                   
013900     88  PRC-BRYTNING                        VALUE 'J'.                   
014000                                                                          
014100 77  LAES-SW                     PIC X       VALUE 'N'.                   
014200     88  LAES-OK                             VALUE 'J'.                   
014300                                                                          
014400 77  C2-RFS-SW                   PIC X       VALUE 'N'.                   
014500     88  C2-OK                               VALUE 'J'.                   
014600     88  C2-EJ-OK                            VALUE 'N'.                   
014700                                                                          
014800 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
014900     88  EGEN-MID                            VALUE '4351'.                
015000     88  GODK-MID                            VALUE '4351'.                
015110                                                                          
015120 01  WS-IDPRTLST.                                                         
015130     03 WS-SYSTDEL               PIC X(1).                                
015140     03 WS-LISTTYP               PIC X(2).                                
015150     03 WS-KDPRT                 PIC X(3).                                
015160     03 FILLER                   PIC X(2)    VALUE SPACE.                 
015161     EJECT                                                                
015170                                                                          
015200*                                                                         
015300*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
015400 01  GENERELLA-SUBPROGRAM.                                                
015410     03  W006PRT                 PIC X(8)    VALUE 'W006PRT '.            
015500     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
015600     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
015700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
015800     03 W005INIT                 PIC X(8)    VALUE 'W005INIT'.            
015900     EJECT                                                                
016000 01  FILLER                      PIC X(16)  VALUE 'WWOMVAND'.             
016100*    --- PARAMETRAR WWOMVAND                                              
016200*01 -COPY WWOMVAND                                                        
016300     EJECT                                                                
016400*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
016500*01 -COPY WMSGINIT                                                        
016600     EJECT                                                                
016700*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
016800*   -COPY WMEDAREA                                  X                     
016810     EJECT                                                                
016820*    --- PARAMETRAR TILL SUBPROGRAM W006PRT                               
016830                                                                          
016840*   -COPY W006PRT                                                         
016900     EJECT                                                                
017000*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
017100*                                                                         
017200 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
017300     SKIP3                                                                
017400*01  MID -COPY W4I35101                                                   
017500     EJECT                                                                
017600 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
017700     SKIP3                                                                
017800*01  -COPY WMSGAREA                                                       
017900     EJECT                                                                
018000*    03  MOD -COPY W4O35101   -RED MSG-AREA.                              
018100     EJECT                                                                
018200 01  FILLER                      PIC X(16)   VALUE 'P-TO-P-SW1'.          
018300 01  P-TO-P-SW1.                                                          
018400     03  PTOP1-LL                PIC S9(4)   VALUE 28 COMP SYNC.          
018500     03  PTOP1-Z1                PIC  X(1)   VALUE LOW-VALUE.             
018600     03  PTOP1-Z2                PIC  X(1)   VALUE LOW-VALUE.             
018700     03  PTOP1-TRANSKOD          PIC  X(7)   VALUE 'W4T375U'.             
018800     03  FILLER                  PIC  X(1)   VALUE SPACE.                 
018900     03  FILLER                  PIC  X(4)   VALUE '4351'.                
019000     03  PTOP1-KDMFSFOR          PIC  X(1).                               
019100*    03  -COPY W4I37501  -PRE PTOP1-                                      
019200                                                                          
019300 01  FILLER                      PIC X(16)   VALUE 'P-TO-P-SW2'.          
019400 01  P-TO-P-SW2.                                                          
019500     03  PTOP2-LL                PIC S9(4)   VALUE 128 COMP SYNC.         
019600     03  PTOP2-Z1                PIC  X(1)   VALUE LOW-VALUE.             
019700     03  PTOP2-Z2                PIC  X(1)   VALUE LOW-VALUE.             
019800     03  PTOP2-TRANSKOD          PIC  X(7)   VALUE 'W4T351 '.             
019900     03  FILLER                  PIC  X(1)   VALUE SPACE.                 
020000     03  FILLER                  PIC  X(4)   VALUE '4351'.                
020100     03  PTOP2-KDMFSFOR          PIC  X(1).                               
020200*    03  -COPY W4I35101  -PRE PTOP2-                                      
020300     EJECT                                                                
020400 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
020500                                                                          
020600*01  -COPY WMFSAREAC0                                                     
020700     EJECT                                                                
020800*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
020900*                                                                         
021000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
021100     SKIP3                                                                
021200 01  NYCKLAR-TILL-DLI.                                                    
021300                                                                          
021400*----> PLOCKSATS                                                          
021500                                                                          
021600     03  W-4001-IDHTYP-X.                                                 
021700         05  W-4001-IDHTYP           PIC  X(4)  VALUE '4001'.             
021800         05  W-4001-IDPRODNR         PIC  9(7).                           
021900         05  W-4001-IDPLKLST         PIC  9(3).                           
022000         05  FILLER                  PIC  X(16) VALUE LOW-VALUE.          
022100                                                                          
022200*----> DIREKTNYCKEL ORDERHUVUD.                                           
022300                                                                          
022400     03  W-IDORDER-X.                                                     
022500         05  W-IDORDER               PIC S9(7)  COMP-3.                   
022600                                                                          
022700     03  W-IDDC-X.                                                        
022800         05  W-IDDC                  PIC X(2).                            
022900                                                                          
023000*----> DIREKTNYCKEL TILL ORDERDEL.                                        
023100                                                                          
023200     03  W-WDQ301KY-X.                                                    
023300         05  W-Q301KY-IDORDER        PIC S9(7)  COMP-3.                   
023400         05  W-Q301KY-IDDC           PIC X(2).                            
023500         05  W-Q301KY-IDPRODNR       PIC S9(7)  COMP-3.                   
023600         05  W-Q301KY-IDPLKLST       PIC S9(3)  COMP-3.                   
023700                                                                          
023800                                                                          
023900*----> SEKUNDÄR INDEX TILL ORDERDEL.                                      
024000                                                                          
024100     03  W-WDQ3BSEQ-MIN-X.                                                
024200         05  W-Q3BSEQ-MIN-IDDC       PIC X(2).                            
024300         05  W-Q3BSEQ-MIN-IDPRCBAS   PIC X(3).                            
024400         05  W-Q3BSEQ-MIN-DAUTSKR    PIC  9(8).                           
024500         05  W-Q3BSEQ-MIN-TIUTSTID   PIC S9(7)  COMP-3.                   
024600         05  W-Q3BSEQ-MIN-DARFS      PIC  9(12).                          
024700         05  W-Q3BSEQ-MIN-DALSTORD   PIC  9(12).                          
024800         05  FILLER                  PIC X(1).                            
024900                                                                          
025000     03      W-Q3BSEQ-MIN-IDPRCVAR   PIC X(1).                            
025100                                                                          
025200     03  W-WDQ3BSEQ-MAX-X.                                                
025300         05  W-Q3BSEQ-MAX-IDDC       PIC X(2).                            
025400         05  W-Q3BSEQ-MAX-IDPRCBAS   PIC X(3).                            
025500         05  W-Q3BSEQ-MAX-DAUTSKR    PIC  9(8).                           
025600         05  W-Q3BSEQ-MAX-TIUTSTID   PIC S9(7)  COMP-3.                   
025700         05  W-Q3BSEQ-MAX-DARFS      PIC  9(12).                          
025800         05  W-Q3BSEQ-MAX-DALSTORD   PIC  9(12).                          
025900         05  FILLER                  PIC X(1).                            
026000                                                                          
026100     03      W-Q3BSEQ-MAX-IDPRCVAR   PIC X(1).                            
026200                                                                          
026300*----> PRC-KANALEN.                                                       
026400                                                                          
026500     03  W-4447-IDHTYP-X.                                                 
026600         05  W-4447-IDHTYP       PIC  X(04) VALUE '4447'.                 
026700         05  W-4447-IDDC         PIC  X(02).                              
026800         05  W-4447-LOW-VALUE    PIC  X(24) VALUE LOW-VALUE.              
026900                                                                          
027000     03  W-4448-IDPRC-X.                                                  
027100         05  W-4448-IDPRC        PIC  X(04).                              
027200         05  W-4448-LOW-VALUE    PIC  X(01) VALUE LOW-VALUE.              
027300                                                                          
027400*----> FÖRRÅDSDATATEXT.                                                   
027500                                                                          
027600     03  W-4535-IDHTYP-X.                                                 
027700         05  W-4535-IDHTYP       PIC  X(04) VALUE '4535'.                 
027800         05  W-4535-KDFDKRAV     PIC S9(03) COMP-3.                       
027900         05  W-4535-LOW-VALUE    PIC  X(24) VALUE LOW-VALUE.              
028000                                                                          
028100     03  W-4536-IDSKYLT-X.                                                
028200         05  W-4536-IDSKYLT      PIC  X(03).                              
028300         05  W-4536-LOW-VALUE    PIC  X(02) VALUE LOW-VALUE.              
028400                                                                          
028500*----> PLOCKSATSENS LÖPNR INOM PRCGRUPP.                                  
028600                                                                          
028700     03  W-4461-IDHTYP-X.                                                 
028800         05  W-4461-IDHTYP       PIC  X(04) VALUE '4461'.                 
028900         05  W-4461-IDDC         PIC  X(02).                              
029000         05  W-4461-LOW-VALUE    PIC  X(24) VALUE LOW-VALUE.              
029100                                                                          
029200     03  W-4462-KDPRCGRP-X       PIC  X(05).                              
029300                                                                          
029400*    --- STATUS-KOD FRÅN IMS                                              
029500 01  STATUS-WS                   PIC XX.                                  
029600     88  SEGMENT-FINNS                       VALUE '  '.                  
029700     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
029800     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
029900     88  END-OF-DATA                         VALUE 'GB'.                  
030000     SKIP2                                                                
030100 01  GODK-STATUSKODER.                                                    
030200     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
030300     SKIP3                                                                
030400 01  SSA1                        PIC X(160).                              
030500 01  SSA2                        PIC X(160).                              
030600     EJECT                                                                
030700****************************                                              
030800*  ARBETSAREA FÖR ORDERDEL *                                              
030900****************************                                              
031000 01  W-ORDERDEL.                                                          
031100*    03  -COPY WDQ301     -PRE W-                                         
031200     EJECT                                                                
031300*    --- IMS FUNKTIONSKODER                                               
031400*01  -COPY W0003                                                          
031500     EJECT                                                                
031600*    ---  DLI INPUT-OUTPUT AREA                                           
031700 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
031800     SKIP3                                                                
031900 01  DLI-IO-AREA.                                                         
032000     03  IO-AREA                 PIC X(1500) VALUE SPACE.                 
032100     SKIP3                                                                
032200     03  WLXXKH01 REDEFINES IO-AREA.                                      
032300*        05  -COPY WDGX4447   -PRE XXKH-                                  
032400     EJECT                                                                
032500     03  WLXXKH11 REDEFINES IO-AREA.                                      
032600*        05  -COPY WDGX4448   -PRE XXKH-                                  
032700     EJECT                                                                
032800     03  WLXXKQ01 REDEFINES IO-AREA.                                      
032900*        05  -COPY WDGX01     -PRE XXKQ-                                  
033000     EJECT                                                                
033100     03  WLXXKU01 REDEFINES IO-AREA.                                      
033200*        05  -COPY WDGX4535   -PRE XXKU-                                  
033300     EJECT                                                                
033400     03  WLXXKU11 REDEFINES IO-AREA.                                      
033500*        05  -COPY WDGX4536   -PRE XXKU-                                  
033600     EJECT                                                                
033700     03  WLXXKO01 REDEFINES IO-AREA.                                      
033800*        05  -COPY WDGX4461   -PRE XXKO-                                  
033900     EJECT                                                                
034000     03  WLXXKO11 REDEFINES IO-AREA.                                      
034100*        05  -COPY WDGX4462   -PRE XXKO-                                  
034200     EJECT                                                                
034300     03  WL400101 REDEFINES IO-AREA.                                      
034400*        05  -COPY WDGX4001   -PRE 4001-                                  
034500     EJECT                                                                
034600 01  DLI-IO-AREA1.                                                        
034700     03  WLORQA01.                                                        
034800*        05  -COPY WDQ301     -PRE ORQA-                                  
034900     EJECT                                                                
035000 01  DLI-IO-AREA2.                                                        
035100*    03 -COPY WDQ201     -PRE ORQI-                                       
035200*    03 -COPY WDQ212     -PRE ORQI-                                       
035300     EJECT                                                                
035400 01  DLI-IO-AREA3.                                                        
035500     03 WL400111.                                                         
035600*        05 -COPY WDGX4002                                                
035700     EJECT                                                                
035800 LINKAGE SECTION.                                                         
035900                                                                          
036000*01  -COPY W0009      -PRE MSG-                                           
036100     EJECT                                                                
036200*01  -COPY W0009      -PRE ALT1-                                          
036300     EJECT                                                                
036400*01  -COPY W0009      -PRE ALT2-                                          
036500     EJECT                                                                
036600*01  -COPY W0008      -PRE USEA-                                          
036700     05  FILLER                  PIC X.                                   
036800     EJECT                                                                
036900*01  -COPY W0008      -PRE ORQ-                                           
037000     05  FILLER                  PIC X.                                   
037100     EJECT                                                                
037200*01  -COPY W0008      -PRE ORQB-                                          
037300     05  FILLER                  PIC X.                                   
037400     EJECT                                                                
037500*01  -COPY W0008      -PRE XXKH-                                          
037600     05  FILLER                  PIC X.                                   
037700     EJECT                                                                
037800*01  -COPY W0008      -PRE 4001-                                          
037900     05  FILLER                  PIC X.                                   
038000     EJECT                                                                
038100*01  -COPY W0008      -PRE XXKU-                                          
038200     05  FILLER                  PIC X.                                   
038300     EJECT                                                                
038400*01  -COPY W0008      -PRE XXKO-                                          
038500     05  FILLER                  PIC X.                                   
038600     EJECT                                                                
038700*01  -COPY W0008      -PRE ORQI-                                          
038800     05  FILLER                  PIC X.                                   
038900     EJECT                                                                
039000 PROCEDURE DIVISION  USING MSG-PCB                                        
039100                           ALT1-PCB                                       
039200                           ALT2-PCB                                       
039300                           USEA-PCB                                       
039400                           ORQ-PCB                                        
039500                           ORQB-PCB                                       
039600                           XXKH-PCB                                       
039700                           4001-PCB                                       
039800                           XXKU-PCB                                       
039900                           XXKO-PCB                                       
040000                           ORQI-PCB.                                      
040100                                                                          
040200     ENTRY 'DLITCBL' USING MSG-PCB                                        
040300                           ALT1-PCB                                       
040400                           ALT2-PCB                                       
040500                           USEA-PCB                                       
040600                           ORQ-PCB                                        
040700                           ORQB-PCB                                       
040800                           XXKH-PCB                                       
040900                           4001-PCB                                       
041000                           XXKU-PCB                                       
041100                           XXKO-PCB                                       
041200                           ORQI-PCB.                                      
041300                                                                          
041400     PERFORM IMS-GU-MSG                                                   
041500     IF SEGMENT-FINNS                                                     
041600        PERFORM A-INIT                                                    
041700        PERFORM B-KOLLA-NYCKLAR                                           
041800        IF NYCKLAR-OK                                                     
041900           PERFORM C-INIT-PLOCKSATSREG                                    
042000           PERFORM D-LAES-VISA-INFO                                       
042100        END-IF                                                            
042200        IF OMSTART                                                        
042300           PERFORM E-OMSTART-W40351                                       
042400        ELSE                                                              
042500           MOVE MAX-MOD-LAENGD TO MSG-KVLL                                
042600           PERFORM IMS-ISRT-MSG                                           
042700        END-IF                                                            
042800     END-IF                                                               
042900                                                                          
043000     MOVE ZERO TO RETURN-CODE                                             
043100     GOBACK                                                               
043200     .                                                                    
043300     EJECT                                                                
043400 A-INIT SECTION.                                                          
043500                                                                          
043600     IF MSG-DUBBLA-TRANSKODER                                             
043700        MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I35101                
043800        MOVE MSG-IDTRANS-2                 TO MFS-IDTRANS                 
043900        MOVE MSG-KDMFSFOR-2                TO MFS-KDMFSFOR                
044000     ELSE                                                                 
044100        MOVE MSG-INDATA-MINUS-1-TRANSKOD   TO MID-W4I35101                
044200        MOVE MSG-IDTRANS-1                 TO MFS-IDTRANS                 
044300        MOVE MSG-KDMFSFOR-1                TO MFS-KDMFSFOR                
044400     END-IF                                                               
044500                                                                          
044600     MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                             
044700     MOVE MSG-IDPFK            TO MFS-IDPFK                               
044800     MOVE MFS-IDTRANS          TO W-IDTRANS                               
044900                                                                          
045000     MOVE LOW-VALUE       TO MSG-AREA                                     
045100     MOVE 'W4O351N1'      TO MFS-IDMOD                                    
045200     MOVE '4351'          TO MOD-IDTRANS                                  
045300     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL                                 
045400                             MOD-TEMFSINF                                 
045500                                                                          
045600     IF NOT EGEN-MID                                                      
045700        MOVE SPACE TO MFS-KDTRTYP                                         
045800        MOVE '7'   TO MFS-IDPFK                                           
045900     END-IF                                                               
046000                                                                          
046100     MOVE NEJ          TO PTOP1-MID-FLSVAR                                
046200     MOVE MFS-KDMFSFOR TO PTOP1-KDMFSFOR                                  
046300                          PTOP2-KDMFSFOR                                  
046400                                                                          
046500     ACCEPT WS-DATUM   FROM DATE                                          
046600     ACCEPT WS-KLOCKAN FROM TIME                                          
046700     MOVE NEJ TO MOD-FLORDKNY                                             
046800                 MOD-MIXAT                                                
046900                 MOD-UTSKR-EJ-KOMPL-PS                                    
047000     .                                                                    
047100     EJECT                                                                
047200 B-KOLLA-NYCKLAR SECTION.                                                 
047300                                                                          
047400     MOVE ALL '+'           TO MSGI-WMSGINIT                              
047500     MOVE '001'             TO MSGI-KDCALL                                
047600     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
047700     MOVE '4351'            TO MSGI-IDTRANS                               
047800     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
047900     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
048000     MOVE MSGI-KDMATT       TO WS-KDMATT                                  
048100                                                                          
048200     IF MSGI-IDLAND-SPR = 'GB'                                            
048300        MOVE +2    TO SPRAK-IX                                            
048400        MOVE 'GB ' TO MED-IDSKYLT                                         
048500     ELSE                                                                 
048600        MOVE +1    TO SPRAK-IX                                            
048700        MOVE 'S  ' TO MED-IDSKYLT                                         
048800     END-IF                                                               
048900                                                                          
049000     MOVE JA TO NYCKLAR-SW                                                
049100                                                                          
049200     MOVE LOW-VALUE  TO W-WDQ3BSEQ-MIN-X                                  
049300                        W-Q3BSEQ-MIN-IDPRCVAR                             
049400                                                                          
049500     MOVE HIGH-VALUE TO W-WDQ3BSEQ-MAX-X                                  
049600                        W-Q3BSEQ-MAX-IDPRCVAR                             
049700                                                                          
049800     MOVE MFS-RENSA-FAELT TO MOD-IDDC-IN                                  
049900                             MOD-IDPRC-IN                                 
050000                             MOD-IDUSER-IN                                
050100                             MOD-IDBORD-IN                                
050200                                                                          
050300     PERFORM BD-KOLLA-IDDC                                                
050400                                                                          
050500     MOVE '011'                TO MSGI-KDCALL                             
050600     MOVE MSG-SIGNON-USERID    TO MSGI-IDUSER                             
050700                                  MSGI-IDLTERM-USER                       
050800     MOVE WS-DATUM             TO MSGI-TILOKDAT                           
050900     MOVE WS-TIHHMMSS(1:4)     TO MSGI-TILOKTID                           
051000     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
051100     MOVE MSGI-TILOKDAT        TO WS-DATUM-LOK                            
051200     MOVE WS-TIHHMMSS          TO WS-TIHHMMSS-LOK                         
051300     MOVE MSGI-TILOKTID        TO WS-TIHHMMSS-LOK(1:4)                    
051400                                                                          
051500     PERFORM BA-KOLLA-PRC                                                 
051600                                                                          
051700     PERFORM BB-KOLLA-PACKARE                                             
051800                                                                          
051900     PERFORM BC-KOLLA-BORD                                                
052020                                                                          
052100     IF NOT GODK-MID                                                      
052200        MOVE NEJ          TO NYCKLAR-SW                                   
052300     END-IF                                                               
052400                                                                          
052500     IF NYCKLAR-OK                                                        
052600        PERFORM BD-KOLLA-OMSTART                                          
052700     END-IF                                                               
052800                                                                          
052900     IF GODK-MID OR NYCKLAR-OK                                            
053000        MOVE WS-IDDC         TO MOD-IDDC-UT                               
053100        MOVE WS-IDPRC        TO MOD-IDPRC-UT                              
053200        MOVE WS-IDUSER       TO MOD-IDUSER-UT                             
053300        MOVE WS-IDBORD       TO MOD-IDBORD-UT                             
053400        INSPECT MOD-IDPRC-UT REPLACING LEADING ZERO BY SPACE              
053500        INSPECT MOD-IDUSER-UT REPLACING LEADING ZERO BY SPACE             
053600     ELSE                                                                 
053700        MOVE MFS-RENSA-FAELT TO MOD-IDDC-UT                               
053800                                MOD-IDPRC-UT                              
053900                                MOD-IDUSER-UT                             
054000                                MOD-IDBORD-UT                             
054100     END-IF                                                               
054200                                                                          
054300     IF NYCKLAR-FEL                                                       
054400        IF GODK-MID                                                       
054500           IF PRC-FEL                                                     
054600              MOVE '324' TO MED-IDMFSFEL                                  
054700           ELSE                                                           
054800              MOVE '401' TO MED-IDMFSFEL                                  
054900           END-IF                                                         
055000           PERFORM S02-FEL-MEDDELANDE                                     
055100           PERFORM MFS-RENSA-FAELT-UT                                     
055200        END-IF                                                            
055300     END-IF                                                               
055400     .                                                                    
055500     EJECT                                                                
055600 BD-KOLLA-IDDC   SECTION.                                                 
055700                                                                          
055800     MOVE MSGI-IDDC                 TO WS-IDDC                            
055900                                                                          
056000     IF WS-IDDC > SPACE                                                   
056100       CONTINUE                                                           
056200     ELSE                                                                 
056300       MOVE NEJ                     TO NYCKLAR-SW                         
056400     END-IF                                                               
056500                                                                          
056600     IF NYCKLAR-OK                                                        
056700       MOVE WS-IDDC                 TO W-IDDC                             
056800     END-IF                                                               
056900     .                                                                    
057000     EJECT                                                                
057100 BA-KOLLA-PRC SECTION.                                                    
057200                                                                          
057300     IF MID-IDPRC-IN = ALL '+'                                            
057400        MOVE MID-IDPRC-UT TO WS-IDPRC                                     
057500     ELSE                                                                 
057600        MOVE MID-IDPRC-IN TO WS-IDPRC                                     
057700        MOVE SPACE        TO MFS-KDTRTYP                                  
057800     END-IF                                                               
057900                                                                          
058000     IF WS-IDPRCBAS NUMERIC AND WS-IDPRCBAS > ZERO                        
058100*PRC 9998 = DIREKTLEV UTSKRIFT. SKALL SKRIVAS UT AV PGM 4695.             
058200       IF NDC                                                             
058300       AND WS-IDPRCBAS = '999' AND WS-IDPRCVAR = '8'                      
058400          MOVE NEJ        TO NYCKLAR-SW                                   
058500                             PRC-FEL-SW                                   
058600       ELSE                                                               
058700          MOVE WS-IDPRCBAS TO W-Q3BSEQ-MIN-IDPRCBAS                       
058800          MOVE WS-IDPRCBAS TO W-Q3BSEQ-MAX-IDPRCBAS                       
058900       END-IF                                                             
059000     ELSE                                                                 
059100        MOVE NEJ          TO NYCKLAR-SW                                   
059200     END-IF                                                               
059300                                                                          
059400     IF WS-IDPRCVAR NOT = SPACE                                           
059500        MOVE WS-IDPRCVAR  TO W-Q3BSEQ-MIN-IDPRCVAR                        
059600        MOVE WS-IDPRCVAR  TO W-Q3BSEQ-MAX-IDPRCVAR                        
059700     END-IF                                                               
059800     .                                                                    
059900     EJECT                                                                
060000 BB-KOLLA-PACKARE SECTION.                                                
060100                                                                          
060200     IF MID-IDUSER-IN = ALL '+'                                           
060300        MOVE MID-IDUSER-UT TO WS-IDUSER                                   
060400     ELSE                                                                 
060500        MOVE MID-IDUSER-IN TO WS-IDUSER                                   
060600        MOVE SPACE         TO MFS-KDTRTYP                                 
060700     END-IF                                                               
060800                                                                          
060900     INSPECT WS-IDUSER REPLACING LEADING SPACE BY ZERO                    
061000                                                                          
061100     IF WS-IDUSER NOT NUMERIC                                             
061200        MOVE NEJ          TO NYCKLAR-SW                                   
061300     END-IF                                                               
061400                                                                          
061500     IF WS-IDUSER (1:3) NOT = ZERO                                        
061600        MOVE NEJ          TO NYCKLAR-SW                                   
061700     END-IF                                                               
061800                                                                          
061900     IF WS-IDUSER (4:5) = ZERO                                            
062000        MOVE NEJ             TO NYCKLAR-SW                                
062100     END-IF                                                               
062200     .                                                                    
062300     EJECT                                                                
062400 BC-KOLLA-BORD SECTION.                                                   
062500                                                                          
062600     IF MID-IDBORD-IN = ALL '+'                                           
062700        MOVE MID-IDBORD-UT TO WS-IDBORD                                   
062800     ELSE                                                                 
062900        MOVE MID-IDBORD-IN TO WS-IDBORD                                   
063000        MOVE SPACE         TO MFS-KDTRTYP                                 
063100        IF WS-IDBORD NOT NUMERIC                                          
063200           MOVE NEJ        TO NYCKLAR-SW                                  
063300        END-IF                                                            
063400     END-IF                                                               
063500     .                                                                    
063600     EJECT                                                                
063700 BD-KOLLA-OMSTART SECTION.                                                
063800                                                                          
063900        IF MID-IDDC-BSEQ       = ALL '+'                                  
064000           MOVE LOW-VALUE         TO W-Q3BSEQ-MIN-IDDC                    
064100        ELSE                                                              
064200           MOVE MID-IDDC-BSEQ     TO W-Q3BSEQ-MIN-IDDC                    
064300        END-IF                                                            
064400                                                                          
064500        IF WS-IDPRCBAS = SPACE                                            
064600          IF MID-IDPRCBAS-BSEQ   = ALL '+'                                
064700             MOVE SPACE           TO W-Q3BSEQ-MIN-IDPRCBAS                
064800          ELSE                                                            
064900             MOVE MID-IDPRCBAS-BSEQ TO W-Q3BSEQ-MIN-IDPRCBAS              
065000          END-IF                                                          
065100        ELSE                                                              
065200          MOVE WS-IDPRCBAS        TO W-Q3BSEQ-MIN-IDPRCBAS                
065300        END-IF                                                            
065400                                                                          
065500        IF MID-TIUTSKR-BSEQ    = ALL '+'                                  
065600           MOVE ZERO              TO W-Q3BSEQ-MIN-DAUTSKR                 
065700        ELSE                                                              
065800           MOVE MID-TIUTSKR-BSEQ  TO W-Q3BSEQ-MIN-DAUTSKR                 
065900           IF MID-TIUTSKR-BSEQ NOT = ZERO                                 
066000             IF MID-TIUTSKR-BSEQ < 500000                                 
066100               MOVE 20            TO W-Q3BSEQ-MIN-DAUTSKR (1:2)           
066200             ELSE                                                         
066300               IF MID-TIUTSKR-BSEQ < 999999                               
066400                 MOVE 19          TO W-Q3BSEQ-MIN-DAUTSKR (1:2)           
066500               ELSE                                                       
066600                 MOVE 99999999    TO W-Q3BSEQ-MIN-DAUTSKR                 
066700               END-IF                                                     
066800             END-IF                                                       
066900           END-IF                                                         
067000        END-IF                                                            
067100                                                                          
067200        IF MID-TIUTSTID-BSEQ   = ALL '+'                                  
067300           MOVE ZERO              TO W-Q3BSEQ-MIN-TIUTSTID                
067400        ELSE                                                              
067500           MOVE MID-TIUTSTID-BSEQ TO W-Q3BSEQ-MIN-TIUTSTID                
067600        END-IF                                                            
067700                                                                          
067800        IF MID-TIRFS-BSEQ      = ALL '+'                                  
067900           MOVE ZERO              TO W-Q3BSEQ-MIN-DARFS                   
068000        ELSE                                                              
068100           MOVE MID-TIRFS-BSEQ    TO W-Q3BSEQ-MIN-DARFS                   
068200           IF MID-TIRFS-BSEQ NOT = ZERO                                   
068300             IF MID-TIRFS-BSEQ < 5000000000                               
068400               MOVE 20            TO W-Q3BSEQ-MIN-DARFS (1:2)             
068500             ELSE                                                         
068600               IF MID-TIRFS-BSEQ < 9999999999                             
068700                 MOVE 19          TO W-Q3BSEQ-MIN-DARFS (1:2)             
068800               ELSE                                                       
068900                 MOVE 999999999999 TO W-Q3BSEQ-MIN-DARFS                  
069000               END-IF                                                     
069100             END-IF                                                       
069200           END-IF                                                         
069300        END-IF                                                            
069400                                                                          
069500        IF MID-TILST-O-BSEQ    = ALL '+'                                  
069600           MOVE ZERO              TO W-Q3BSEQ-MIN-DALSTORD                
069700        ELSE                                                              
069800           MOVE MID-TILST-O-BSEQ  TO W-Q3BSEQ-MIN-DALSTORD                
069900           IF MID-TILST-O-BSEQ NOT = ZERO                                 
070000             IF MID-TILST-O-BSEQ < 5000000000                             
070100               MOVE 20            TO W-Q3BSEQ-MIN-DALSTORD (1:2)          
070200             ELSE                                                         
070300               IF MID-TILST-O-BSEQ < 9999999999                           
070400                 MOVE 19          TO W-Q3BSEQ-MIN-DALSTORD (1:2)          
070500               ELSE                                                       
070600                 MOVE 999999999999 TO W-Q3BSEQ-MIN-DALSTORD               
070700               END-IF                                                     
070800             END-IF                                                       
070900           END-IF                                                         
071000        END-IF                                                            
071100                                                                          
071200        IF WS-IDPRCVAR = SPACE                                            
071300          IF MID-IDPRCVAR-BSEQ   = ALL '+'                                
071400             MOVE SPACE           TO W-Q3BSEQ-MIN-IDPRCVAR                
071500          ELSE                                                            
071600             MOVE MID-IDPRCVAR-BSEQ TO W-Q3BSEQ-MIN-IDPRCVAR              
071700          END-IF                                                          
071800        ELSE                                                              
071900          MOVE WS-IDPRCVAR        TO W-Q3BSEQ-MIN-IDPRCVAR                
072000        END-IF                                                            
072100                                                                          
072200     .                                                                    
072330     EJECT                                                                
072400 C-INIT-PLOCKSATSREG SECTION.                                             
072500                                                                          
072600     MOVE WS-IDUSER    TO 4002-IDUSER                                     
072700     MOVE WS-IDBORD    TO 4002-IDBORD                                     
072800     MOVE MID-FLORDKNY TO 4002-FLORDKNY                                   
072900                                                                          
073000     MOVE 1            TO 4002-KDSEGKEY                                   
073100     MOVE NEJ          TO 4002-FLORDSPL                                   
073200     MOVE SPACE        TO 4002-KDPRT-PLE                                  
073300                          4002-KDPRT-PU                                   
073400                          4002-KDSORT                                     
073500                                                                          
073600     INITIALIZE           4002-DEAL-PR-SUM                                
073700     MOVE ZERO         TO 4002-IDLOPNR-PL                                 
073800                          4002-IXHEL                                      
073900                          4002-KVORDSPL                                   
074000                          4002-KVRADER                                    
074100                          4002-VKORDNTO                                   
074200                          4002-VLORDNTO                                   
074300                          4002-SUORDV                                     
074400                          4002-IDMSG3IV                                   
074410                          4002-IDSNO3IV                                   
074420                          4002-ADDISPXTRA                                 
074500                                                                          
074600     MOVE 1 TO IX1                                                        
074700     PERFORM UNTIL IX1 > 99                                               
074800        MOVE LOW-VALUE TO 4002-ORDDEL (IX1)                               
074900        ADD 1 TO IX1                                                      
075000     END-PERFORM                                                          
075100     .                                                                    
075200     EJECT                                                                
075300 D-LAES-VISA-INFO SECTION.                                                
075400                                                                          
075500     PERFORM DA-KOLLA-INPUT-PARAMETRAR                                    
075600                                                                          
075700     IF INDATA-OK                                                         
075800        PERFORM DB-LAES-FORSTA-ORDERDEL                                   
075900        IF ORDERDEL-FINNS                                                 
076000           PERFORM DC-LAES-PRC-KANAL                                      
076100           IF SEGMENT-FINNS                                               
076200              PERFORM DD-KOLLA-PLOCKGRANS                                 
076300              PERFORM DE-REDIGERA-INFOTEXTER                              
076400              IF PLOCKGRANS                                               
076500                 PERFORM DG-UPPLAGG-PLOCKSATSREG                          
076600                 PERFORM DH-UPPDAT-ORDERDEL                               
076700              ELSE                                                        
076800                 PERFORM DG-UPPLAGG-PLOCKSATSREG                          
076900                 PERFORM DH-UPPDAT-ORDERDEL                               
077000                 MOVE ORQA-ODEL-IDPRCVAR TO WS-SPAR-IDPRCVAR              
077100                 IF W-Q3BSEQ-MAX-IDPRCVAR = HIGH-VALUE                    
077200                    MOVE LOW-VALUE TO W-Q3BSEQ-MIN-IDPRCVAR               
077300                 END-IF                                                   
077400                 PERFORM UNTIL PLOCKGRANS                                 
077500                               OR                                         
077600                               ORDERDEL-SLUT                              
077700                               OR                                         
077800                               PRC-BRYTNING                               
077900                    PERFORM DF-LAES-NASTA-ORDERDEL                        
078000                    IF ORDERDEL-FINNS                                     
078100                       IF (MID-MIXAT = JA OR YES)                         
078200                           OR                                             
078300                          (MID-MIXAT = NEJ                                
078400                           AND                                            
078500                           ORQA-ODEL-IDPRCVAR = WS-SPAR-IDPRCVAR)         
078600                          ADD 1                   TO WS-KVORDER           
078700                          ADD ORQA-ODEL-KVRADER   TO WS-KVRADER           
078800                          ADD ORQA-ODEL-VKORDNTO  TO WS-VKORDNTO          
078900                          ADD ORQA-ODEL-VLORDNTO  TO WS-VLORDNTO          
079000                          PERFORM DD-KOLLA-PLOCKGRANS                     
079100                          PERFORM DG-UPPLAGG-PLOCKSATSREG                 
079200                          PERFORM DH-UPPDAT-ORDERDEL                      
079300                       ELSE                                               
079400                          MOVE JA TO PRC-BRYT-SW                          
079500                       END-IF                                             
079600                    END-IF                                                
079700                 END-PERFORM                                              
079800              END-IF                                                      
079900              PERFORM DI-VISA-BILD                                        
080000           ELSE                                                           
080100              MOVE '023' TO MED-IDMFSFEL                                  
080200              PERFORM S02-FEL-MEDDELANDE                                  
080300              PERFORM MFS-RENSA-FAELT-UT                                  
080400              PERFORM MFS-ROR-EJ-FAELT-IN                                 
080500           END-IF                                                         
080600        ELSE                                                              
080700           MOVE '036' TO MED-IDMFSINF                                     
080800           PERFORM S03-INFO-MEDDELANDE                                    
080900           PERFORM MFS-RENSA-FAELT-UT                                     
081000           PERFORM MFS-ROR-EJ-FAELT-IN                                    
081100        END-IF                                                            
081200     END-IF                                                               
081300     .                                                                    
081400     EJECT                                                                
081500 DA-KOLLA-INPUT-PARAMETRAR SECTION.                                       
081600                                                                          
081620     PERFORM DAA-KOLLA-PRINTER                                            
081630                                                                          
081700     IF MID-FLORDKNY = JA OR NEJ                                          
081800        MOVE MID-FLORDKNY TO MOD-FLORDKNY                                 
081900     ELSE                                                                 
082000        MOVE MFS-ALFA-FAELT-FEL TO MOD-FLORDKNY-ATTR                      
082100        MOVE NEJ TO INDATA-SW                                             
082200     END-IF                                                               
082300                                                                          
082400     IF MID-MIXAT = JA OR YES OR NEJ                                      
082500        MOVE MID-MIXAT TO MOD-MIXAT                                       
082600     ELSE                                                                 
082700        MOVE MFS-ALFA-FAELT-FEL TO MOD-MIXAT-ATTR                         
082800        MOVE NEJ TO INDATA-SW                                             
082900     END-IF                                                               
083000                                                                          
083100     IF MID-UTSKR-EJ-KOMPL-PS = JA OR YES OR NEJ                          
083200        MOVE MID-UTSKR-EJ-KOMPL-PS TO MOD-UTSKR-EJ-KOMPL-PS               
083300     ELSE                                                                 
083400        MOVE MFS-ALFA-FAELT-FEL TO MOD-UTSKR-EJ-KOMPL-PS-ATTR             
083500        MOVE NEJ TO INDATA-SW                                             
083600     END-IF                                                               
083700                                                                          
083800     IF INDATA-FEL                                                        
083900        MOVE '001' TO MED-IDMFSFEL                                        
084000        PERFORM S02-FEL-MEDDELANDE                                        
084100        PERFORM MFS-ROR-EJ-FAELT-UT                                       
084200        PERFORM MFS-ROR-EJ-FAELT-IN                                       
084300     END-IF                                                               
084400     .                                                                    
084410 DAA-KOLLA-PRINTER SECTION.                                               
084420                                                                          
084421*    ENDAST CDC FÅR ANGE PRINTER                                          
084422     IF INDATA-OK AND                                                     
084423        NOT CDC-SE AND                                                    
084424        (MID-KDPRT-PU NOT = ALL '+' OR                                    
084425        MID-KDPRT-PLE NOT = ALL '+')                                      
084426        MOVE MFS-ALFA-FAELT-FEL TO MOD-KDPRT-PU-ATTR                      
084427        MOVE MID-KDPRT-PU      TO MOD-KDPRT-PU                            
084428        MOVE '808' TO MED-IDMFSFEL                                        
084429        MOVE NEJ               TO INDATA-SW                               
084430     END-IF                                                               
084432                                                                          
084433*    ANGES PRINTER MÅSTE BÅDA VAR IFYLLDA                                 
084434     IF INDATA-OK AND                                                     
084435        ((MID-KDPRT-PU NOT = ALL '+' AND                                  
084436        MID-KDPRT-PLE = ALL '+')                                          
084437        OR                                                                
084438        (MID-KDPRT-PU = ALL '+' AND                                       
084439        MID-KDPRT-PLE NOT = ALL '+'))                                     
084440        MOVE MFS-ALFA-FAELT-FEL TO MOD-KDPRT-PU-ATTR                      
084441        MOVE MID-KDPRT-PU      TO MOD-KDPRT-PU                            
084442        MOVE '809' TO MED-IDMFSFEL                                        
084443        MOVE NEJ               TO INDATA-SW                               
084444     END-IF                                                               
084445                                                                          
084446     IF INDATA-OK AND                                                     
084447        MID-KDPRT-PU NOT = ALL '+'                                        
084448        MOVE '4'            TO WS-SYSTDEL                                 
084450        MOVE 'PU'           TO WS-LISTTYP                                 
084460        MOVE MID-KDPRT-PU TO WS-KDPRT                                     
084470                                                                          
084480        MOVE 1              TO PRT-KDCALL                                 
084490        MOVE WS-IDPRTLST    TO PRT-IDPRTLST                               
084491        CALL W006PRT    USING PRT-W006PRT                                 
084492                                                                          
084493        IF PRT-IDLTERM = 'SAKNAS  '                                       
084494           MOVE MFS-ALFA-FAELT-FEL TO MOD-KDPRT-PU-ATTR                   
084495           MOVE MID-KDPRT-PU           TO MOD-KDPRT-PU                    
084496           MOVE '772' TO MED-IDMFSFEL                                     
084497           MOVE NEJ     TO INDATA-SW                                      
084498        ELSE                                                              
084499           MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDPRT-PU-ATTR                 
084500           MOVE MID-KDPRT-PU           TO MOD-KDPRT-PU                    
084501                                          4002-KDPRT-PU                   
084502        END-IF                                                            
084503     END-IF                                                               
084504     IF MID-KDPRT-PLE NOT = ALL '+'                                       
084505        MOVE '4'             TO WS-SYSTDEL                                
084506        MOVE 'PE'            TO WS-LISTTYP                                
084507        MOVE MID-KDPRT-PLE TO WS-KDPRT                                    
084508                                                                          
084509        MOVE 1               TO PRT-KDCALL                                
084510        MOVE WS-IDPRTLST     TO PRT-IDPRTLST                              
084511        CALL W006PRT    USING PRT-W006PRT                                 
084512                                                                          
084513        IF PRT-IDLTERM = 'SAKNAS  '                                       
084514           MOVE MFS-ALFA-FAELT-FEL TO MOD-KDPRT-PLE-ATTR                  
084515           MOVE MID-KDPRT-PLE          TO MOD-KDPRT-PLE                   
084516           MOVE '772' TO MED-IDMFSFEL                                     
084517           MOVE NEJ     TO INDATA-SW                                      
084518        ELSE                                                              
084519           MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDPRT-PLE-ATTR                
084520           MOVE MID-KDPRT-PLE          TO MOD-KDPRT-PLE                   
084521                                          4002-KDPRT-PLE                  
084522        END-IF                                                            
084523     END-IF                                                               
084524     .                                                                    
084530     EJECT                                                                
084600 DB-LAES-FORSTA-ORDERDEL SECTION.                                         
084700                                                                          
084800     MOVE NEJ TO LAES-SW                                                  
084900     MOVE WS-IDDC  TO W-Q3BSEQ-MIN-IDDC                                   
085000                      W-Q3BSEQ-MAX-IDDC                                   
085100                                                                          
085200     PERFORM UNTIL LAES-OK                                                
085300        IF FORSTA-GANG                                                    
085400           MOVE NEJ TO FORSTA-SW                                          
085500           PERFORM IMS-GHU-ORQB-WLORQA01                                  
085600        ELSE                                                              
085700           PERFORM IMS-GHN-ORQB-WLORQA01                                  
085800        END-IF                                                            
085900        IF SEGMENT-SAKNAS OR END-OF-DATA                                  
086000           MOVE JA TO LAES-SW                                             
086100                      ORDERDEL-SW                                         
086200        END-IF                                                            
086300        IF SEGMENT-FINNS                                                  
086400           IF ORQA-ODEL-IDUSER = SPACE     OR                             
086500              ORQA-ODEL-IDUSER = WS-IDUSER                                
086600              MOVE ORQA-ODEL-IDORDER  TO W-IDORDER                        
086700              MOVE ORQA-ODEL-IDDC     TO W-IDDC                           
086800              PERFORM IMS-GHU-ORQI-WLORQI01-WLORQI12                      
086900              IF SEGMENT-FINNS                                            
087000                 IF ORQI-OHUV-FLKLAR = JA                                 
087100                    MOVE JA TO LAES-SW                                    
087200                 END-IF                                                   
087300              END-IF                                                      
087400           END-IF                                                         
087500        END-IF                                                            
087600     END-PERFORM                                                          
087700                                                                          
087800     IF ORDERDEL-FINNS                                                    
087900        IF ENGLISH-TEXT                                                   
088000           PERFORM S01-KONTROLL-RFS-DAG                                   
088100        END-IF                                                            
088200     END-IF                                                               
088300                                                                          
088400     IF ORDERDEL-FINNS                                                    
088500        ADD 1                   TO WS-KVORDER                             
088600        ADD ORQA-ODEL-KVRADER   TO WS-KVRADER                             
088700        ADD ORQA-ODEL-VKORDNTO  TO WS-VKORDNTO                            
088800        ADD ORQA-ODEL-VLORDNTO  TO WS-VLORDNTO                            
088900     END-IF                                                               
089000     .                                                                    
089100     EJECT                                                                
089200 DC-LAES-PRC-KANAL SECTION.                                               
089300                                                                          
089400     MOVE WS-IDDC         TO W-4447-IDDC                                  
089500     MOVE ORQA-ODEL-IDPRC TO W-4448-IDPRC                                 
089600                                                                          
089700     PERFORM IMS-GU-XXKH-WLXXKH11                                         
089800                                                                          
089900     IF SEGMENT-FINNS                                                     
090000        MOVE XXKH-4448-KVORDER  TO WS-PRC-KVORDER                         
090100        MOVE XXKH-4448-KVRADER  TO WS-PRC-KVRADER                         
090200        MOVE XXKH-4448-VKORDNTO TO WS-PRC-VKORDNTO                        
090300        MOVE XXKH-4448-VLORDNTO TO WS-PRC-VLORDNTO                        
090400        MOVE XXKH-4448-KVPLSRAD TO WS-PRC-KVPLSRAD                        
090500        MOVE XXKH-4448-VKPLSNTO TO WS-PRC-VKPLSNTO                        
090600        MOVE XXKH-4448-VLPLSNTO TO WS-PRC-VLPLSNTO                        
090700        MOVE XXKH-4448-RESPLIT  TO WS-PRC-RESPLIT                         
090800        MOVE XXKH-4448-KDPRCGRP TO W-4462-KDPRCGRP-X                      
090900     END-IF                                                               
091000     .                                                                    
091100     EJECT                                                                
091200 DD-KOLLA-PLOCKGRANS SECTION.                                             
091300                                                                          
091400     IF WS-KVORDER    = WS-PRC-KVORDER     OR                             
091500        WS-KVRADER   >= WS-PRC-KVRADER     OR                             
091600        WS-VLORDNTO  >= WS-PRC-VLORDNTO    OR                             
091700        WS-VKORDNTO  >= WS-PRC-VKORDNTO                                   
091800        MOVE JA TO PLOCKGRANS-SW                                          
091900     END-IF                                                               
092000                                                                          
092100     IF PLOCKGRANS                                                        
092200        IF WS-PRC-KVORDER = 1                                             
092300           IF WS-KVRADER    > WS-PRC-KVRADER     OR                       
092400              WS-VLORDNTO   > WS-PRC-VLORDNTO    OR                       
092500              WS-VKORDNTO   > WS-PRC-VKORDNTO                             
092600              MOVE JA TO SPLIT-SW                                         
092700           END-IF                                                         
092800        END-IF                                                            
092900     END-IF                                                               
093000                                                                          
093100     IF SPLIT                                                             
093200        PERFORM DDA-BERAKNA-ANTAL-SPLITSATSER                             
093300     END-IF                                                               
093400     .                                                                    
093500     EJECT                                                                
093600 DDA-BERAKNA-ANTAL-SPLITSATSER SECTION.                                   
093700                                                                          
093800     MOVE 0 TO WS-PROC-RAD                                                
093900               WS-PROC-VIKT                                               
094000               WS-PROC-VOLYM                                              
094100                                                                          
094200     IF WS-KVRADER > WS-PRC-KVRADER                                       
094300        COMPUTE WS-PROC-RAD =                                             
094400                WS-KVRADER * 100 / WS-PRC-KVRADER                         
094500                ON SIZE ERROR MOVE 0 TO WS-PROC-RAD                       
094600        END-COMPUTE                                                       
094700     END-IF                                                               
094800                                                                          
094900     IF WS-VKORDNTO > WS-PRC-VKORDNTO                                     
095000        COMPUTE WS-PROC-VIKT =                                            
095100                WS-VKORDNTO * 100 / WS-PRC-VKORDNTO                       
095200                ON SIZE ERROR MOVE 0 TO WS-PROC-VIKT                      
095300        END-COMPUTE                                                       
095400     END-IF                                                               
095500                                                                          
095600     IF WS-VLORDNTO > WS-PRC-VLORDNTO                                     
095700        COMPUTE WS-PROC-VOLYM =                                           
095800                WS-VLORDNTO * 100 / WS-PRC-VLORDNTO                       
095900                ON SIZE ERROR MOVE 0 TO WS-PROC-VOLYM                     
096000        END-COMPUTE                                                       
096100     END-IF                                                               
096200                                                                          
096300     IF WS-PROC-RAD NOT < WS-PROC-VIKT                                    
096400        AND                                                               
096500        WS-PROC-RAD NOT < WS-PROC-VOLYM                                   
096600        MOVE WS-KVRADER     TO WS-SPLITGRANS                              
096700        MOVE WS-PRC-KVRADER TO WS-PRC-SPLITGRANS                          
096800        MOVE 'RA'           TO WS-KDSORT                                  
096900     ELSE                                                                 
097000        IF WS-PROC-VIKT NOT < WS-PROC-RAD                                 
097100           AND                                                            
097200           WS-PROC-VIKT NOT < WS-PROC-VOLYM                               
097300           MOVE WS-VKORDNTO     TO WS-SPLITGRANS                          
097400           MOVE WS-PRC-VKORDNTO TO WS-PRC-SPLITGRANS                      
097500           MOVE 'KG'            TO WS-KDSORT                              
097600        ELSE                                                              
097700           IF WS-PROC-VOLYM NOT < WS-PROC-RAD                             
097800              AND                                                         
097900              WS-PROC-VOLYM NOT < WS-PROC-VIKT                            
098000              MOVE WS-VLORDNTO     TO WS-SPLITGRANS                       
098100              MOVE WS-PRC-VLORDNTO TO WS-PRC-SPLITGRANS                   
098200              MOVE 'M3'            TO WS-KDSORT                           
098300           END-IF                                                         
098400        END-IF                                                            
098500     END-IF                                                               
098600                                                                          
098700     COMPUTE WS-ANTSPLIT = WS-SPLITGRANS / WS-PRC-SPLITGRANS              
098800             ON SIZE ERROR MOVE ZERO TO WS-ANTSPLIT                       
098900     END-COMPUTE                                                          
099000                                                                          
099100     IF WS-ANTSPLIT (9:2) > ZERO                                          
099200        MOVE ZERO              TO WS-ANTSPLIT (9:2)                       
099300        COMPUTE WS-SPLITREST = WS-SPLITGRANS -                            
099400                              (WS-ANTSPLIT * WS-PRC-SPLITGRANS)           
099500        END-COMPUTE                                                       
099600        COMPUTE WS-RESPLIT =  WS-SPLITREST / WS-PRC-SPLITGRANS            
099700                ON SIZE ERROR MOVE ZERO TO WS-RESPLIT                     
099800        END-COMPUTE                                                       
099900        IF WS-RESPLIT >= WS-PRC-RESPLIT                                   
100000           ADD 1 TO WS-ANTSPLIT                                           
100100        END-IF                                                            
100200     END-IF                                                               
100300                                                                          
100400     IF WS-ANTSPLIT > 1                                                   
100500        MOVE WS-PRC-SPLITGRANS TO 4002-KVORDSPL                           
100600        MOVE WS-KDSORT         TO 4002-KDSORT                             
100700        MOVE JA                TO 4002-FLORDSPL                           
100800     ELSE                                                                 
100900        MOVE NEJ               TO SPLIT-SW                                
101000     END-IF                                                               
101100     .                                                                    
101200     EJECT                                                                
101300 DE-REDIGERA-INFOTEXTER SECTION.                                          
101400                                                                          
101500     MOVE ORQA-ODEL-KDFDKRAV TO W-4535-KDFDKRAV                           
101600     IF SPRAK-IX = 1                                                      
101700        MOVE 'S  '           TO W-4536-IDSKYLT                            
101800        MOVE 'SAKNAS'        TO MOD-FRAKTDATA                             
101900                                MOD-BELAGINS-GRP                          
102000     ELSE                                                                 
102100        MOVE 'GB '           TO W-4536-IDSKYLT                            
102200        MOVE 'MISSING'       TO MOD-FRAKTDATA                             
102300                                MOD-BELAGINS-GRP                          
102400     END-IF                                                               
102500     PERFORM IMS-GU-XXKU-WLXXKU11                                         
102600     IF SEGMENT-FINNS                                                     
102700        MOVE XXKU-4536-BEFDKRAV       TO MOD-FRAKTDATA                    
102800     END-IF                                                               
102900     IF ORQI-OHUV-BELAGINS-GRP NOT = SPACE                                
103000        MOVE ORQI-OHUV-BELAGINS-GRP   TO MOD-BELAGINS-GRP                 
103100     END-IF                                                               
103200     .                                                                    
103300     EJECT                                                                
103400 DF-LAES-NASTA-ORDERDEL SECTION.                                          
103500                                                                          
103600     MOVE NEJ TO LAES-SW                                                  
103700                                                                          
103800     PERFORM UNTIL LAES-OK                                                
103900        PERFORM IMS-GHN-ORQB-WLORQA01                                     
104000        IF SEGMENT-SAKNAS OR END-OF-DATA                                  
104100           MOVE JA TO LAES-SW                                             
104200                      ORDERDEL-SW                                         
104300        END-IF                                                            
104400        IF SEGMENT-FINNS                                                  
104500           IF ORQA-ODEL-IDUSER = SPACE     OR                             
104600              ORQA-ODEL-IDUSER = WS-IDUSER                                
104700              IF (ORQA-ODEL-KVRADER  <= WS-PRC-KVPLSRAD                   
104800                  AND                                                     
104900                  ORQA-ODEL-VKORDNTO <= WS-PRC-VKPLSNTO                   
105000                  AND                                                     
105100                  ORQA-ODEL-VLORDNTO <= WS-PRC-VLPLSNTO)                  
105200                  OR                                                      
105300                 (MID-MIXAT = NEJ                                         
105400                  AND                                                     
105500                  ORQA-ODEL-IDPRCVAR NOT = WS-SPAR-IDPRCVAR)              
105600                 MOVE ORQA-ODEL-IDORDER  TO W-IDORDER                     
105700                 MOVE ORQA-ODEL-IDDC     TO W-IDDC                        
105800                 PERFORM IMS-GHU-ORQI-WLORQI01-WLORQI12                   
105900                 IF SEGMENT-FINNS                                         
106000                    IF ORQI-OHUV-FLKLAR = JA                              
106100                       MOVE JA TO LAES-SW                                 
106200                    END-IF                                                
106300                 END-IF                                                   
106400              END-IF                                                      
106500           END-IF                                                         
106600        END-IF                                                            
106700     END-PERFORM                                                          
106800                                                                          
106900     IF ORDERDEL-FINNS                                                    
107000        IF ENGLISH-TEXT                                                   
107100           IF C2-EJ-OK                                                    
107200              PERFORM S01-KONTROLL-RFS-DAG                                
107300           END-IF                                                         
107400        END-IF                                                            
107500     END-IF                                                               
107600     .                                                                    
107700     EJECT                                                                
107800 DG-UPPLAGG-PLOCKSATSREG SECTION.                                         
107900                                                                          
108000     ADD 1                   TO 4002-IXHEL                                
108100     MOVE ORQA-ODEL-IDORDER  TO 4002-IDORDER  (4002-IXHEL)                
108200     MOVE ORQA-ODEL-IDDC     TO 4002-IDDC     (4002-IXHEL)                
108300     MOVE ORQA-ODEL-IDPRODNR TO 4002-IDPRODNR (4002-IXHEL)                
108400     MOVE ORQA-ODEL-IDPLKLST TO 4002-IDPLKLST (4002-IXHEL)                
108500                                                                          
108600     IF SPLIT                                                             
108700        PERFORM DGA-HAMTA-NASTA-PLOCKLISTNR                               
108800     END-IF                                                               
108900     .                                                                    
109000     EJECT                                                                
109100 DGA-HAMTA-NASTA-PLOCKLISTNR SECTION.                                     
109200                                                                          
109300     MOVE 4002-ORDDEL (4002-IXHEL) TO 4002-ORDDEL (99)                    
109400     COMPUTE ORQI-ARB-IDPLKLST-SISTA                                      
109500                      = ORQI-ARB-IDPLKLST-SISTA + 1                       
109600     MOVE ORQI-ARB-IDPLKLST-SISTA TO 4002-IDPLKLST (99)                   
109700                                                                          
109800     PERFORM IMS-REPL-ORQI-WLORQI12                                       
109900     .                                                                    
110000     EJECT                                                                
110100 DH-UPPDAT-ORDERDEL SECTION.                                              
110200                                                                          
110300     MOVE WS-IDUSER       TO ORQA-ODEL-IDUSER                             
110400     MOVE WS-IDBORD       TO ORQA-ODEL-IDBORD                             
110500     MOVE WS-DATUM-LOK    TO ORQA-ODEL-DAUTSKR                            
110600     IF WS-DATUM-LOK NOT = ZERO                                           
110700       IF WS-DATUM-LOK < 500000                                           
110800         MOVE 20          TO ORQA-ODEL-DAUTSKR (1:2)                      
110900       ELSE                                                               
111000         IF WS-DATUM-LOK < 999999                                         
111100           MOVE 19        TO ORQA-ODEL-DAUTSKR (1:2)                      
111200         ELSE                                                             
111300           MOVE 99999999  TO ORQA-ODEL-DAUTSKR                            
111400         END-IF                                                           
111500       END-IF                                                             
111600     END-IF                                                               
111700     MOVE WS-TIHHMMSS-LOK TO ORQA-ODEL-TIUTSTID                           
111800     MOVE 'U'             TO ORQA-ODEL-KDODELSTA                          
111900     PERFORM IMS-REPL-ORQB-WLORQA01                                       
112000     .                                                                    
112100     EJECT                                                                
112200 DI-VISA-BILD SECTION.                                                    
112300                                                                          
112400     IF EJ-SPLIT                                                          
112500        MOVE WS-KVORDER      TO MOD-KVORDER                               
112600        MOVE WS-KVRADER      TO MOD-KVRADER                               
112700        IF US-MATT                                                        
112800           COMPUTE MOD-VKORDNTO = WS-VKORDNTO                             
112900                                * CONV-KG-TO-LB                           
113000           COMPUTE MOD-VLORDNTO = WS-VLORDNTO                             
113100                                * CONV-M3-TO-FT3                          
113200        ELSE                                                              
113300          MOVE WS-VKORDNTO   TO MOD-VKORDNTO                              
113400          MOVE WS-VLORDNTO   TO MOD-VLORDNTO                              
113500        END-IF                                                            
113600     ELSE                                                                 
113700        MOVE WS-KVORDER      TO MOD-KVORDER                               
113800        MOVE ZERO            TO MOD-KVRADER                               
113900                                MOD-VKORDNTO                              
114000                                MOD-VLORDNTO                              
114100        IF 4002-KDSORT = 'RA'                                             
114200           MOVE 4002-KVORDSPL TO MOD-KVRADER                              
114300        ELSE                                                              
114400           IF 4002-KDSORT = 'KG'                                          
114500              IF US-MATT                                                  
114600                 COMPUTE MOD-VKORDNTO = 4002-KVORDSPL                     
114700                                      * CONV-KG-TO-LB                     
114800              ELSE                                                        
114900                MOVE 4002-KVORDSPL TO MOD-VKORDNTO                        
115000              END-IF                                                      
115100           ELSE                                                           
115200              IF 4002-KDSORT = 'M3'                                       
115300                 IF US-MATT                                               
115400                    COMPUTE MOD-VLORDNTO = 4002-KVORDSPL                  
115500                                         * CONV-M3-TO-FT3                 
115600                 ELSE                                                     
115700                   MOVE 4002-KVORDSPL TO MOD-VLORDNTO                     
115800                 END-IF                                                   
115900              END-IF                                                      
116000           END-IF                                                         
116100        END-IF                                                            
116200     END-IF                                                               
116300                                                                          
116400     IF WS-KVORDER > 1                                                    
116500        MOVE 'DIVERSE' TO MOD-FRAKTDATA                                   
116600                          MOD-BELAGINS-GRP                                
116700     END-IF                                                               
116800                                                                          
116900     IF (MID-UTSKR-EJ-KOMPL-PS = NEJ AND ORDERDEL-SLUT)                   
117000         OR                                                               
117100        (MID-UTSKR-EJ-KOMPL-PS = NEJ AND PRC-BRYTNING)                    
117200        MOVE MFS-RENSA-FAELT TO MOD-IDPRC                                 
117300        IF ORDERDEL-FINNS                                                 
117400           MOVE WLORQA01 TO W-ORDERDEL                                    
117500           PERFORM DIA-ATERSTALL-ORDERDELAR                               
117600           MOVE W-ORDERDEL TO WLORQA01                                    
117700           MOVE JA TO OMSTART-SW                                          
117800        ELSE                                                              
117900           PERFORM DIA-ATERSTALL-ORDERDELAR                               
118000           MOVE '039'        TO MED-IDMFSINF                              
118100           PERFORM S03-INFO-MEDDELANDE                                    
118200        END-IF                                                            
118300     ELSE                                                                 
118400        PERFORM DIB-SKAPA-PLOCKSATS                                       
118500        MOVE W-4448-IDPRC    TO MOD-IDPRC                                 
118600        MOVE MFS-RENSA-FAELT TO MOD-IDDC-UT                               
118700                                MOD-IDPRC-UT                              
118800                                MOD-IDUSER-UT                             
118900                                MOD-IDBORD-UT                             
119000        PERFORM IMS-ISRT-MSG-ALT1                                         
119100        MOVE '037'               TO MED-IDMFSINF                          
119200        PERFORM S03-INFO-MEDDELANDE                                       
119300        IF SPLIT                                                          
119400           IF MED-IDSKYLT = 'S  '                                         
119500              STRING MED-IDMFSINF ' SPLITTAD ' MED-TEMFSINF               
119600                     DELIMITED BY '   ' INTO MOD-TEMFSINF                 
119700           ELSE                                                           
119800              STRING MED-IDMFSINF ' SPLITTED ' MED-TEMFSINF               
119900                     DELIMITED BY '   ' INTO MOD-TEMFSINF                 
120000           END-IF                                                         
120100        END-IF                                                            
120200     END-IF                                                               
120300     .                                                                    
120400     EJECT                                                                
120500 DIA-ATERSTALL-ORDERDELAR SECTION.                                        
120600                                                                          
120700     MOVE 1 TO IX1                                                        
120800                                                                          
120900     PERFORM UNTIL IX1 > 99                                               
121000       PERFORM DIAA-LAES-ORDERDEL                                         
121100       IF SEGMENT-FINNS                                                   
121200          PERFORM IMS-REPL-ORQ-WLORQA01                                   
121300          ADD 1 TO IX1                                                    
121400       END-IF                                                             
121500     END-PERFORM                                                          
121600     .                                                                    
121700     EJECT                                                                
121800 DIAA-LAES-ORDERDEL SECTION.                                              
121900                                                                          
122000     IF 4002-ORDDEL (IX1) NOT = LOW-VALUE                                 
122100        MOVE 4002-IDORDER  (IX1) TO W-Q301KY-IDORDER                      
122200        MOVE 4002-IDDC     (IX1) TO W-Q301KY-IDDC                         
122300        MOVE 4002-IDPRODNR (IX1) TO W-Q301KY-IDPRODNR                     
122400        MOVE 4002-IDPLKLST (IX1) TO W-Q301KY-IDPLKLST                     
122500        PERFORM IMS-GHU-ORQ-WLORQA01                                      
122600        IF SEGMENT-FINNS                                                  
122700           MOVE SPACE   TO ORQA-ODEL-IDUSER                               
122800                           ORQA-ODEL-IDBORD                               
122900           MOVE ZERO    TO ORQA-ODEL-DAUTSKR                              
123000                           ORQA-ODEL-TIUTSTID                             
123100           MOVE 'R'     TO ORQA-ODEL-KDODELSTA                            
123200        END-IF                                                            
123300     ELSE                                                                 
123400        MOVE 'GE'    TO STATUS-WS                                         
123500        MOVE 100     TO IX1                                               
123600     END-IF                                                               
123700     .                                                                    
123800     EJECT                                                                
123900 DIB-SKAPA-PLOCKSATS SECTION.                                             
124000                                                                          
124100     IF 4002-ORDDEL (1) NOT = LOW-VALUE                                   
124200        PERFORM DIBA-HAMTA-PLOCKSATSNR                                    
124300        MOVE 4002-IDPRODNR (1) TO W-4001-IDPRODNR                         
124400                                  PTOP1-MID-IDPRODNR                      
124500        MOVE 4002-IDPLKLST (1) TO W-4001-IDPLKLST                         
124600                                  PTOP1-MID-IDPLKLST                      
124700        MOVE W-4001-IDHTYP-X   TO WL400101                                
124800        PERFORM IMS-ISRT-4001-WL400101                                    
124900        MOVE ZERO              TO 4002-IXHEL                              
125000        PERFORM IMS-ISRT-4001-WL400111                                    
125100     END-IF                                                               
125200     .                                                                    
125300     EJECT                                                                
125400 DIBA-HAMTA-PLOCKSATSNR SECTION.                                          
125500                                                                          
125600     MOVE WS-IDDC  TO W-4461-IDDC                                         
125700     PERFORM IMS-GHU-XXKO-WLXXKO11                                        
125800     IF SEGMENT-FINNS                                                     
125900        IF XXKO-4462-IDLOPNR-PL = 999                                     
126000           MOVE WS-DATUM-LOK      TO XXKO-4462-TIDATUM                    
126100           MOVE 1                 TO XXKO-4462-IDLOPNR-PL                 
126200        ELSE                                                              
126300           ADD  1                 TO XXKO-4462-IDLOPNR-PL                 
126400        END-IF                                                            
126500        MOVE XXKO-4462-IDLOPNR-PL TO 4002-IDLOPNR-PL                      
126600        PERFORM IMS-REPL-XXKO-WLXXKO11                                    
126700     ELSE                                                                 
126800        MOVE 1                    TO 4002-IDLOPNR-PL                      
126900     END-IF                                                               
127000     .                                                                    
127100     EJECT                                                                
127200 E-OMSTART-W40351 SECTION.                                                
127300                                                                          
127400     MOVE ORQA-ODEL-IDDC        TO MID-IDDC-BSEQ                          
127500     MOVE ORQA-ODEL-IDPRCBAS    TO MID-IDPRCBAS-BSEQ                      
127600     MOVE ORQA-ODEL-DAUTSKR (3:6)  TO MID-TIUTSKR-BSEQ                    
127700     MOVE ORQA-ODEL-TIUTSTID    TO MID-TIUTSTID-BSEQ                      
127800     MOVE ORQA-ODEL-DARFS (3:10) TO MID-TIRFS-BSEQ                        
127900     MOVE ORQA-ODEL-DALSTORD (3:10) TO MID-TILST-O-BSEQ                   
128000     MOVE ORQA-ODEL-IDPRCVAR    TO MID-IDPRCVAR-BSEQ                      
128100     MOVE MID-W4I35101          TO PTOP2-MID-W4I35101                     
128200     PERFORM IMS-ISRT-MSG-ALT2                                            
128300     .                                                                    
128400     EJECT                                                                
128500 S01-KONTROLL-RFS-DAG SECTION.                                            
128600                                                                          
128700     MOVE ORQA-ODEL-DARFS TO WS-DARFS                                     
128800                                                                          
128900     MOVE WS-RFS-DATUM   TO TMP1-YYMMDD                                   
129000     MOVE WS-DATUM-LOK   TO TMP2-YYMMDD                                   
129100     PERFORM WY2000P1                                                     
129200     IF TMP1-YYMMDD > TMP2-YYMMDD                                         
129300        IF 4002-IXHEL = 0                                                 
129400           MOVE JA TO C2-RFS-SW                                           
129500        ELSE                                                              
129600           MOVE JA TO ORDERDEL-SW                                         
129700        END-IF                                                            
129800     END-IF                                                               
129900     .                                                                    
130000     EJECT                                                                
130100 S02-FEL-MEDDELANDE  SECTION.                                             
130200                                                                          
130300     CALL WMEDKONV USING MED-WMEDAREA                                     
130400     MOVE MED-MFSFEL TO MOD-TEMFSFEL                                      
130500     .                                                                    
130600                                                                          
130700                                                                          
130800 S03-INFO-MEDDELANDE SECTION.                                             
130900                                                                          
131000     CALL WMEDKONV USING MED-WMEDAREA                                     
131100     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
131200     .                                                                    
131300     EJECT                                                                
131400 MFS-RENSA-FAELT-UT SECTION.                                              
131500                                                                          
131600*    --- ALLA UTDATA-FÄLT                                                 
131700     MOVE MFS-RENSA-FAELT TO MOD-KVORDER                                  
131800                             MOD-KVRADER                                  
131900                             MOD-IDPRC                                    
132000                             MOD-VKORDNTO                                 
132100                             MOD-VLORDNTO                                 
132200                             MOD-FRAKTDATA                                
132300                             MOD-BELAGINS-GRP                             
132400     .                                                                    
132500     EJECT                                                                
132600 MFS-ROR-EJ-FAELT-UT  SECTION.                                            
132700                                                                          
132800*    --- ALLA UTDATA-FÄLT                                                 
132900     MOVE MFS-ROER-EJ-FAELT TO MOD-KVORDER                                
133000                               MOD-KVRADER                                
133100                               MOD-IDPRC                                  
133200                               MOD-VKORDNTO                               
133300                               MOD-VLORDNTO                               
133400                               MOD-FRAKTDATA                              
133500                               MOD-BELAGINS-GRP                           
133600     .                                                                    
133700                                                                          
133800                                                                          
133900 MFS-ROR-EJ-FAELT-IN  SECTION.                                            
134000                                                                          
134100*    --- ALLA INDATA-FÄLT                                                 
134200     MOVE MFS-ROER-EJ-FAELT TO MOD-FLORDKNY                               
134300                               MOD-MIXAT                                  
134400                               MOD-UTSKR-EJ-KOMPL-PS                      
134410                               MOD-KDPRT-PU                               
134420                               MOD-KDPRT-PLE                              
134500     .                                                                    
134600     EJECT                                                                
134700* --- IMS SEKTIONER ---                                                   
134800     SKIP3                                                                
134900 IMS-GU-MSG SECTION.                                                      
135000                                                                          
135100     MOVE '  QC' TO GODK-STATUSKODER                                      
135200     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
135300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
135400     PERFORM IMS-STATUSKONTROLL                                           
135500     .                                                                    
135600     SKIP3                                                                
135700 IMS-ISRT-MSG SECTION.                                                    
135800                                                                          
135900     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
136000       MOVE '0' TO MFS-KDHUVOMR                                           
136100     END-IF                                                               
136200     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
136300     MOVE SPACE TO GODK-STATUSKODER                                       
136400     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
136500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
136600     PERFORM IMS-STATUSKONTROLL                                           
136700     .                                                                    
136800     SKIP3                                                                
136900 IMS-ISRT-MSG-ALT1 SECTION.                                               
137000                                                                          
137100     MOVE SPACE TO GODK-STATUSKODER                                       
137200     CALL CBLTDLI USING ISRT ALT1-PCB P-TO-P-SW1                          
137300     MOVE ALT1-STATUS-CODE TO STATUS-WS                                   
137400     PERFORM IMS-STATUSKONTROLL                                           
137500     .                                                                    
137600     EJECT                                                                
137700 IMS-ISRT-MSG-ALT2 SECTION.                                               
137800                                                                          
137900     MOVE SPACE TO GODK-STATUSKODER                                       
138000     CALL CBLTDLI USING ISRT ALT2-PCB P-TO-P-SW2                          
138100     MOVE ALT2-STATUS-CODE TO STATUS-WS                                   
138200     PERFORM IMS-STATUSKONTROLL                                           
138300     .                                                                    
138400     EJECT                                                                
138500 IMS-GHU-ORQI-WLORQI01-WLORQI12 SECTION.                                  
138600                                                                          
138700     STRING 'WLORQI01*D(IDORDER  =' W-IDORDER-X ')'                       
138800          DELIMITED BY SIZE INTO SSA1                                     
138900     STRING 'WLORQI12(IDDC     =' W-IDDC-X ')'                            
139000          DELIMITED BY SIZE INTO SSA2                                     
139100     MOVE '  GE' TO GODK-STATUSKODER                                      
139200     CALL CBLTDLI USING GHU ORQI-PCB DLI-IO-AREA2 SSA1 SSA2               
139300     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
139400     PERFORM IMS-STATUSKONTROLL                                           
139500     .                                                                    
139600                                                                          
139700                                                                          
139800 IMS-REPL-ORQI-WLORQI12 SECTION.                                          
139900                                                                          
140000     MOVE 'WLORQI01*N' TO SSA1                                            
140100     MOVE '    ' TO GODK-STATUSKODER                                      
140200     CALL CBLTDLI USING REPL ORQI-PCB DLI-IO-AREA2 SSA1                   
140300     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
140400     PERFORM IMS-STATUSKONTROLL                                           
140500     .                                                                    
140600     EJECT                                                                
140700 IMS-GHU-ORQ-WLORQA01 SECTION.                                            
140800                                                                          
140900     STRING 'WLORQA01(WDQ301KY =' W-WDQ301KY-X ')'                        
141000          DELIMITED BY SIZE INTO SSA1                                     
141100     MOVE '  GE' TO GODK-STATUSKODER                                      
141200     CALL CBLTDLI USING GHU ORQ-PCB DLI-IO-AREA1 SSA1                     
141300     MOVE ORQ-STATUS-CODE TO STATUS-WS                                    
141400     PERFORM IMS-STATUSKONTROLL                                           
141500     .                                                                    
141600                                                                          
141700                                                                          
141800 IMS-GHU-ORQB-WLORQA01 SECTION.                                           
141900                                                                          
142000     STRING 'WLORQA01(WDQ3BSEQ>=' W-WDQ3BSEQ-MIN-X                        
142100                    '&WDQ3BSEQ<=' W-WDQ3BSEQ-MAX-X                        
142200                    '&IDPRCVAR>=' W-Q3BSEQ-MIN-IDPRCVAR                   
142300                    '&IDPRCVAR<=' W-Q3BSEQ-MAX-IDPRCVAR ')'               
142400          DELIMITED BY SIZE INTO SSA1                                     
142500     MOVE '  GE' TO GODK-STATUSKODER                                      
142600     CALL CBLTDLI USING GHU ORQB-PCB DLI-IO-AREA1 SSA1                    
142700     MOVE ORQB-STATUS-CODE TO STATUS-WS                                   
142800     PERFORM IMS-STATUSKONTROLL                                           
142900     .                                                                    
143000                                                                          
143100                                                                          
143200 IMS-GHN-ORQB-WLORQA01 SECTION.                                           
143300                                                                          
143400     STRING 'WLORQA01(WDQ3BSEQ>=' W-WDQ3BSEQ-MIN-X                        
143500                    '&WDQ3BSEQ<=' W-WDQ3BSEQ-MAX-X                        
143600                    '&IDPRCVAR>=' W-Q3BSEQ-MIN-IDPRCVAR                   
143700                    '&IDPRCVAR<=' W-Q3BSEQ-MAX-IDPRCVAR ')'               
143800          DELIMITED BY SIZE INTO SSA1                                     
143900     MOVE '  GBGE' TO GODK-STATUSKODER                                    
144000     CALL CBLTDLI USING GHN ORQB-PCB DLI-IO-AREA1 SSA1                    
144100     MOVE ORQB-STATUS-CODE TO STATUS-WS                                   
144200     PERFORM IMS-STATUSKONTROLL                                           
144300     .                                                                    
144400                                                                          
144500                                                                          
144600 IMS-REPL-ORQ-WLORQA01 SECTION.                                           
144700                                                                          
144800     MOVE '  ' TO GODK-STATUSKODER                                        
144900     CALL CBLTDLI USING REPL ORQ-PCB DLI-IO-AREA1                         
145000     MOVE ORQ-STATUS-CODE TO STATUS-WS                                    
145100     PERFORM IMS-STATUSKONTROLL                                           
145200     .                                                                    
145300                                                                          
145400                                                                          
145500 IMS-REPL-ORQB-WLORQA01 SECTION.                                          
145600                                                                          
145700     MOVE '  ' TO GODK-STATUSKODER                                        
145800     CALL CBLTDLI USING REPL ORQB-PCB DLI-IO-AREA1                        
145900     MOVE ORQB-STATUS-CODE TO STATUS-WS                                   
146000     PERFORM IMS-STATUSKONTROLL                                           
146100     .                                                                    
146200     EJECT                                                                
146300 IMS-GU-XXKH-WLXXKH11 SECTION.                                            
146400                                                                          
146500     STRING 'WLXXKH01(WDGXKEY  =' W-4447-IDHTYP-X ')'                     
146600          DELIMITED BY SIZE INTO SSA1                                     
146700     STRING 'WLXXKH11(WDGXKEY  =' W-4448-IDPRC-X ')'                      
146800          DELIMITED BY SIZE INTO SSA2                                     
146900     MOVE '  GE' TO GODK-STATUSKODER                                      
147000     CALL CBLTDLI USING GU XXKH-PCB DLI-IO-AREA SSA1 SSA2                 
147100     MOVE XXKH-STATUS-CODE TO STATUS-WS                                   
147200     PERFORM IMS-STATUSKONTROLL                                           
147300     .                                                                    
147400     EJECT                                                                
147500 IMS-GU-XXKU-WLXXKU11 SECTION.                                            
147600                                                                          
147700     STRING 'WLXXKU01(WDGXKEY  =' W-4535-IDHTYP-X ')'                     
147800          DELIMITED BY SIZE INTO SSA1                                     
147900     STRING 'WLXXKU11(WDGXKEY  =' W-4536-IDSKYLT-X ')'                    
148000          DELIMITED BY SIZE INTO SSA2                                     
148100     MOVE '  GE' TO GODK-STATUSKODER                                      
148200     CALL CBLTDLI USING GU XXKU-PCB DLI-IO-AREA SSA1 SSA2                 
148300     MOVE XXKU-STATUS-CODE TO STATUS-WS                                   
148400     PERFORM IMS-STATUSKONTROLL                                           
148500     .                                                                    
148600     EJECT                                                                
148700 IMS-GHU-XXKO-WLXXKO11 SECTION.                                           
148800                                                                          
148900     STRING 'WLXXKO01(WDGXKEY  =' W-4461-IDHTYP-X ')'                     
149000          DELIMITED BY SIZE INTO SSA1                                     
149100     STRING 'WLXXKO11(WDGXKEY  =' W-4462-KDPRCGRP-X ')'                   
149200          DELIMITED BY SIZE INTO SSA2                                     
149300     MOVE '  GE' TO GODK-STATUSKODER                                      
149400     CALL CBLTDLI USING GHU XXKO-PCB DLI-IO-AREA SSA1 SSA2                
149500     MOVE XXKO-STATUS-CODE TO STATUS-WS                                   
149600     PERFORM IMS-STATUSKONTROLL                                           
149700     .                                                                    
149800                                                                          
149900                                                                          
150000 IMS-REPL-XXKO-WLXXKO11 SECTION.                                          
150100                                                                          
150200     MOVE '    ' TO GODK-STATUSKODER                                      
150300     CALL CBLTDLI USING REPL XXKO-PCB DLI-IO-AREA                         
150400     MOVE XXKO-STATUS-CODE TO STATUS-WS                                   
150500     PERFORM IMS-STATUSKONTROLL                                           
150600     .                                                                    
150700     EJECT                                                                
150800 IMS-ISRT-4001-WL400101 SECTION.                                          
150900                                                                          
151000     MOVE 'WL400101 ' TO SSA1                                             
151100     MOVE '    ' TO GODK-STATUSKODER                                      
151200     CALL CBLTDLI USING ISRT 4001-PCB DLI-IO-AREA SSA1                    
151300     MOVE 4001-STATUS-CODE TO STATUS-WS                                   
151400     PERFORM IMS-STATUSKONTROLL                                           
151500     .                                                                    
151600                                                                          
151700 IMS-ISRT-4001-WL400111 SECTION.                                          
151800                                                                          
151900     STRING 'WL400101(WDGXKEY  =' W-4001-IDHTYP-X ')'                     
152000          DELIMITED BY SIZE INTO SSA1                                     
152100     MOVE 'WL400111 ' TO SSA2                                             
152200     MOVE '    ' TO GODK-STATUSKODER                                      
152300     CALL CBLTDLI USING ISRT 4001-PCB DLI-IO-AREA3 SSA1 SSA2              
152400     MOVE 4001-STATUS-CODE TO STATUS-WS                                   
152500     PERFORM IMS-STATUSKONTROLL                                           
152600     .                                                                    
152700     EJECT                                                                
152800 IMS-STATUSKONTROLL SECTION.                                              
152900                                                                          
153000     SET STATUS-IX TO 1                                                   
153100     SEARCH GODK-STATUS                                                   
153200       AT END CALL FELLOG                                                 
153300       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
153400     END-SEARCH                                                           
153500     .                                                                    
153600     EJECT                                                                
153700*    -COPY WY2000P1                                                       
