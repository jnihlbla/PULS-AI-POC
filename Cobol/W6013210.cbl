000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W6013210.                                                
000400*AUTHOR.         LARS THELL > LASSI                                       
000500*DATE-WRITTEN.   92/06/01   > NOV 2011.                                   
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        PROGRAMMET VISAR KÖ TILL EN VISS PLACERING ELLER GODS            
001100*        SOM ÄR PÅ VÄG TILL EN PLACERING, I PRIORITETSORDNING.            
001200*        KOLLIN/PARTIER SOM SKA FÖRPACKAS ELLER FÖRBEHANDLAS VÄLJS        
001300*        PÅ BILDEN.                                                       
001400*                                                                         
001500*        PROGRAMMET          UPPDATERAR W6INLA (W6D1)                     
001600*        PROGRAMMET          LÄSER      W6PLAA (W6G1)                     
001700*                                  WDK6 WDK7 WDB6 WDD3                    
001800*    SUB PROGRAMMET W611PMRK UPPDATERAR W6INLA (W6D1)                     
001900*                            LÄSER      W6PLAA (W6G1)                     
002000*                                                                         
002100*    PROGRAMMET SKICKAR TRANS TILL 6197                                   
002200*                                                                         
002300*    INDATA.                                                              
002400*       REQU:         W60132I1                                            
002500*                                                                         
002600*    UTDATA.                                                              
002700*       RESP:         W60132O1                                            
002800                                                                          
002900     SKIP3                                                                
003000 ENVIRONMENT DIVISION.                                                    
003100     EJECT                                                                
003200 DATA DIVISION.                                                           
003300 WORKING-STORAGE SECTION.                                                 
003400 77  IDPGM                       PIC X(08)   VALUE 'W6013210'.            
003500 77  FELTEXT                     PIC X(80)   VALUE SPACE.                 
003600 77  JA                          PIC X       VALUE 'J'.                   
003700 77  YES                         PIC X       VALUE 'Y'.                   
003800 77  NEJ                         PIC X       VALUE 'N'.                   
003900 01  W-SPACE.                                                             
004000    03 FILLER                    PIC X(50)   VALUE SPACE.                 
004100                                                                          
004200*    --- INDEX FÖR BLÄDDRINGSRADER                                        
004300 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
004400 77  6191-IX                     PIC S9(9)  VALUE +0    COMP SYNC.        
004500 77  MAX-6191-IX                 PIC S9(9)  VALUE +24   COMP SYNC.        
004600 77  6197-IX                     PIC S9(9)  VALUE +0    COMP SYNC.        
004700 77  MAX-6197-IX                 PIC S9(9)  VALUE +15   COMP SYNC.        
004800 77  VAGN-IX                     PIC S9(9)  VALUE +0    COMP SYNC.        
004900 77  MAX-VAGN-IX                 PIC S9(9)  VALUE +51   COMP SYNC.        
005000 77  LNG-P-TO-P-PREFIX           PIC S9(4)  VALUE +17   COMP SYNC.        
005100                                                                          
005200 77  FL-KVROS                    PIC X      VALUE 'N'.                    
005300 77  W-NYCKEL-KOMB               PIC S9(1)  VALUE ZERO  COMP-3.           
005400 77  W-SPAR-IDLEVNR              PIC X(5)   VALUE SPACE.                  
005500 77  W-SPAR-IDFS                 PIC X(8)   VALUE SPACE.                  
005600 77  W-SPAR-TIAVIDAT             PIC 9(7)   VALUE ZERO.                   
005700 77  W-SPAR-IDRADNR-INL          PIC 9(5)   VALUE ZERO.                   
005800 77  W-SPAR-IDRADNR              PIC 9(5)   VALUE ZERO.                   
005900 77  W-SPAR-KDINLPRIO            PIC 9(3)   VALUE ZERO.                   
006000 77  W-BEFT-FOM                  PIC 9(3)   VALUE ZERO.                   
006100 77  W-BEFT-TOM                  PIC 9(3)   VALUE ZERO.                   
006200 77  W-SPAR-KDCMDVAL-LINE        PIC X(3)   VALUE SPACE.                  
006300 77  W-ADINLOMR-OLD              PIC X(4)   VALUE SPACE.                  
006400 77  W-ADINLOMR-NXT-OLD          PIC X(4)   VALUE SPACE.                  
006500 77  W-KDINLSTA-OLD              PIC X(3)   VALUE SPACE.                  
006600 77  W-KVINLART-OLD              PIC S9(7)  VALUE ZERO COMP-3.            
006700 77  W-KDINLOMR                  PIC X(3)   VALUE SPACE.                  
006800 77  W-KDINLOMR-RTA-PAR          PIC X(3)   VALUE SPACE.                  
006900 77  W-KDINLOMR-NXT              PIC X(3)   VALUE SPACE.                  
007000 77  W-KVINLCAR                  PIC 9(7)   VALUE ZERO.                   
007100 77  W-REQU-KVINLCAR             PIC 9(7)   VALUE ZERO.                   
007200 77  W-REQU-KVRADER-HIT          PIC 9(7)   VALUE ZERO.                   
007300 77  W-REQU-KVRADER-TOT          PIC 9(7)   VALUE ZERO.                   
007400 77  W-REQU-KVRADER-PRIO         PIC 9(7)   VALUE ZERO.                   
007500 77  W-KVRADER-PRIO              PIC 9(7)   VALUE ZERO.                   
007600 77  W-KVRADER-TOT               PIC 9(7)   VALUE ZERO.                   
007700 77  W-KVRADER-HIT               PIC 9(7)   VALUE ZERO.                   
007800                                                                          
007900*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
008000 77  WS-ADINLOMR                 PIC X(4)    VALUE SPACE.                 
008100 77  WS-ADINLOMR-NXT             PIC X(4)    VALUE SPACE.                 
008200 77  WS-KDINLQ                   PIC X(1)    VALUE SPACE.                 
008300 77  WS-BEFT-FOM                 PIC X(2)    VALUE SPACE.                 
008400 77  WS-BEFT-TOM                 PIC X(2)    VALUE SPACE.                 
008500 77  WS-FLINLFB                  PIC X(1)    VALUE SPACE.                 
008600                                                                          
008700*    --- HOPPNYCKLAR SOM EJ SYNS PÅ SKÄRMEN                               
008800 77  WS-IDLEVNR-KOLLI            PIC X(5)    VALUE SPACE.                 
008900 77  WS-IDOKOLLI                 PIC X(9)    VALUE SPACE.                 
009000 77  WS-IDLOPNRM                 PIC X(9)    VALUE SPACE.                 
009100 77  WS-IDINLVGN                 PIC X(3)    VALUE SPACE.                 
009200                                                                          
009300 77  TORG-ADINLOMR               PIC X(4)    VALUE SPACE.                 
009400 77  TORG-ADLAGOMR               PIC S9(2)   VALUE +0.                    
009500 77  TORG-ADGANG-FOM             PIC S9(2)   VALUE +0.                    
009600 77  TORG-ADGANG-TOM             PIC S9(2)   VALUE +0.                    
009700*    --- SWITCHAR                                                         
009800 77  INDATA-SW                   PIC X       VALUE 'J'.                   
009900     88  INDATA-OK                           VALUE 'J'.                   
010000     88  INDATA-FEL                          VALUE 'N'.                   
010100                                                                          
010200 77  INPUT-SW                    PIC X       VALUE 'J'.                   
010300     88  INPUT-FINNS                         VALUE 'J'.                   
010400     88  INPUT-NIX                           VALUE 'N'.                   
010500                                                                          
010600 77  KDINLOMR-SW                 PIC X       VALUE 'J'.                   
010700     88  KDINLOMR-OK                         VALUE 'J'.                   
010800     88  KDINLOMR-FEL                        VALUE 'N'.                   
010900                                                                          
011000 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
011100     88  NYCKLAR-OK                          VALUE 'J'.                   
011200     88  NYCKLAR-FEL                         VALUE 'N'.                   
011300                                                                          
011400 77  KOLLI-TRAEFF-SW             PIC X       VALUE 'J'.                   
011500     88  KOLLI-TRAEFF                        VALUE 'J'.                   
011600     88  EJ-KOLLI-TRAEFF                     VALUE 'N'.                   
011700                                                                          
011800 77  TORG-SW                     PIC X       VALUE 'N'.                   
011900     88  EJ-TORG                             VALUE 'N'.                   
012000     88  TORG-LAES                           VALUE 'J'.                   
012100                                                                          
012200 77  BAS-LAES-SW                 PIC X       VALUE 'F'.                   
012300     88  LAES-PLAC                           VALUE 'F'.                   
012400     88  LAES-ADR                            VALUE 'H'.                   
012500                                                                          
012600 77  KDCMDVAL-SW                 PIC X       VALUE 'N'.                   
012700     88  KDCMDVAL-IFYLLD                     VALUE 'J'.                   
012800                                                                          
012900 77  SKAPA-RAD1-SW               PIC X       VALUE 'N'.                   
013000     88  SKAPA-RAD1                          VALUE 'J'.                   
013100                                                                          
013200 77  SPAR-RAD1-SW                PIC X       VALUE 'N'.                   
013300     88  SPAR-RAD1-FINNS                     VALUE 'J'.                   
013400                                                                          
013500 77  FOERSTA-6191-SW             PIC X       VALUE 'J'.                   
013600     88  FOERSTA-6191                        VALUE 'J'.                   
013700                                                                          
013800 77  FOERSTA-6197-SW             PIC X       VALUE 'J'.                   
013900     88  FOERSTA-6197                        VALUE 'J'.                   
014000                                                                          
014100 77  PRIO-LAESNING-SW            PIC X       VALUE 'N'.                   
014200     88  PRIO-LAESNING                       VALUE 'J'.                   
014300                                                                          
014400 77  OPACKAD-SW                  PIC X       VALUE 'N'.                   
014500     88  PACKAD                              VALUE 'N'.                   
014600                                                                          
014700 77  FBRAPP-SW                   PIC X       VALUE 'J'.                   
014800     88  FBRAPP                              VALUE 'J'.                   
014900                                                                          
015000 77  FLPREPFF-SW                 PIC X       VALUE 'J'.                   
015100     88  FLPREPFF-VISAS                      VALUE 'J'.                   
015200     88  FLPREPFF-VISAS-EJ                   VALUE 'N'.                   
015300                                                                          
015400 01  SOEKVAEG-KOD                PIC 9(2)    VALUE ZERO.                  
015500     EJECT                                                                
015600 01  MESSAGE-CODES.                                                       
015700     03  INF-UPDATE-DONE         PIC X(3)    VALUE '001'.                 
015800     03  INF-FIRST-PAGE          PIC X(3)    VALUE '010'.                 
015900     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '011'.                 
016000     03  INF-NO-MORE-INFO        PIC X(3)    VALUE '012'.                 
016100     03  INF-PRESS-PF11          PIC X(3)    VALUE '013'.                 
016200     03  ERR-UPD-NOT-ALLOWED     PIC X(3)    VALUE '007'.                 
016300     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '014'.                 
016400     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '020'.                 
016500     03  ERR-WRONG-KEY           PIC X(3)    VALUE '022'.                 
016600     03  ERR-WRONG-VALUE         PIC X(3)    VALUE '023'.                 
016700     03  ERR-MISSING-DATA        PIC X(3)    VALUE '027'.                 
016800     03  ERR-LOT-NOT-ON-LOC      PIC X(3)    VALUE '224'.                 
016900     03  ERR-KIT-MARK-CASE       PIC X(3)    VALUE '334'.                 
017000     EJECT                                                                
017100*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
017200 01  GENERELLA-SUBPROGRAM.                                                
017300     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
017400     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
017500     03  WTRAUTF8                PIC X(8)    VALUE 'WTRAUTF8'.            
017600     03  W611PMRK                PIC X(8)    VALUE 'W611PMRK'.            
017700                                                                          
017800*01  -COPY W611PMRK                                                       
017900     EJECT                                                                
018000*    --- SPAR AREOR FÖR MATCHNING AV PLAC OCH ADR INDEX                   
018100*01  -COPY W6D111   -PRE SEQE-                                            
018200     EJECT                                                                
018300*01  -COPY W6D121   -PRE SEQE-                                            
018400     EJECT                                                                
018500*01  -COPY W6D111   -PRE SEQG-                                            
018600     EJECT                                                                
018700*01  -COPY W6D121   -PRE SEQG-                                            
018800     EJECT                                                                
018900*    --- SPAR AREOR FÖR SKAPANDE AV NY RAD1 PÅ INLA21                     
019000*01  -COPY W6D121   -PRE SPAR-                                            
019100     EJECT                                                                
019200 01  FILLER                      PIC X(16)  VALUE 'P-TO-P-AREA'.          
019300 01      P-TO-P-SW.                                                       
019400  02     P-TO-P-KVLL             PIC S9(4)           COMP SYNC.           
019500  02     P-TO-P-KDZ1             PIC X(1)  VALUE LOW-VALUE.               
019600  02     P-TO-P-KDZ2             PIC X(1)  VALUE LOW-VALUE.               
019700  02     P-TO-P-KDTRANS          PIC X(8).                                
019800  02     P-TO-P-IDTRANS          PIC X(4).                                
019900  02     P-TO-P-KDMFSFOR         PIC X(1).                                
020000  02     P-TO-P-DATA             PIC X(1551).                             
020100     EJECT                                                                
020200 01      FILLER                  PIC X(24)   VALUE                        
020300                                 'MOD6191-MID-W6I19101'.                  
020400     SKIP2                                                                
020500     -COPY W6I19101 -PRE MOD6191-                                         
020600     EJECT                                                                
020700 01      FILLER                  PIC X(24)   VALUE                        
020800                                 'MOD6197-MID-W6I19701'.                  
020900     SKIP2                                                                
021000     -COPY W6I19701 -PRE 6197-                                            
021100     EJECT                                                                
021101 01  FILLER                      PIC X(16)   VALUE 'DC CODES   '.         
021110*   -COPY WWDC99                                                          
021120                                                                          
021200 01  FILLER                  PIC X(16)  VALUE 'WTRAUTF8-AREA   '.         
021300*01  -COPY WTRAUTF8                                                       
021400                                                                          
021500 01  WS-IDSKYLT-SE               PIC X(3) VALUE 'S  '.                    
021600 01  WS-IDSKYLT-GB               PIC X(3) VALUE 'GB '.                    
021700 01  WS-IDSKYLT-CN               PIC X(3) VALUE 'RCN'.                    
021800                                                                          
021900 01  WS-CP-UNICODE               PIC X(4)  VALUE 'UTF8'.                  
022000 01  WS-CP-EBCDIC                PIC X(3)  VALUE '278'.                   
022100     EJECT                                                                
022200 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
022300     SKIP3                                                                
022400*01  -COPY WMFSAREA                                                       
022500     EJECT                                                                
022600*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
022700*                                                                         
022800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
022900     SKIP3                                                                
023000 01  NYCKLAR-TILL-DLI.                                                    
023100     03  W-W6GXKEY-6005-X.                                                
023200         05  W-6005-IDHTYP       PIC X(4)    VALUE '6005'.                
023300         05  W-6005-IDDC         PIC X(2)    VALUE SPACE.                 
023400         05  FILLER              PIC X(24)   VALUE LOW-VALUE.             
023500                                                                          
023600     03  W-W6GXKEY-6006-X.                                                
023700         05  W-6006-ADINLOMR     PIC X(4)    VALUE SPACE.                 
023800         05  FILLER              PIC X(1)    VALUE LOW-VALUE.             
023900                                                                          
024000     03  W-W6D1E1KY-X.                                                    
024100         05  W-D1E1KY-ADINLOMR      PIC X(4)         VALUE SPACE.         
024200         05  W-D1E1KY-KDINLPRIO     PIC S9(3) COMP-3 VALUE ZERO.          
024300         05  W-D1E1KY-IDRADNR-INL   PIC S9(5) COMP-3 VALUE ZERO.          
024400         05  W-D1E1KY-IDDC          PIC  X(2) VALUE SPACE.                
024500         05  W-D1E1KY-IDLEVNR       PIC  X(5) VALUE SPACE.                
024600         05  W-D1E1KY-IDFS          PIC X(8)         VALUE SPACE.         
024700         05  W-D1E1KY-TIAVIDAT      PIC S9(7) COMP-3 VALUE ZERO.          
024800         05  W-D1E1KY-IDRADNR       PIC S9(5) COMP-3 VALUE ZERO.          
024900                                                                          
025000     03  W-W6D1E1KY-MIN-X.                                                
025100         05  W-D1E1KY-ADINLOMR-MIN  PIC X(4)         VALUE SPACE.         
025200         05  W-D1E1KY-KDINLPRIO-MIN PIC S9(3) COMP-3 VALUE ZERO.          
025300         05  W-D1E1KY-IDRADNR-INL-MIN PIC S9(5) COMP-3 VALUE ZERO.        
025400         05  W-D1E1KY-IDDC-MIN      PIC  X(2) VALUE SPACE.                
025500         05  W-D1E1KY-IDLEVNR-MIN   PIC  X(5) VALUE SPACE.                
025600         05  W-D1E1KY-IDFS-MIN      PIC X(8)         VALUE SPACE.         
025700         05  W-D1E1KY-TIAVIDAT-MIN  PIC S9(7) COMP-3 VALUE ZERO.          
025800         05  W-D1E1KY-IDRADNR-MIN   PIC S9(5) COMP-3 VALUE ZERO.          
025900                                                                          
026000     03  W-W6D1E1KY-MAX-X.                                                
026100         05  W-D1E1KY-ADINLOMR-MAX  PIC X(4)         VALUE SPACE.         
026200         05  W-D1E1KY-KDINLPRIO-MAX PIC S9(3) COMP-3 VALUE ZERO.          
026300         05  W-D1E1KY-IDRADNR-INL-MAX PIC S9(5) COMP-3 VALUE ZERO.        
026400         05  W-D1E1KY-IDDC-MAX      PIC  X(2) VALUE SPACE.                
026500         05  W-D1E1KY-IDLEVNR-MAX   PIC  X(5)        VALUE SPACE.         
026600         05  W-D1E1KY-IDFS-MAX      PIC X(8)         VALUE SPACE.         
026700         05  W-D1E1KY-TIAVIDAT-MAX  PIC S9(7) COMP-3 VALUE ZERO.          
026800         05  W-D1E1KY-IDRADNR-MAX   PIC S9(5) COMP-3 VALUE ZERO.          
026900                                                                          
027000     03  W-W6D1G1KY-X.                                                    
027100       05  W-D1G1KY-ADINLOMR-NXT     PIC  X(4)        VALUE SPACE.        
027200       05  W-D1G1KY-KDINLPRIO        PIC S9(3) COMP-3 VALUE ZERO.         
027300       05  W-D1G1KY-IDRADNR-INL      PIC S9(5) COMP-3 VALUE ZERO.         
027400       05  W-D1G1KY-IDDC             PIC  X(2) VALUE SPACE.               
027500       05  W-D1G1KY-IDLEVNR          PIC  X(5)        VALUE SPACE.        
027600       05  W-D1G1KY-IDFS             PIC X(8)        VALUE SPACE.         
027700       05  W-D1G1KY-TIAVIDAT         PIC S9(7) COMP-3 VALUE ZERO.         
027800       05  W-D1G1KY-IDRADNR          PIC S9(5) COMP-3 VALUE ZERO.         
027900                                                                          
028000     03  W-W6D1G1KY-MIN-X.                                                
028100       05  W-D1G1KY-ADINLOMR-NXT-MIN PIC  X(4)        VALUE SPACE.        
028200       05  W-D1G1KY-KDINLPRIO-MIN    PIC S9(3) COMP-3 VALUE ZERO.         
028300       05  W-D1G1KY-IDRADNR-INL-MIN  PIC S9(5) COMP-3 VALUE ZERO.         
028400       05  W-D1G1KY-IDDC-MIN         PIC  X(2) VALUE SPACE.               
028500       05  W-D1G1KY-IDLEVNR-MIN      PIC  X(5)        VALUE SPACE.        
028600       05  W-D1G1KY-IDFS-MIN         PIC X(8)        VALUE SPACE.         
028700       05  W-D1G1KY-TIAVIDAT-MIN     PIC S9(7) COMP-3 VALUE ZERO.         
028800       05  W-D1G1KY-IDRADNR-MIN      PIC S9(5) COMP-3 VALUE ZERO.         
028900                                                                          
029000     03  W-W6D1G1KY-MAX-X.                                                
029100       05  W-D1G1KY-ADINLOMR-NXT-MAX PIC  X(4)        VALUE SPACE.        
029200       05  W-D1G1KY-KDINLPRIO-MAX    PIC S9(3) COMP-3 VALUE ZERO.         
029300       05  W-D1G1KY-IDRADNR-INL-MAX  PIC S9(5) COMP-3 VALUE ZERO.         
029400       05  W-D1G1KY-IDDC-MAX         PIC  X(2) VALUE SPACE.               
029500       05  W-D1G1KY-IDLEVNR-MAX      PIC  X(5)        VALUE SPACE.        
029600       05  W-D1G1KY-IDFS-MAX         PIC X(8)        VALUE SPACE.         
029700       05  W-D1G1KY-TIAVIDAT-MAX     PIC S9(7) COMP-3 VALUE ZERO.         
029800       05  W-D1G1KY-IDRADNR-MAX      PIC S9(5) COMP-3 VALUE ZERO.         
029900                                                                          
030000     03  W-W6D101KY-X.                                                    
030100         05  W-D101KY-IDDC       PIC X(2)     VALUE SPACE.                
030200         05  W-D101KY-IDLEVNR    PIC  X(5)           VALUE SPACE.         
030300         05  W-D101KY-IDFS       PIC X(8)     VALUE SPACE.                
030400         05  W-D101KY-TIAVIDAT   PIC S9(7)    COMP-3 VALUE ZERO.          
030500                                                                          
030600     03  W-W6D1BSEQ-X.                                                    
030700         05  W-D1BSEQ-IDLOPNRM   PIC S9(9)    COMP-3 VALUE ZERO.          
030800                                                                          
030900     03  W-W6D1CSEQ-X.                                                    
031000         05  W-D1CSEQ-IDLEVNR    PIC  X(5)           VALUE SPACE.         
031100         05  W-D1CSEQ-IDOKOLLI   PIC 9(9)    VALUE ZERO.                  
031200                                                                          
031300     03  W-IDARTNR-X.                                                     
031400         05  W-IDARTNR           PIC S9(9)   COMP-3 VALUE ZERO.           
031500     03  W-KDSEGKEY-X.                                                    
031600         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
031700     03  W-IDLEVNRK-X.                                                    
031800         05  W-IDLEVNRK          PIC  X(5)   VALUE SPACE.                 
031900                                                                          
032000     03  W-IDDC-X.                                                        
032100         05  W-IDDC              PIC  X(2)   VALUE SPACE.                 
032200                                                                          
032300     03  W-IDSKYLT-X.                                                     
032400         05  W-IDSKYLT           PIC  X(3)   VALUE 'GB '.                 
032500                                                                          
032600     03  W-IDDC-B6-X.                                                     
032700         05 W-IDDC-B6            PIC X(2).                                
032800                                                                          
032900     03  W-IDOKOLLI-X.                                                    
033000         05  W-IDOKOLLI          PIC 9(9)   VALUE ZERO.                   
033100                                                                          
033200     03  W-IDRADNR-INL-X.                                                 
033300         05  W-IDRADNR-INL       PIC S9(5)   VALUE ZERO COMP-3.           
033400                                                                          
033500     03  W-IDRADNR-X.                                                     
033600         05  W-IDRADNR           PIC S9(5)   VALUE ZERO COMP-3.           
033700                                                                          
033800     03  W-IDLOPNRM-X.                                                    
033900         05  W-IDLOPNRM          PIC S9(9)   VALUE ZERO COMP-3.           
034000                                                                          
034100     03  W-ADINLOMR-X.                                                    
034200         05  W-ADINLOMR          PIC  X(4)   VALUE SPACE.                 
034300                                                                          
034400     03  W-ADINLOMR-NXT-X.                                                
034500         05  W-ADINLOMR-NXT      PIC  X(4)   VALUE SPACE.                 
034600                                                                          
034700     03  W-KDINLSTA              PIC  X(3)   VALUE SPACE.                 
034800                                                                          
034900     03  W-IDINLVGN              PIC  9(3)   VALUE ZERO.                  
035000                                                                          
035100     03  W-FLINLFB               PIC  X(1)   VALUE 'N'.                   
035200                                                                          
035300     03  W-FLINLFP               PIC  X(1)   VALUE 'N'.                   
035400                                                                          
035500     SKIP2                                                                
035600*    --- STATUS-KOD FRÅN IMS                                              
035700 01  STATUS-WS                   PIC XX.                                  
035800     88  SEGMENT-FINNS                       VALUE '  '.                  
035900     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
036000     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
036100                                                                          
036200 01  INLF-STATUS-WS              PIC XX.                                  
036300     88  INLF-SEGMENT-FINNS                  VALUE '  '.                  
036400     88  INLF-SEGMENT-SAKNAS                 VALUE 'GE'.                  
036500                                                                          
036600 01  INLH-STATUS-WS              PIC XX.                                  
036700     88  INLH-SEGMENT-FINNS                  VALUE '  '.                  
036800     88  INLH-SEGMENT-SAKNAS                 VALUE 'GE'.                  
036900                                                                          
037000 01  GODK-STATUSKODER.                                                    
037100     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
037200     SKIP3                                                                
037300 01  SSA1                        PIC X(160).                              
037400 01  SSA2                        PIC X(64).                               
037500 01  SSA3                        PIC X(64).                               
037600     EJECT                                                                
037700*    --- IMS FUNKTIONSKODER                                               
037800*01  -COPY W0003                                                          
037900     EJECT                                                                
038000*    ---  DLI INPUT-OUTPUT AREA                                           
038100 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-AREA1'.         
038200                                                                          
038300 01  DLI-IO-AREA1.                                                        
038400     03  IO-AREA1                PIC X(150)  VALUE SPACE.                 
038500                                                                          
038600     03  W6INLF01 REDEFINES IO-AREA1.                                     
038700*        05  -COPY W6D1E1                                                 
038800     EJECT                                                                
038900     03  W6PLAA11 REDEFINES IO-AREA1.                                     
039000*        05  -COPY W6GX6006 -PRE PLAA-                                    
039100     EJECT                                                                
039200 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-AREA2'.         
039300                                                                          
039400 01  DLI-IO-AREA2.                                                        
039500     03  IO-AREA2                PIC X(150)  VALUE SPACE.                 
039600                                                                          
039700     03  W6INLA11 REDEFINES IO-AREA2.                                     
039800*        05  -COPY W6D111                                                 
039900     EJECT                                                                
040000 01  DLI-IO-AREA3.                                                        
040100     03  IO-AREA3                PIC X(150)  VALUE SPACE.                 
040200                                                                          
040300     03  W6INLA21 REDEFINES IO-AREA3.                                     
040400*        05  -COPY W6D121                                                 
040500     EJECT                                                                
040600 01  DLI-IO-AREA4.                                                        
040700     03  IO-AREA4                PIC X(150)  VALUE SPACE.                 
040800                                                                          
040900     03  W6INLH01 REDEFINES IO-AREA4.                                     
041000*        05  -COPY W6D1G1                                                 
041100     EJECT                                                                
041200 01  DLI-IO-AREA5.                                                        
041300     03  IO-AREA5                PIC X(150)  VALUE SPACE.                 
041400                                                                          
041500     03  W6INLC01 REDEFINES IO-AREA5.                                     
041600*        05  -COPY W6D1B1                                                 
041700     EJECT                                                                
041800 01  DLI-IO-AREA-WDK6.                                                    
041900     03  WDK611.                                                          
042000*        05  -COPY WDK611                                                 
042100     EJECT                                                                
042200 01  DLI-IO-AREA-WDK7.                                                    
042300     03  WDK711.                                                          
042400*        05  -COPY WDK711                                                 
042500                                                                          
042600 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
042700 01   DLI-IO-AREA-B601.                                                   
042800*     03  -COPY WDB601                                                    
042900                                                                          
043000 01  FILLER                PIC X(16) VALUE 'DLI-IO-WDD311'.               
043100 01  DLI-IO-WDD311.                                                       
043200*    03  -COPY WDD311                                                     
043300                                                                          
043400     EJECT                                                                
043500 LINKAGE SECTION.                                                         
043600 01  REQU-AREA.                                                           
043700*    03 -COPY WZ01REQU                                                    
043800*    03 -COPY W60132I1                                                    
043900     EJECT                                                                
044000 01  RESP-AREA.                                                           
044100*    03 -COPY WZ01RESP                                                    
044200*    03 -COPY W60132O1                                                    
044300     EJECT                                                                
044400 01  MAX-KVRADER                 PIC S9(4) COMP.                          
044500                                                                          
044600*01  -COPY W0009  -PRE ALT-                                               
044700*01  -COPY W0009  -PRE 6197-                                              
044800     EJECT                                                                
044900*01  -COPY W0008  -PRE INLA1-                                             
045000     05  FILLER                  PIC X.                                   
045100     EJECT                                                                
045200*01  -COPY W0008  -PRE INLA2-                                             
045300     05  FILLER                  PIC X.                                   
045400     EJECT                                                                
045500*01  -COPY W0008  -PRE INLA3-                                             
045600     05  FILLER                  PIC X.                                   
045700     EJECT                                                                
045800*01  -COPY W0008  -PRE INLC-                                              
045900     05  FILLER                  PIC X.                                   
046000     EJECT                                                                
046100*01  -COPY W0008  -PRE INLF-                                              
046200     05  FILLER                  PIC X.                                   
046300     EJECT                                                                
046400*01  -COPY W0008  -PRE INLH-                                              
046500     05  FILLER                  PIC X.                                   
046600     EJECT                                                                
046700*01  -COPY W0008  -PRE PLAA-                                              
046800     05  FILLER                  PIC X.                                   
046900     EJECT                                                                
047000*01  -COPY W0008  -PRE WDK6-                                              
047100     05  FILLER                  PIC X.                                   
047200     EJECT                                                                
047300*01  -COPY W0008  -PRE WDK7-                                              
047400     05  FILLER                  PIC X.                                   
047500     EJECT                                                                
047600*01  -COPY W0008  -PRE WDB6-                                              
047700     05  FILLER                  PIC X.                                   
047800     EJECT                                                                
047900*01  -COPY W0008  -PRE WDD3-                                              
048000     05  FILLER                  PIC X.                                   
048100     EJECT                                                                
048200**  PCB'ER FÖR SUBPGM                                                     
048300 01  PMRK-INLB-PCB               PIC X.                                   
048400                                                                          
048500 01  PMRK-INLC-PCB               PIC X.                                   
048600                                                                          
048700 01  PMRK-PLAA-PCB               PIC X.                                   
048800                                                                          
048900     EJECT                                                                
049000 PROCEDURE DIVISION  USING REQU-AREA RESP-AREA MAX-KVRADER                
049100                           ALT-PCB 6197-PCB                               
049200                           INLA1-PCB INLA2-PCB INLA3-PCB                  
049300                           INLC-PCB INLF-PCB INLH-PCB  PLAA-PCB           
049400                           WDK6-PCB WDK7-PCB WDB6-PCB WDD3-PCB            
049500                           PMRK-INLB-PCB PMRK-INLC-PCB                    
049600                           PMRK-PLAA-PCB.                                 
049700                                                                          
049800     PERFORM A-INIT                                                       
049900     PERFORM B-KOLLA-NYCKLAR                                              
050000     IF NYCKLAR-OK                                                        
050100       IF REQU-UPDATE                                                     
050200         PERFORM G-KOLLA-INPUT                                            
050300         IF INDATA-OK                                                     
050400           PERFORM H-UPPDATERA                                            
050500         END-IF                                                           
050600       ELSE                                                               
050700         IF REQU-FIRST                                                    
050800           PERFORM C-FOERSTA-SIDA                                         
050900         ELSE                                                             
051000           IF REQU-NEXT                                                   
051100             PERFORM D-NAESTA-SIDA                                        
051200           ELSE                                                           
051300             PERFORM E-SAMMA-SIDA                                         
051400           END-IF                                                         
051500         END-IF                                                           
051600       END-IF                                                             
051700       IF INDATA-OK                                                       
051800         PERFORM F-LAES-VISA-INFO                                         
051900       END-IF                                                             
052000     END-IF                                                               
052100                                                                          
052200     GOBACK                                                               
052300     .                                                                    
052400     EJECT                                                                
052500 A-INIT SECTION.                                                          
052600                                                                          
052700     MOVE ALL '+'       TO RESP-W60132O1                                  
052800     PERFORM MFS-FORM-ATTR                                                
052900     IF MAX-KVRADER > +12                                                 
053000* DO IT ONLY FOR WEB!                                                     
053100       MOVE +1          TO INDX                                           
053200       PERFORM UNTIL INDX > MAX-KVRADER                                   
053300         MOVE ALL X'2B' TO RESP-BEART-LINE (INDX)                         
053400         ADD +1         TO INDX                                           
053500       END-PERFORM                                                        
053600     END-IF                                                               
053700     MOVE 001          TO RESP-IDMSGVER                                   
053800     MOVE SPACE        TO RESP-IDMSG-ERROR                                
053900                          RESP-IDMSG-INFO                                 
054000                          RESP-IDELMT-ERROR                               
054100                                                                          
054200     MOVE JA           TO INDATA-SW                                       
054300     MOVE REQU-KVRADER TO RESP-KVRADER                                    
054400     .                                                                    
054500     EJECT                                                                
054600 B-KOLLA-NYCKLAR SECTION.                                                 
054700                                                                          
054800     MOVE JA                   TO NYCKLAR-SW                              
054900                                                                          
055000     PERFORM BA-KOLLA-IDDC                                                
055100     PERFORM BH-KOLLA-ADINLOMR                                            
055200     PERFORM BB-KOLLA-ADINLOMR-NXT                                        
055300     PERFORM BC-KOLLA-KDINLQ                                              
055400     PERFORM BD-KOLLA-BEFT                                                
055500     PERFORM BE-KOLLA-FLINLFB                                             
055600                                                                          
055700     IF (WS-ADINLOMR           NOT = WS-ADINLOMR-NXT AND                  
055800         WS-ADINLOMR           NOT = SPACE AND                            
055900         WS-ADINLOMR-NXT       NOT = SPACE)   OR                          
056000        (WS-ADINLOMR           = SPACE  AND                               
056100         WS-ADINLOMR-NXT       = SPACE)                                   
056200         MOVE NEJ              TO NYCKLAR-SW                              
056300         MOVE 'ADINLOMR'       TO RESP-IDELMT-ERROR                       
056400      ELSE                                                                
056500         PERFORM BG-KOLLA-NYCKEL-KOMB                                     
056600     END-IF                                                               
056700     MOVE WS-ADINLOMR          TO RESP-ADINLOMR-KEY                       
056800     MOVE WS-ADINLOMR-NXT      TO RESP-ADINLOMR-NXT-KEY                   
056900     IF TORG-ADINLOMR     NOT = SPACE                                     
057000       MOVE TORG-ADINLOMR      TO RESP-ADINLOMR-KEY                       
057100       IF RESP-ADINLOMR-NXT-KEY NOT = SPACE                               
057200          MOVE TORG-ADINLOMR   TO RESP-ADINLOMR-NXT-KEY                   
057300       END-IF                                                             
057400     END-IF                                                               
057500                                                                          
057600     IF NYCKLAR-FEL                                                       
057700       MOVE ERR-WRONG-KEY      TO RESP-IDMSG-ERROR                        
057800       MOVE ZERO               TO RESP-KVRADER                            
057900     END-IF                                                               
058000     .                                                                    
058100     EJECT                                                                
058200 BA-KOLLA-IDDC        SECTION.                                            
058300                                                                          
058400     MOVE REQU-IDDC-KEY       TO W-IDDC-B6                                
058500     PERFORM IMS-GU-WDB601                                                
058600                                                                          
058700     IF DCS-KDDC = SPACE OR DCS-DDC                                       
058800         MOVE NEJ          TO NYCKLAR-SW                                  
058900         MOVE 'IDDC'       TO RESP-IDELMT-ERROR                           
059000     ELSE                                                                 
059100         MOVE DCS-IDDC     TO W-6005-IDDC                                 
059200                              W-IDDC                                      
059210                              WS-IDDC                                     
059300     END-IF                                                               
059400     .                                                                    
059500     EJECT                                                                
059600 BB-KOLLA-ADINLOMR-NXT  SECTION.                                          
059700                                                                          
059800     MOVE REQU-ADINLOMR-NXT-KEY TO WS-ADINLOMR-NXT                        
059900                                                                          
060000     IF WS-ADINLOMR-NXT        NOT = SPACE                                
060100         MOVE WS-ADINLOMR-NXT  TO W-6006-ADINLOMR                         
060200         PERFORM IMS-GU-PLAA-PLAA11                                       
060300         IF SEGMENT-FINNS                                                 
060400             MOVE PLAA-6006-KDINLOMR  TO W-KDINLOMR-NXT                   
060500             IF W-KDINLOMR-NXT = 'RTA'                                    
060600               MOVE PLAA-6006-ADINLOMR-PAR TO W-6006-ADINLOMR             
060700               PERFORM IMS-GU-PLAA-PLAA11-BLANK                           
060800               IF PLAA-6006-KDINLOMR = 'FB ' OR 'F  ' OR 'FBP'            
060900                 MOVE PLAA-6006-KDINLOMR TO W-KDINLOMR-RTA-PAR            
061000               END-IF                                                     
061100             END-IF                                                       
061200             IF PLAA-6006-ADINLOMR-PAR = '10  ' AND                       
061300                PLAA-6006-KDINLOMR     = 'TRG'                            
061400                MOVE PLAA-6006-ADGANG-FOM   TO TORG-ADGANG-FOM            
061500                MOVE PLAA-6006-ADGANG-TOM   TO TORG-ADGANG-TOM            
061600                MOVE WS-ADINLOMR-NXT        TO TORG-ADINLOMR              
061700                MOVE '10  '                 TO WS-ADINLOMR-NXT            
061800                MOVE 'LO  '                 TO W-KDINLOMR-NXT             
061900                MOVE +10                    TO TORG-ADLAGOMR              
062000                MOVE JA                     TO TORG-SW                    
062100             END-IF                                                       
062200          ELSE                                                            
062300             MOVE SPACE               TO W-KDINLOMR-NXT                   
062400             MOVE NEJ                 TO NYCKLAR-SW                       
062500             MOVE 'ADINLOMR'          TO RESP-IDELMT-ERROR                
062600         END-IF                                                           
062700     END-IF                                                               
062800     .                                                                    
062900     EJECT                                                                
063000 BC-KOLLA-KDINLQ      SECTION.                                            
063100                                                                          
063200     MOVE REQU-KDINLQ-KEY      TO WS-KDINLQ                               
063300                                                                          
063400     IF WS-KDINLQ              = SPACE OR 'T' OR 'V' OR 'K'               
063500         IF WS-KDINLQ          = SPACE                                    
063600             MOVE 'T'          TO WS-KDINLQ                               
063700         END-IF                                                           
063800      ELSE                                                                
063900         MOVE NEJ              TO NYCKLAR-SW                              
064000         MOVE 'KDINLQ'         TO RESP-IDELMT-ERROR                       
064100     END-IF                                                               
064200     .                                                                    
064300     EJECT                                                                
064400 BD-KOLLA-BEFT        SECTION.                                            
064500                                                                          
064600     MOVE REQU-BEFT-FOM-KEY    TO WS-BEFT-FOM                             
064700     IF WS-BEFT-FOM            = SPACE                                    
064800         CONTINUE                                                         
064900      ELSE                                                                
065000         IF WS-BEFT-FOM        NUMERIC                                    
065100             MOVE WS-BEFT-FOM  TO W-BEFT-FOM                              
065200          ELSE                                                            
065300             MOVE NEJ          TO NYCKLAR-SW                              
065400             MOVE 'BEFT-FOM'   TO RESP-IDELMT-ERROR                       
065500         END-IF                                                           
065600     END-IF                                                               
065700                                                                          
065800     MOVE REQU-BEFT-TOM-KEY    TO WS-BEFT-TOM                             
065900     IF WS-BEFT-TOM            = SPACE                                    
066000        MOVE WS-BEFT-FOM     TO WS-BEFT-TOM                               
066100                                W-BEFT-TOM                                
066200      ELSE                                                                
066300         IF WS-BEFT-TOM        NUMERIC AND                                
066400            WS-BEFT-FOM        NUMERIC                                    
066500             MOVE WS-BEFT-TOM  TO W-BEFT-TOM                              
066600          ELSE                                                            
066700             MOVE NEJ          TO NYCKLAR-SW                              
066800             MOVE 'BEFT-TOM'   TO RESP-IDELMT-ERROR                       
066900         END-IF                                                           
067000     END-IF                                                               
067100     .                                                                    
067200     EJECT                                                                
067300 BE-KOLLA-FLINLFB     SECTION.                                            
067400                                                                          
067500     MOVE REQU-FLINLFB-KEY     TO WS-FLINLFB                              
067600                                                                          
067700     IF WS-FLINLFB             = SPACE                                    
067800         MOVE NEJ              TO WS-FLINLFB                              
067900      ELSE                                                                
068000         IF WS-FLINLFB         = JA OR NEJ OR YES                         
068100             CONTINUE                                                     
068200          ELSE                                                            
068300             MOVE NEJ          TO NYCKLAR-SW                              
068400             MOVE 'FLINLFB'    TO RESP-IDELMT-ERROR                       
068500         END-IF                                                           
068600     END-IF                                                               
068700     .                                                                    
068800     EJECT                                                                
068900 BG-KOLLA-NYCKEL-KOMB SECTION.                                            
069000                                                                          
069100     EVALUATE TRUE                                                        
069200                                                                          
069300       WHEN WS-ADINLOMR        NOT = SPACE AND                            
069400            WS-ADINLOMR-NXT    = SPACE     AND                            
069500            WS-BEFT-FOM        = SPACE                                    
069600             MOVE 1            TO W-NYCKEL-KOMB                           
069700                                                                          
069800       WHEN WS-ADINLOMR        = SPACE     AND                            
069900            WS-ADINLOMR-NXT    NOT = SPACE AND                            
070000            WS-BEFT-FOM        = SPACE                                    
070100             MOVE 2            TO W-NYCKEL-KOMB                           
070200                                                                          
070300       WHEN WS-ADINLOMR        NOT = SPACE AND                            
070400            WS-ADINLOMR-NXT    NOT = SPACE AND                            
070500            WS-BEFT-FOM        = SPACE                                    
070600             MOVE 3            TO W-NYCKEL-KOMB                           
070700                                                                          
070800       WHEN WS-ADINLOMR        NOT = SPACE AND                            
070900            WS-ADINLOMR-NXT    = SPACE     AND                            
071000            WS-BEFT-FOM        NOT = SPACE                                
071100             MOVE 4            TO W-NYCKEL-KOMB                           
071200                                                                          
071300       WHEN WS-ADINLOMR        = SPACE     AND                            
071400            WS-ADINLOMR-NXT    NOT = SPACE AND                            
071500            WS-BEFT-FOM        NOT = SPACE                                
071600             MOVE 5            TO W-NYCKEL-KOMB                           
071700                                                                          
071800       WHEN WS-ADINLOMR        NOT = SPACE AND                            
071900            WS-ADINLOMR-NXT    NOT = SPACE AND                            
072000            WS-BEFT-FOM        NOT = SPACE                                
072100             MOVE 6            TO W-NYCKEL-KOMB                           
072200                                                                          
072300       WHEN OTHER                                                         
072400            MOVE NEJ           TO NYCKLAR-SW                              
072500                                                                          
072600     END-EVALUATE                                                         
072700     .                                                                    
072800     EJECT                                                                
072900 BH-KOLLA-ADINLOMR  SECTION.                                              
073000                                                                          
073100     MOVE REQU-ADINLOMR-KEY    TO WS-ADINLOMR                             
073200                                                                          
073300     IF WS-ADINLOMR            NOT = SPACE                                
073400         MOVE WS-ADINLOMR      TO W-6006-ADINLOMR                         
073500         PERFORM IMS-GU-PLAA-PLAA11                                       
073600         IF SEGMENT-FINNS                                                 
073700             MOVE PLAA-6006-KDINLOMR TO W-KDINLOMR                        
073800             IF W-KDINLOMR = 'RTA'                                        
073900               MOVE PLAA-6006-ADINLOMR-PAR TO W-6006-ADINLOMR             
074000               PERFORM IMS-GU-PLAA-PLAA11-BLANK                           
074100               IF PLAA-6006-KDINLOMR = 'FB ' OR 'F  ' OR 'FBP'            
074200                 MOVE PLAA-6006-KDINLOMR TO W-KDINLOMR-RTA-PAR            
074300               END-IF                                                     
074400             END-IF                                                       
074500             IF PLAA-6006-ADINLOMR-PAR = '10  ' AND                       
074600                PLAA-6006-KDINLOMR     = 'TRG'                            
074700                MOVE PLAA-6006-ADGANG-FOM   TO TORG-ADGANG-FOM            
074800                MOVE PLAA-6006-ADGANG-TOM   TO TORG-ADGANG-TOM            
074900                MOVE WS-ADINLOMR            TO TORG-ADINLOMR              
075000                MOVE '10  '                 TO WS-ADINLOMR                
075100                MOVE 'LO  '                 TO W-KDINLOMR                 
075200                MOVE +10                    TO TORG-ADLAGOMR              
075300                MOVE JA                     TO TORG-SW                    
075400             END-IF                                                       
075500          ELSE                                                            
075600             MOVE SPACE              TO W-KDINLOMR                        
075700             MOVE NEJ                TO NYCKLAR-SW                        
075800             MOVE 'ADINLOMR'         TO RESP-IDELMT-ERROR                 
075900         END-IF                                                           
076000     END-IF                                                               
076100     .                                                                    
076200     EJECT                                                                
076300 C-FOERSTA-SIDA SECTION.                                                  
076400                                                                          
076500     MOVE INF-FIRST-PAGE       TO RESP-IDMSG-INFO                         
076600     PERFORM MFS-RENSA-FAELT-IN                                           
076700     .                                                                    
076800     EJECT                                                                
076900 D-NAESTA-SIDA SECTION.                                                   
077000                                                                          
077100     MOVE WS-ADINLOMR            TO  W-D1E1KY-ADINLOMR-MIN                
077200                                     W-ADINLOMR                           
077300     MOVE WS-ADINLOMR-NXT        TO  W-D1G1KY-ADINLOMR-NXT-MIN            
077400                                     W-ADINLOMR-NXT                       
077500     MOVE REQU-IDLEVNR-START     TO  W-D1E1KY-IDLEVNR-MIN                 
077600                                     W-D1G1KY-IDLEVNR-MIN                 
077700     MOVE W-IDDC                 TO  W-D1E1KY-IDDC-MIN                    
077800                                     W-D1G1KY-IDDC-MIN                    
077900     MOVE REQU-IDFS-START        TO  W-D1E1KY-IDFS-MIN                    
078000                                     W-D1G1KY-IDFS-MIN                    
078100     MOVE REQU-TIAVIDAT-START    TO  W-D1E1KY-TIAVIDAT-MIN                
078200                                     W-D1G1KY-TIAVIDAT-MIN                
078300     MOVE REQU-IDRADNR-INL-START TO  W-D1E1KY-IDRADNR-INL-MIN             
078400                                     W-D1G1KY-IDRADNR-INL-MIN             
078500     MOVE REQU-IDRADNR-START     TO  W-D1E1KY-IDRADNR-MIN                 
078600                                     W-D1G1KY-IDRADNR-MIN                 
078700     MOVE REQU-KDINLPRIO-START   TO  W-D1E1KY-KDINLPRIO-MIN               
078800                                     W-D1G1KY-KDINLPRIO-MIN               
078900     PERFORM MFS-RENSA-FAELT-IN                                           
079000     .                                                                    
079100     EJECT                                                                
079200 E-SAMMA-SIDA SECTION.                                                    
079300                                                                          
079400     MOVE WS-ADINLOMR         TO W-D1E1KY-ADINLOMR-MIN                    
079500                                 W-ADINLOMR                               
079600     MOVE WS-ADINLOMR-NXT     TO W-D1G1KY-ADINLOMR-NXT-MIN                
079700                                 W-ADINLOMR-NXT                           
079800     MOVE REQU-IDLEVNR-START  TO W-D1E1KY-IDLEVNR-MIN                     
079900                                 W-D1G1KY-IDLEVNR-MIN                     
080000     MOVE REQU-IDFS-START     TO W-D1E1KY-IDFS-MIN                        
080100                                 W-D1G1KY-IDFS-MIN                        
080200     MOVE W-IDDC             TO  W-D1E1KY-IDDC-MIN                        
080300                                 W-D1G1KY-IDDC-MIN                        
080400     MOVE REQU-TIAVIDAT-START TO W-D1E1KY-TIAVIDAT-MIN                    
080500                                 W-D1G1KY-TIAVIDAT-MIN                    
080600     MOVE REQU-IDRADNR-INL-START TO W-D1E1KY-IDRADNR-INL-MIN              
080700                                    W-D1G1KY-IDRADNR-INL-MIN              
080800     MOVE REQU-IDRADNR-START  TO W-D1E1KY-IDRADNR-MIN                     
080900                                 W-D1G1KY-IDRADNR-MIN                     
081000     MOVE REQU-KDINLPRIO-START TO W-D1E1KY-KDINLPRIO-MIN                  
081100                                  W-D1G1KY-KDINLPRIO-MIN                  
081200                                                                          
081300     MOVE +1                     TO INDX                                  
081400     PERFORM UNTIL INDX          >  REQU-KVRADER                          
081500       IF REQU-KDCMDVAL-LINE(INDX) = ALL '+'                              
081600         MOVE SPACE        TO RESP-KDCMDVAL-LINE (INDX)                   
081700       ELSE                                                               
081800         MOVE INF-PRESS-PF11 TO RESP-IDMSG-INFO                           
081900         PERFORM MFS-LAES-IN-IGEN                                         
082000         PERFORM EA-FLYTTA-REQU-TILL-RESP                                 
082100                                                                          
082200* EFTER EA- SKA DENNA LOOP AVSLUTAS                                       
082300         MOVE +500               TO INDX                                  
082400       END-IF                                                             
082500                                                                          
082600       ADD +1                    TO INDX                                  
082700     END-PERFORM                                                          
082800     .                                                                    
082900     EJECT                                                                
083000 EA-FLYTTA-REQU-TILL-RESP SECTION.                                        
083100                                                                          
083200     MOVE +1                              TO VAGN-IX                      
083300     PERFORM UNTIL VAGN-IX                >  MAX-VAGN-IX                  
083400         MOVE REQU-IDINLVGN-SPAR(VAGN-IX) TO                              
083500                                RESP-IDINLVGN-SPAR(VAGN-IX)               
083600         ADD +1                           TO VAGN-IX                      
083700     END-PERFORM                                                          
083800                                                                          
083900     PERFORM UNTIL INDX            >  REQU-KVRADER                        
084000       MOVE REQU-KDCMDVAL-LINE(INDX) TO RESP-KDCMDVAL-LINE(INDX)          
084100       ADD +1                    TO INDX                                  
084200     END-PERFORM                                                          
084300     .                                                                    
084400     EJECT                                                                
084500 F-LAES-VISA-INFO SECTION.                                                
084600                                                                          
084700     MOVE 'GE'                 TO INLF-STATUS-WS                          
084800                                  INLH-STATUS-WS                          
084900     MOVE +1                   TO INDX                                    
085000                                                                          
085100     PERFORM S20-BESTAEM-SOEKVAEG                                         
085200                                                                          
085300     PERFORM S01-LAES-RADDATA                                             
085400                                                                          
085500     MOVE ZERO                 TO W-KVINLCAR                              
085600                                  W-KVRADER-HIT                           
085700                                  W-KVRADER-TOT                           
085800                                  W-KVRADER-PRIO                          
085900                                  RESP-KVRADER                            
086000                                                                          
086100     IF REQU-FIRST                                                        
086200         MOVE +1               TO VAGN-IX                                 
086300         PERFORM UNTIL VAGN-IX > MAX-VAGN-IX                              
086400             MOVE ZERO         TO REQU-IDINLVGN-SPAR(VAGN-IX)             
086500             ADD +1            TO VAGN-IX                                 
086600         END-PERFORM                                                      
086700     END-IF                                                               
086800                                                                          
086900     IF INLF-SEGMENT-SAKNAS AND INLH-SEGMENT-SAKNAS                       
087000         IF REQU-NEXT                                                     
087100             MOVE INF-NO-MORE-INFO  TO RESP-IDMSG-INFO                    
087200          ELSE                                                            
087300             MOVE ERR-MISSING-DATA  TO RESP-IDMSG-ERROR                   
087400         END-IF                                                           
087500         MOVE ZERO             TO RESP-TIAVIDAT-START                     
087600                                  RESP-IDRADNR-INL-START                  
087700                                  RESP-IDRADNR-START                      
087800                                  RESP-KDINLPRIO-START                    
087900         MOVE SPACE            TO RESP-IDFS-START                         
088000                                  RESP-IDLEVNR-START                      
088100      ELSE                                                                
088200         MOVE W-SPAR-IDLEVNR   TO RESP-IDLEVNR-START                      
088300         MOVE W-SPAR-IDFS      TO RESP-IDFS-START                         
088400         MOVE W-SPAR-TIAVIDAT  TO RESP-TIAVIDAT-START                     
088500         MOVE W-SPAR-IDRADNR-INL TO RESP-IDRADNR-INL-START                
088600         MOVE W-SPAR-IDRADNR   TO RESP-IDRADNR-START                      
088700         MOVE W-SPAR-KDINLPRIO TO RESP-KDINLPRIO-START                    
088800     END-IF                                                               
088900                                                                          
089000     PERFORM UNTIL INDX > MAX-KVRADER                                     
089100       IF INLF-SEGMENT-FINNS OR INLH-SEGMENT-FINNS                        
089200         PERFORM FA-FYLL-I-RESP-RAD                                       
089300         ADD 1         TO INDX                                            
089400                          RESP-KVRADER                                    
089500         PERFORM S01-LAES-RADDATA                                         
089600       ELSE                                                               
089700         COMPUTE INDX = MAX-KVRADER + 1                                   
089800       END-IF                                                             
089900     END-PERFORM                                                          
090000                                                                          
090100     IF INLF-SEGMENT-FINNS OR INLH-SEGMENT-FINNS                          
090200       MOVE W-SPAR-IDLEVNR       TO RESP-IDLEVNR-NEXT                     
090300       MOVE W-SPAR-IDFS          TO RESP-IDFS-NEXT                        
090400       MOVE W-SPAR-TIAVIDAT      TO RESP-TIAVIDAT-NEXT                    
090500       MOVE W-SPAR-IDRADNR-INL   TO RESP-IDRADNR-INL-NEXT                 
090600       MOVE W-SPAR-IDRADNR       TO RESP-IDRADNR-NEXT                     
090700       MOVE W-SPAR-KDINLPRIO     TO RESP-KDINLPRIO-NEXT                   
090800       IF RESP-IDMSG-INFO        = SPACE                                  
090900         MOVE INF-MORE-INFO-EXISTS TO RESP-IDMSG-INFO                     
091000       END-IF                                                             
091100       IF REQU-FIRST                                                      
091200         IF EJ-TORG                                                       
091300            IF RAD-KDINLPRIO <  31                                        
091400              MOVE JA            TO PRIO-LAESNING-SW                      
091500            END-IF                                                        
091600           PERFORM FB-RAEKNA-ALLA-RADER                                   
091700         END-IF                                                           
091800       END-IF                                                             
091900     ELSE                                                                 
092000       MOVE HIGH-VALUE           TO RESP-IDLEVNR-NEXT                     
092100       MOVE 999999               TO RESP-TIAVIDAT-NEXT                    
092200       MOVE 99999                TO RESP-IDRADNR-INL-NEXT                 
092300       MOVE 99999                TO RESP-IDRADNR-NEXT                     
092400       MOVE 99                   TO RESP-KDINLPRIO-NEXT                   
092500       MOVE HIGH-VALUE           TO RESP-IDFS-NEXT                        
092600       IF REQU-FIRST                                                      
092700          MOVE SPACE             TO RESP-IDMSG-ERROR                      
092800       ELSE                                                               
092900         IF RESP-IDMSG-INFO    =  SPACE                                   
093000           MOVE INF-NO-MORE-INFO TO RESP-IDMSG-INFO                       
093100         END-IF                                                           
093200       END-IF                                                             
093300     END-IF                                                               
093400                                                                          
093500     IF REQU-UPDATE                                                       
093600       MOVE W-SPACE   TO RESP-KVINLCAR                                    
093700                         RESP-KVRADER-HIT                                 
093800                         RESP-KVRADER-TOT                                 
093900                         RESP-KVRADER-PRIO                                
094000     ELSE                                                                 
094100       PERFORM FC-FLYTTA-VAGN-TAB                                         
094200       IF REQU-QUERY                                                      
094300           CONTINUE                                                       
094400        ELSE                                                              
094500           PERFORM FD-REDIGERA-SUMMARAD                                   
094600       END-IF                                                             
094700     END-IF                                                               
094800     .                                                                    
094900     EJECT                                                                
095000 FA-FYLL-I-RESP-RAD   SECTION.                                            
095100                                                                          
095200     IF RAD-IDINLVGN                  = ZERO                OR            
095300                                        REQU-UPDATE         OR            
095400                                        REQU-QUERY          OR            
095500       (REQU-KVINLCAR                 = ALL '+' AND REQU-NEXT)            
095600         CONTINUE                                                         
095700      ELSE                                                                
095800         PERFORM FAA-RAEKNA-ANT-VAGN                                      
095900     END-IF                                                               
096000                                                                          
096100     IF RAD-KDINLPRIO                 <  30                               
096200         COMPUTE W-KVRADER-PRIO       =  W-KVRADER-PRIO + 1               
096300     END-IF                                                               
096400                                                                          
096500     COMPUTE W-KVRADER-TOT            =  W-KVRADER-TOT + 1                
096600     COMPUTE W-KVRADER-HIT            =  W-KVRADER-HIT + 1                
096700                                                                          
096800     MOVE ART-IDARTNR                 TO RESP-IDARTNR-LINE (INDX)         
096900     MOVE RAD-FLDIVKLI                TO RESP-FLDIVKLI-LINE (INDX)        
097000     MOVE RAD-IDINLVGN                TO RESP-IDINLVGN-LINE (INDX)        
097100*    MOVE ART-BEART                   TO RESP-BEART-LINE   (INDX)         
097200*      -- FETCH A BETTER FLAVOR OF DESCRIPTION                            
097300     MOVE ART-IDARTNR TO W-IDARTNR                                        
097400     PERFORM FAC-GET-BEART                                                
097500     MOVE ART-IDLOPNRM                TO RESP-IDLOPNRM-LINE (INDX)        
097600     MOVE RAD-IDRADNR                 TO RESP-IDRADNR-LINE (INDX)         
097700     IF RAD-IDOKOLLI                  >  ZERO                             
097800         MOVE RAD-IDLEVNR-KOLLI       TO RESP-IDLEVNR-LINE (INDX)         
097900         MOVE RAD-IDOKOLLI            TO RESP-IDOKOLLI-LINE (INDX)        
098000      ELSE                                                                
098100         MOVE W-SPAR-IDLEVNR          TO RESP-IDLEVNR-LINE (INDX)         
098200         MOVE ZERO                    TO RESP-IDOKOLLI-LINE (INDX)        
098300     END-IF                                                               
098400                                                                          
098500     IF (W-KDINLOMR                   = 'FB' OR                           
098600        W-KDINLOMR-NXT               = 'FB') OR                           
098700        (W-KDINLOMR     = 'RTA' AND W-KDINLOMR-RTA-PAR = 'FB ') OR        
098800        (W-KDINLOMR-NXT = 'RTA' AND W-KDINLOMR-RTA-PAR = 'FB ')           
098900                                                                          
099000       MOVE RAD-FLINLFB             TO RESP-FLINLFB-LINE(INDX)            
099100       IF RAD-FLINLFB = JA                                                
099200          MOVE YES                  TO RESP-FLINLFB-LINE(INDX)            
099300       END-IF                                                             
099400     ELSE                                                                 
099500       IF (W-KDINLOMR                   = 'F' OR                          
099600         W-KDINLOMR-NXT               = 'F') OR                           
099700        (W-KDINLOMR     = 'RTA' AND W-KDINLOMR-RTA-PAR = 'F  ') OR        
099800        (W-KDINLOMR-NXT = 'RTA' AND W-KDINLOMR-RTA-PAR = 'F  ')           
099900         MOVE RAD-FLINLFP            TO RESP-FLINLFB-LINE(INDX)           
100000         IF RAD-FLINLFP = JA                                              
100100            MOVE YES                  TO RESP-FLINLFB-LINE(INDX)          
100200         END-IF                                                           
100300       ELSE                                                               
100400         IF (W-KDINLOMR                   = 'FBP' OR                      
100500            W-KDINLOMR-NXT               = 'FBP') OR                      
100600           (W-KDINLOMR = 'RTA' AND W-KDINLOMR-RTA-PAR = 'FBP') OR         
100700           (W-KDINLOMR-NXT = 'RTA' AND W-KDINLOMR-RTA-PAR = 'FBP')        
100800           IF RAD-FLINLFB = NEJ                                           
100900             MOVE RAD-FLINLFP       TO RESP-FLINLFB-LINE(INDX)            
101000             IF RAD-FLINLFP = JA                                          
101100                MOVE YES            TO RESP-FLINLFB-LINE(INDX)            
101200             END-IF                                                       
101300           ELSE                                                           
101400             MOVE RAD-FLINLFB       TO RESP-FLINLFB-LINE(INDX)            
101500             IF RAD-FLINLFB = JA                                          
101600                MOVE YES             TO RESP-FLINLFB-LINE(INDX)           
101700             END-IF                                                       
101800           END-IF                                                         
101900                                                                          
102000         ELSE                                                             
102100           MOVE SPACE           TO RESP-FLINLFB-LINE(INDX)                
102200         END-IF                                                           
102300       END-IF                                                             
102400     END-IF                                                               
102500                                                                          
102600     MOVE NEJ         TO FL-KVROS                                         
102700     IF DCS-CDC                                                           
102800       PERFORM IMS-GU-WDK611                                              
102900       IF SEGMENT-FINNS                                                   
103000         IF CLAG-KVROS > ZERO                                             
103100           MOVE JA TO FL-KVROS                                            
103200         END-IF                                                           
103300       END-IF                                                             
103400     ELSE                                                                 
103500       PERFORM IMS-GU-WDK711                                              
103600       IF SEGMENT-FINNS                                                   
103700         IF SLAG-KVROS-DAG > ZERO OR SLAG-KVROS-BULK > ZERO               
103800           MOVE JA TO FL-KVROS                                            
103900         END-IF                                                           
104000       END-IF                                                             
104100     END-IF                                                               
104200     IF FL-KVROS =  JA                                                    
104300       MOVE YES              TO RESP-FLKVROS-LINE(INDX)                   
104400     ELSE                                                                 
104500        MOVE SPACE           TO RESP-FLKVROS-LINE(INDX)                   
104600     END-IF                                                               
104700                                                                          
104800     IF (W-KDINLOMR                   = 'F   ' OR 'FBP ') OR              
104900        (W-KDINLOMR-NXT               = 'F   ' OR 'FBP ') OR              
105000        (W-KDINLOMR-RTA-PAR           = 'F   ' OR 'FBP ')                 
105100                                                                          
105200         IF WS-ADINLOMR               =  SPACE                            
105300             MOVE SPACE               TO RESP-FLPREPFF-LINE (INDX)        
105400          ELSE                                                            
105500             PERFORM FAB-BEHANDLA-FLPREPFF                                
105600         END-IF                                                           
105700      ELSE                                                                
105800         MOVE SPACE                   TO RESP-FLPREPFF-LINE (INDX)        
105900     END-IF                                                               
106000     .                                                                    
106100     EJECT                                                                
106200 FAA-RAEKNA-ANT-VAGN  SECTION.                                            
106300                                                                          
106400     MOVE +1                   TO VAGN-IX                                 
106500     PERFORM UNTIL VAGN-IX     > MAX-VAGN-IX             OR               
106600               REQU-IDINLVGN-SPAR(VAGN-IX) = RAD-IDINLVGN OR              
106700              (REQU-IDINLVGN-SPAR(VAGN-IX) = ZERO OR                      
106800               REQU-IDINLVGN-SPAR(VAGN-IX) = SPACE)                       
106900         ADD +1                TO VAGN-IX                                 
107000     END-PERFORM                                                          
107100                                                                          
107200     IF VAGN-IX                >  MAX-VAGN-IX                             
107300         MOVE INF-MORE-INFO-EXISTS TO RESP-IDMSG-INFO                     
107400         MOVE 'FLER ÄN 50VAGNAR'   TO RESP-IDELMT-ERROR                   
107500      ELSE                                                                
107600         IF REQU-IDINLVGN-SPAR(VAGN-IX) = ZERO                            
107700             ADD +1                    TO W-KVINLCAR                      
107800             MOVE RAD-IDINLVGN         TO                                 
107900                                  REQU-IDINLVGN-SPAR(VAGN-IX)             
108000         END-IF                                                           
108100     END-IF                                                               
108200     .                                                                    
108300     EJECT                                                                
108400 FAB-BEHANDLA-FLPREPFF    SECTION.                                        
108500                                                                          
108600     MOVE JA                   TO FLPREPFF-SW                             
108700     PERFORM IMS-GNP-INLA1-INLA21-FIRST                                   
108800     PERFORM UNTIL SEGMENT-SAKNAS OR FLPREPFF-VISAS-EJ                    
108900         IF RAD-ADINLOMR       = WS-ADINLOMR AND                          
109000            (RAD-KDINLSTA      = SPACE OR 'SAK')                          
109100              PERFORM IMS-GNP-INLA1-INLA21                                
109200          ELSE                                                            
109300              MOVE NEJ         TO FLPREPFF-SW                             
109400         END-IF                                                           
109500     END-PERFORM                                                          
109600                                                                          
109700     IF FLPREPFF-VISAS                                                    
109800       IF DCS-CDC                                                         
109900         MOVE JA               TO RESP-FLPREPFF-LINE (INDX)               
110000       ELSE                                                               
110100         MOVE YES              TO RESP-FLPREPFF-LINE (INDX)               
110200       END-IF                                                             
110300      ELSE                                                                
110400         MOVE SPACE            TO RESP-FLPREPFF-LINE (INDX)               
110500     END-IF                                                               
110600     .                                                                    
110700     EJECT                                                                
110800 FAC-GET-BEART         SECTION.                                           
110900*    -- SELECT LANGUAGE TO FETCH AND CORRESPONDING CODE-PAGE              
           MOVE DCS-IDSKYLT-DB        TO W-IDSKYLT                              
           IF DCS-UNICODE-IDSKYLT                                               
              MOVE 'UTF8'             TO TRAUTF8-KDCP                           
           ELSE                                                                 
              MOVE '278 '             TO TRAUTF8-KDCP                           
           END-IF                                                               
112300                                                                          
112400     PERFORM IMS-GU-WDD311                                                
112500     IF SEGMENT-FINNS                                                     
112600       MOVE TEXT-BEART    TO TRAUTF8-TECONV-FROM                          
112700     ELSE                                                                 
112800       MOVE SPACE         TO TRAUTF8-TECONV-FROM                          
112900                             TEXT-BEART                                   
113000       MOVE WS-CP-EBCDIC  TO TRAUTF8-KDCP                                 
113100     END-IF                                                               
           IF TRAUTF8-TECONV-FROM = SPACES                                      
            MOVE 'GB'  TO W-IDSKYLT                                             
            MOVE '278' TO TRAUTF8-KDCP                                          
            PERFORM IMS-GU-WDD311                                               
            MOVE TEXT-BEART    TO TRAUTF8-TECONV-FROM                           
           END-IF                                                               
113200                                                                          
113300     IF REQU-IDMSGVER = '001'                                             
113400*      CALL FROM WEB AND NDC CHINA                                        
113500*      CONVERT TO UNICODE IF NOT ALREADY SO, STRIP TRAILING SPACE         
113600       CALL WTRAUTF8 USING TRAUTF8-AREA                                   
113700       MOVE TRAUTF8-TECONV-TO TO RESP-BEART-LINE (INDX)                   
113800     ELSE                                                                 
113900*      CALL FROM 3270 SCREEN. RETURN AS-IS (EBCDIC)                       
114000       MOVE TEXT-BEART        TO RESP-BEART-LINE (INDX)                   
114100     END-IF                                                               
114200     .                                                                    
114300     EJECT                                                                
114400 FB-RAEKNA-ALLA-RADER SECTION.                                            
114500                                                                          
114600     PERFORM UNTIL (INLF-SEGMENT-SAKNAS AND INLH-SEGMENT-SAKNAS)          
114700         IF W-SPAR-KDINLPRIO      < 30                                    
114800             COMPUTE W-KVRADER-PRIO = W-KVRADER-PRIO + 1                  
114900         END-IF                                                           
115000         COMPUTE W-KVRADER-TOT = W-KVRADER-TOT + 1                        
115100         PERFORM S01-LAES-RADDATA                                         
115200     END-PERFORM                                                          
115300     .                                                                    
115400     EJECT                                                                
115500 FC-FLYTTA-VAGN-TAB      SECTION.                                         
115600                                                                          
115700     MOVE +1                   TO VAGN-IX                                 
115800     PERFORM UNTIL VAGN-IX     > MAX-VAGN-IX                              
115900         MOVE REQU-IDINLVGN-SPAR(VAGN-IX)                                 
116000                               TO RESP-IDINLVGN-SPAR(VAGN-IX)             
116100         ADD +1                TO VAGN-IX                                 
116200     END-PERFORM                                                          
116300     .                                                                    
116400     EJECT                                                                
116500 FD-REDIGERA-SUMMARAD    SECTION.                                         
116600                                                                          
116700     IF REQU-FIRST                                                        
116800         MOVE W-KVINLCAR          TO RESP-KVINLCAR                        
116900         MOVE W-KVRADER-HIT       TO RESP-KVRADER-HIT                     
117000         MOVE W-KVRADER-TOT       TO RESP-KVRADER-TOT                     
117100         MOVE W-KVRADER-PRIO      TO RESP-KVRADER-PRIO                    
117200      ELSE                                                                
117300         IF REQU-KVINLCAR         =  ALL '+'                              
117400             MOVE W-SPACE         TO RESP-KVINLCAR                        
117500                                     RESP-KVRADER-HIT                     
117600                                     RESP-KVRADER-TOT                     
117700                                     RESP-KVRADER-PRIO                    
117800          ELSE                                                            
117900             INSPECT REQU-KVINLCAR REPLACING LEADING SPACE BY ZERO        
118000             INSPECT REQU-KVRADER-HIT                                     
118100                     REPLACING LEADING SPACE BY ZERO                      
118200             INSPECT REQU-KVRADER-TOT REPLACING                           
118300                                       LEADING SPACE BY ZERO              
118400             INSPECT REQU-KVRADER-PRIO REPLACING                          
118500                                       LEADING SPACE BY ZERO              
118600             MOVE REQU-KVINLCAR   TO W-REQU-KVINLCAR                      
118700             MOVE REQU-KVRADER-HIT TO W-REQU-KVRADER-HIT                  
118800             MOVE REQU-KVRADER-TOT TO W-REQU-KVRADER-TOT                  
118900             MOVE REQU-KVRADER-PRIO TO W-REQU-KVRADER-PRIO                
119000             COMPUTE RESP-KVINLCAR = W-KVINLCAR + W-REQU-KVINLCAR         
119100             COMPUTE RESP-KVRADER-HIT =                                   
119200                     W-KVRADER-HIT + W-REQU-KVRADER-HIT                   
119300             IF TORG-LAES                                                 
119400               IF REQU-NEXT                                               
119500                 COMPUTE RESP-KVRADER-TOT = W-KVRADER-TOT  +              
119600                                            W-REQU-KVRADER-TOT            
119700                 COMPUTE RESP-KVRADER-PRIO = W-KVRADER-PRIO +             
119800                                            W-REQU-KVRADER-PRIO           
119900               END-IF                                                     
120000             ELSE                                                         
120100               IF REQU-NEXT                                               
120200                  CONTINUE                                                
120300                ELSE                                                      
120400                   MOVE W-KVRADER-PRIO    TO RESP-KVRADER-TOT             
120500                   MOVE W-KVRADER-PRIO    TO RESP-KVRADER-PRIO            
120600               END-IF                                                     
120700             END-IF                                                       
120800         END-IF                                                           
120900     END-IF                                                               
121000     .                                                                    
121100     EJECT                                                                
121200 G-KOLLA-INPUT SECTION.                                                   
121300                                                                          
121400     MOVE NEJ                    TO INPUT-SW                              
121500     MOVE +1                     TO INDX                                  
121600     PERFORM UNTIL INDX          >  REQU-KVRADER                          
121700                                 OR INPUT-FINNS                           
121800       IF REQU-KDCMDVAL-LINE(INDX) = ALL '+'                              
121900         CONTINUE                                                         
122000       ELSE                                                               
122100         MOVE JA           TO INPUT-SW                                    
122200       END-IF                                                             
122300       ADD +1              TO INDX                                        
122400     END-PERFORM                                                          
122500                                                                          
122600     IF INPUT-NIX                                                         
122700       MOVE ERR-PF11-AND-NO-DATA TO RESP-IDMSG-ERROR                      
122800       MOVE +1                          TO VAGN-IX                        
122900       PERFORM UNTIL VAGN-IX            >  MAX-VAGN-IX                    
123000           MOVE REQU-IDINLVGN-SPAR(VAGN-IX) TO                            
123100                                  RESP-IDINLVGN-SPAR(VAGN-IX)             
123200           ADD +1                       TO VAGN-IX                        
123300       END-PERFORM                                                        
123400       MOVE NEJ                  TO INDATA-SW                             
123500     ELSE                                                                 
123600       PERFORM GA-KOLLA-ADINLOMR                                          
123700       IF INDATA-OK                                                       
123800         MOVE +1               TO INDX                                    
123900         PERFORM UNTIL INDX    >  REQU-KVRADER                            
124000           IF REQU-KDCMDVAL-LINE(INDX) = ALL '+' OR SPACE                 
124100             CONTINUE                                                     
124200           ELSE                                                           
124300             PERFORM GB-KOLLA-KDCMDVAL                                    
124400             EVALUATE TRUE                                                
124500               WHEN REQU-KDCMDVAL-LINE(INDX) = 'ÄPL' OR 'LOC'             
124600                 PERFORM GC-KOLLA-AEPL                                    
124700                                                                          
124800               WHEN REQU-KDCMDVAL-LINE(INDX) = 'VP' OR 'SB'               
124900                 PERFORM GD-KOLLA-VP                                      
125000                                                                          
125100               WHEN REQU-KDCMDVAL-LINE(INDX) = 'VK' OR 'SC'               
125200                 PERFORM GE-KOLLA-VK                                      
125300                                                                          
125400               WHEN REQU-KDCMDVAL-LINE(INDX) = 'R' OR 'RET'               
125500                 PERFORM GF-KOLLA-RET                                     
125600                                                                          
125700               WHEN REQU-KDCMDVAL-LINE(INDX) = 'FPP' OR 'PB' OR           
125800                                               'SPL'                      
125900                 PERFORM GG-KOLLA-FPP                                     
126000                                                                          
126100               WHEN REQU-KDCMDVAL-LINE(INDX) = 'FPK' OR 'PC'              
126200                                                     OR 'SPC'             
126300                 PERFORM GH-KOLLA-FPK                                     
126400                                                                          
126500               WHEN REQU-KDCMDVAL-LINE(INDX) = 'E' OR 'EB'                
126600                 PERFORM S04-KOLLA-KDINLOMR                               
126700             END-EVALUATE                                                 
126800           END-IF                                                         
126900           ADD +1          TO INDX                                        
127000         END-PERFORM                                                      
127100       END-IF                                                             
127200                                                                          
127300       IF INDATA-FEL                                                      
127400         MOVE +1                          TO VAGN-IX                      
127500         PERFORM UNTIL VAGN-IX            >  MAX-VAGN-IX                  
127600           MOVE REQU-IDINLVGN-SPAR(VAGN-IX) TO                            
127700                                  RESP-IDINLVGN-SPAR(VAGN-IX)             
127800           ADD +1                       TO VAGN-IX                        
127900         END-PERFORM                                                      
128000                                                                          
128100       END-IF                                                             
128200     END-IF                                                               
128300     .                                                                    
128400     EJECT                                                                
128500 GA-KOLLA-ADINLOMR  SECTION.                                              
128600                                                                          
128700     IF WS-ADINLOMR              =  SPACE AND                             
128800        WS-ADINLOMR-NXT          =  SPACE                                 
128900        MOVE MFS-ALFA-FAELT-FEL  TO RESP-KDCMDVAL-ATTR(INDX)              
129000        MOVE NEJ                 TO INDATA-SW                             
129100        MOVE ERR-UPD-NOT-ALLOWED TO RESP-IDMSG-ERROR                      
129200      ELSE                                                                
129300         IF WS-ADINLOMR = SPACE                                           
129400           MOVE WS-ADINLOMR-NXT  TO W-6006-ADINLOMR                       
129500         ELSE                                                             
129600           MOVE WS-ADINLOMR      TO W-6006-ADINLOMR                       
129700         END-IF                                                           
129800         PERFORM IMS-GU-PLAA-PLAA11                                       
129900         IF SEGMENT-FINNS                                                 
130000             CONTINUE                                                     
130100          ELSE                                                            
130200           MOVE MFS-ALFA-FAELT-FEL TO RESP-KDCMDVAL-ATTR(INDX)            
130300           MOVE NEJ                TO INDATA-SW                           
130400          MOVE ERR-UPD-NOT-ALLOWED TO RESP-IDMSG-ERROR                    
130500         END-IF                                                           
130600     END-IF                                                               
130700     .                                                                    
130800     EJECT                                                                
130900 GB-KOLLA-KDCMDVAL  SECTION.                                              
131000                                                                          
131100     IF REQU-KDCMDVAL-LINE(INDX)   = 'ÄPL' OR 'VP' OR 'VK' OR             
131200                                     'R' OR 'RET' OR 'FPP' OR             
131300                                     'FPK' OR 'E' OR 'EB' OR              
131400                                     'LOC' OR 'SB' OR 'SC' OR             
131500                                     'PB' OR 'PC' OR                      
131600                                     'SPL' OR 'SPC'                       
131700         MOVE MFS-ALFA-FAELT-RAETT TO RESP-KDCMDVAL-ATTR(INDX)            
131800      ELSE                                                                
131900         MOVE ERR-WRONG-VALUE      TO RESP-IDMSG-ERROR                    
132000         MOVE 'CMD'                TO RESP-IDELMT-ERROR                   
132100         MOVE MFS-ALFA-FAELT-FEL   TO RESP-KDCMDVAL-ATTR(INDX)            
132200         MOVE NEJ                  TO INDATA-SW                           
132300     END-IF                                                               
132400                                                                          
132500     IF KDCMDVAL-IFYLLD                                                   
132600         IF REQU-KDCMDVAL-LINE(INDX) = ('RET' OR 'R' OR 'E' OR            
132700                                      'EB'  OR 'VP' OR 'VK' OR            
132800                                      'FPK' OR 'ÄPL' OR                   
132900                                      'LOC' OR 'SB' OR 'SC' OR            
133000                                      'PB ' OR 'PC' OR                    
133100                                      'SPL' OR 'SPC') AND                 
133200            REQU-KDCMDVAL-LINE(INDX) = W-SPAR-KDCMDVAL-LINE               
133300             CONTINUE                                                     
133400          ELSE                                                            
133500            MOVE ERR-WRONG-VALUE    TO RESP-IDMSG-ERROR                   
133600            MOVE 'CMD'              TO RESP-IDELMT-ERROR                  
133700            MOVE MFS-ALFA-FAELT-FEL TO RESP-KDCMDVAL-ATTR(INDX)           
133800            MOVE NEJ                TO INDATA-SW                          
133900         END-IF                                                           
134000      ELSE                                                                
134100         MOVE JA                     TO KDCMDVAL-SW                       
134200         MOVE REQU-KDCMDVAL-LINE(INDX) TO W-SPAR-KDCMDVAL-LINE            
134300     END-IF                                                               
134400     .                                                                    
134500     EJECT                                                                
134600 GC-KOLLA-AEPL     SECTION.                                               
134700                                                                          
134800     IF WS-ADINLOMR-NXT          = SPACE                                  
134900         MOVE MFS-ALFA-FAELT-FEL TO RESP-KDCMDVAL-ATTR(INDX)              
135000         MOVE NEJ                TO INDATA-SW                             
135100        MOVE ERR-UPD-NOT-ALLOWED TO RESP-IDMSG-ERROR                      
135200      ELSE                                                                
135300         MOVE REQU-IDLOPNRM-LINE(INDX) TO W-D1BSEQ-IDLOPNRM               
135400         MOVE REQU-IDRADNR-LINE(INDX) TO W-IDRADNR                        
135500         PERFORM IMS-GU-INLA2-INLA21                                      
135600         IF SEGMENT-SAKNAS OR RAD-IDINLVGN NOT = ZERO                     
135700           MOVE MFS-ALFA-FAELT-FEL TO RESP-KDCMDVAL-ATTR(INDX)            
135800           MOVE NEJ                  TO INDATA-SW                         
135900           MOVE ERR-UPD-NOT-ALLOWED  TO RESP-IDMSG-ERROR                  
136000         END-IF                                                           
136100     END-IF                                                               
136200     .                                                                    
136300     EJECT                                                                
136400 GD-KOLLA-VP       SECTION.                                               
136500                                                                          
136600     PERFORM S03-KOLLA-KDINLOMR                                           
136700     IF KDINLOMR-OK                                                       
136800         PERFORM S05-KOLLA-KOLLIN                                         
136900     END-IF                                                               
137000     .                                                                    
137100     EJECT                                                                
137200 GE-KOLLA-VK       SECTION.                                               
137300                                                                          
137400     PERFORM S03-KOLLA-KDINLOMR                                           
137500     IF KDINLOMR-OK                                                       
137600         PERFORM S06-KOLLA-KOLLI                                          
137700     END-IF                                                               
137800     .                                                                    
137900     EJECT                                                                
138000 GF-KOLLA-RET      SECTION.                                               
138100                                                                          
138200     PERFORM S03-KOLLA-KDINLOMR                                           
138300     .                                                                    
138400     EJECT                                                                
138500 GG-KOLLA-FPP      SECTION.                                               
138600                                                                          
138700     PERFORM S04-KOLLA-KDINLOMR                                           
138800     IF KDINLOMR-OK                                                       
138900         PERFORM S05-KOLLA-KOLLIN                                         
139000     END-IF                                                               
139100     .                                                                    
139200     EJECT                                                                
139300 GH-KOLLA-FPK      SECTION.                                               
139400                                                                          
139500     PERFORM S04-KOLLA-KDINLOMR                                           
139600     IF KDINLOMR-OK                                                       
139700         PERFORM S06-KOLLA-KOLLI                                          
139800     END-IF                                                               
139900     .                                                                    
140000     EJECT                                                                
140100 H-UPPDATERA SECTION.                                                     
140200                                                                          
140300     MOVE +1                     TO INDX                                  
140400                                    6197-IX                               
140500     PERFORM UNTIL INDX          >  REQU-KVRADER                          
140600         IF REQU-KDCMDVAL-LINE(INDX) = ALL '+' OR SPACE                   
140700             CONTINUE                                                     
140800          ELSE                                                            
140900             PERFORM HA-LAES-INLC                                         
141000             MOVE REQU-IDRADNR-LINE (INDX) TO W-D1E1KY-IDRADNR            
141100                                              W-D1G1KY-IDRADNR            
141200             EVALUATE TRUE                                                
141300               WHEN REQU-KDCMDVAL-LINE(INDX) = 'ÄPL' OR 'LOC'             
141400                 PERFORM HB-UPPDATERA-AEPL                                
141500                                                                          
141600               WHEN REQU-KDCMDVAL-LINE(INDX) = 'VP' OR 'SB'               
141700                 PERFORM HC-UPPDATERA-VP                                  
141800                 PERFORM S17-FIXA-HOPP-MID                                
141900                                                                          
142000               WHEN REQU-KDCMDVAL-LINE(INDX) = 'VK' OR 'SC'               
142100                 PERFORM HD-UPPDATERA-VK                                  
142200                 PERFORM S17-FIXA-HOPP-MID                                
142300                                                                          
142400               WHEN REQU-KDCMDVAL-LINE(INDX) = 'R' OR 'RET'               
142500                 PERFORM HE-UPPDATERA-RET                                 
142600                                                                          
142700               WHEN REQU-KDCMDVAL-LINE(INDX) = 'FPP' OR 'PB'              
142800                                                     OR 'SPL'             
142900                 PERFORM HF-UPPDATERA-FPP                                 
143000                 PERFORM S17-FIXA-HOPP-MID                                
143100                                                                          
143200               WHEN REQU-KDCMDVAL-LINE(INDX) = 'FPK' OR 'PC'              
143300                                                     OR 'SPC'             
143400                 PERFORM HG-UPPDATERA-FPK                                 
143500                 PERFORM S17-FIXA-HOPP-MID                                
143600                                                                          
143700               WHEN REQU-KDCMDVAL-LINE(INDX) = 'E'                        
143800                 PERFORM HH-UPPDATERA-E                                   
143900                                                                          
144000               WHEN REQU-KDCMDVAL-LINE(INDX) = 'EB'                       
144100                 PERFORM HI-UPPDATERA-EB                                  
144200             END-EVALUATE                                                 
144300         END-IF                                                           
144400         ADD +1                TO INDX                                    
144500     END-PERFORM                                                          
144600     IF 6191-IX                > 1                                        
144700         PERFORM S08-STARTA-W6T191                                        
144800     END-IF                                                               
144900     IF 6197-IX                > 1                                        
145000         PERFORM S15-STARTA-6197-TRANS                                    
145100     END-IF                                                               
145200                                                                          
145300     MOVE INF-UPDATE-DONE      TO RESP-IDMSG-INFO                         
145400     PERFORM MFS-FORM-ATTR                                                
145500     PERFORM MFS-RENSA-FAELT-IN                                           
145600     .                                                                    
145700     EJECT                                                                
145800 HA-LAES-INLC        SECTION.                                             
145900                                                                          
146000     MOVE REQU-IDLOPNRM-LINE(INDX) TO W-IDLOPNRM                          
146100     PERFORM IMS-GU-INLC-INLC01                                           
146200                                                                          
146300     MOVE SEQB-IDDC            TO W-D101KY-IDDC                           
146400                                  W-D1E1KY-IDDC                           
146500                                  W-D1E1KY-IDDC                           
146600                                  W-D1G1KY-IDDC                           
146700                                  W-D1G1KY-IDDC                           
146800     MOVE SEQB-IDLEVNR         TO W-D101KY-IDLEVNR                        
146900                                  W-D1E1KY-IDLEVNR                        
147000                                  W-D1E1KY-IDLEVNR-MIN                    
147100                                  W-D1G1KY-IDLEVNR                        
147200                                  W-D1G1KY-IDLEVNR-MIN                    
147300     MOVE SEQB-IDFS            TO W-D101KY-IDFS                           
147400                                  W-D1E1KY-IDFS                           
147500                                  W-D1E1KY-IDFS-MIN                       
147600                                  W-D1G1KY-IDFS                           
147700                                  W-D1G1KY-IDFS-MIN                       
147800     MOVE SEQB-TIAVIDAT        TO W-D101KY-TIAVIDAT                       
147900                                  W-D1E1KY-TIAVIDAT                       
148000                                  W-D1E1KY-TIAVIDAT-MIN                   
148100                                  W-D1G1KY-TIAVIDAT                       
148200                                  W-D1G1KY-TIAVIDAT-MIN                   
148300     MOVE SEQB-IDRADNR-INL     TO W-IDRADNR-INL                           
148400                                  W-D1E1KY-IDRADNR-INL                    
148500                                  W-D1E1KY-IDRADNR-INL-MIN                
148600                                  W-D1G1KY-IDRADNR-INL                    
148700                                  W-D1G1KY-IDRADNR-INL-MIN                
148800     .                                                                    
148900     EJECT                                                                
149000 HB-UPPDATERA-AEPL   SECTION.                                             
149100                                                                          
149200     PERFORM IMS-GU-INLA1-INLA11                                          
149300                                                                          
149400     MOVE REQU-IDRADNR-LINE(INDX) TO W-IDRADNR                            
149500     PERFORM IMS-GHNP-INLA1-INLA21-KVAL                                   
149600     MOVE RAD-KDINLPRIO          TO W-D1E1KY-KDINLPRIO                    
149700                                    W-D1E1KY-KDINLPRIO-MIN                
149800                                    W-D1G1KY-KDINLPRIO                    
149900                                    W-D1G1KY-KDINLPRIO-MIN                
150000     IF RAD-IDOKOLLI > ZERO                                               
150100       MOVE RAD-IDOKOLLI TO W-D1CSEQ-IDOKOLLI                             
150200                            W-IDOKOLLI                                    
150300       MOVE RAD-IDLEVNR-KOLLI TO W-D1CSEQ-IDLEVNR                         
150400                                 W-IDLEVNRK                               
150500       PERFORM IMS-GU-INLA3-INLA11                                        
150600                                                                          
150700       PERFORM UNTIL SEGMENT-SAKNAS                                       
150800         PERFORM IMS-GHNP-INLA3-INLA21                                    
150900         PERFORM UNTIL SEGMENT-SAKNAS                                     
151000           MOVE RAD-ADINLOMR           TO W-ADINLOMR-OLD                  
151100           MOVE RAD-ADINLOMR-NXT       TO W-ADINLOMR-NXT-OLD              
151200           MOVE RAD-KDINLSTA           TO W-KDINLSTA-OLD                  
151300                                                                          
151400           MOVE WS-ADINLOMR-NXT        TO RAD-ADINLOMR                    
151500           MOVE SPACE                  TO RAD-ADINLOMR-NXT                
151600           IF RAD-KDINLSTA             =  'SAK'                           
151700               MOVE SPACE              TO RAD-KDINLSTA                    
151800           END-IF                                                         
151900           PERFORM IMS-REPL-INLA3-INLA21                                  
152000           PERFORM S07-SKAPA-R6191-MID                                    
152100           PERFORM IMS-GHNP-INLA3-INLA21                                  
152200         END-PERFORM                                                      
152300         PERFORM IMS-GN-INLA3-INLA11                                      
152400       END-PERFORM                                                        
152500     ELSE                                                                 
152600       MOVE RAD-ADINLOMR           TO W-ADINLOMR-OLD                      
152700       MOVE RAD-ADINLOMR-NXT       TO W-ADINLOMR-NXT-OLD                  
152800       MOVE RAD-KDINLSTA           TO W-KDINLSTA-OLD                      
152900                                                                          
153000       MOVE WS-ADINLOMR-NXT        TO RAD-ADINLOMR                        
153100       MOVE SPACE                  TO RAD-ADINLOMR-NXT                    
153200       IF RAD-KDINLSTA             =  'SAK'                               
153300           MOVE SPACE              TO RAD-KDINLSTA                        
153400       END-IF                                                             
153500       PERFORM IMS-REPL-INLA1-INLA21                                      
153600       PERFORM S07-SKAPA-R6191-MID                                        
153700     END-IF                                                               
153800     .                                                                    
153900     EJECT                                                                
154000 HC-UPPDATERA-VP  SECTION.                                                
154100                                                                          
154200     PERFORM IMS-GHU-INLA1-INLA21-OKVAL                                   
154300     MOVE RAD-KDINLPRIO          TO W-D1E1KY-KDINLPRIO                    
154400                                    W-D1E1KY-KDINLPRIO-MIN                
154500                                    W-D1G1KY-KDINLPRIO                    
154600                                    W-D1G1KY-KDINLPRIO-MIN                
154700     PERFORM UNTIL SEGMENT-SAKNAS                                         
154800         MOVE JA                 TO RAD-FLINLFB                           
154900         PERFORM IMS-REPL-INLA1-INLA21                                    
155000         PERFORM IMS-GHNP-INLA1-INLA21-OKVAL                              
155100     END-PERFORM                                                          
155200     .                                                                    
155300     EJECT                                                                
155400 HD-UPPDATERA-VK   SECTION.                                               
155500                                                                          
155600     MOVE REQU-IDRADNR-LINE(INDX) TO W-IDRADNR                            
155700     PERFORM IMS-GHU-INLA1-INLA21-KVAL                                    
155800     MOVE RAD-KDINLPRIO          TO W-D1E1KY-KDINLPRIO                    
155900                                    W-D1E1KY-KDINLPRIO-MIN                
156000                                    W-D1G1KY-KDINLPRIO                    
156100                                    W-D1G1KY-KDINLPRIO-MIN                
156200     IF RAD-KDINLSTA             =  'SAK'                                 
156300         MOVE RAD-ADINLOMR-NXT   TO W-ADINLOMR-NXT-OLD                    
156400         MOVE RAD-KDINLSTA       TO W-KDINLSTA-OLD                        
156500         MOVE SPACE              TO RAD-KDINLSTA                          
156600     END-IF                                                               
156700     MOVE JA                     TO RAD-FLINLFB                           
156800     PERFORM IMS-REPL-INLA1-INLA21                                        
156900     IF RAD-KDINLSTA             =  'SAK'                                 
157000         PERFORM IMS-GU-INLA1-INLA11                                      
157100         PERFORM S07-SKAPA-R6191-MID                                      
157200     END-IF                                                               
157300     .                                                                    
157400     EJECT                                                                
157500 HE-UPPDATERA-RET   SECTION.                                              
157600                                                                          
157700     MOVE REQU-IDRADNR-LINE(INDX) TO W-IDRADNR                            
157800     PERFORM IMS-GHU-INLA1-INLA21-KVAL                                    
157900     MOVE RAD-KDINLPRIO          TO W-D1E1KY-KDINLPRIO                    
158000                                    W-D1E1KY-KDINLPRIO-MIN                
158100                                    W-D1G1KY-KDINLPRIO                    
158200                                    W-D1G1KY-KDINLPRIO-MIN                
158300     IF RAD-KDINLSTA             =  'SAK'                                 
158400         MOVE RAD-ADINLOMR-NXT   TO W-ADINLOMR-NXT-OLD                    
158500         MOVE RAD-KDINLSTA       TO W-KDINLSTA-OLD                        
158600         MOVE SPACE              TO RAD-KDINLSTA                          
158700     END-IF                                                               
158800     MOVE NEJ                    TO RAD-FLINLFB                           
158900     PERFORM IMS-REPL-INLA1-INLA21                                        
159000     IF RAD-KDINLSTA             =  'SAK'                                 
159100         PERFORM IMS-GU-INLA1-INLA11                                      
159200         PERFORM S07-SKAPA-R6191-MID                                      
159300     END-IF                                                               
159400     .                                                                    
159500     EJECT                                                                
159600 HF-UPPDATERA-FPP      SECTION.                                           
159700                                                                          
159800     MOVE NEJ                    TO SPAR-RAD1-SW                          
159900                                    SKAPA-RAD1-SW                         
160000     MOVE +1                     TO 6191-IX                               
160100                                    6197-IX                               
160200                                                                          
160300     PERFORM IMS-GU-INLA1-INLA11                                          
160400     PERFORM IMS-GHNP-INLA1-INLA21-OKVAL                                  
160500     MOVE RAD-KDINLPRIO          TO W-D1E1KY-KDINLPRIO                    
160600                                    W-D1E1KY-KDINLPRIO-MIN                
160700                                    W-D1G1KY-KDINLPRIO                    
160800                                    W-D1G1KY-KDINLPRIO-MIN                
160900     IF RAD-IDRADNR              =  1                                     
161000         MOVE RAD-ADINLOMR       TO W-ADINLOMR-OLD                        
161100         MOVE RAD-ADINLOMR-NXT   TO W-ADINLOMR-NXT-OLD                    
161200         MOVE RAD-KDINLSTA       TO W-KDINLSTA-OLD                        
161300         MOVE RAD-KVINLART       TO W-KVINLART-OLD                        
161400         MOVE RAD-W6D121         TO SPAR-RAD-W6D121                       
161500         MOVE JA                 TO SPAR-RAD1-SW                          
161600         PERFORM IMS-GHNP-INLA1-INLA21-OKVAL                              
161700      ELSE                                                                
161800         MOVE SPACE              TO W-ADINLOMR-OLD                        
161900                                    W-ADINLOMR-NXT-OLD                    
162000                                    W-KDINLSTA-OLD                        
162100         MOVE ZERO               TO W-KVINLART-OLD                        
162200     END-IF                                                               
162300     PERFORM UNTIL SEGMENT-SAKNAS                                         
162400         IF RAD-KDINLSTA           = SPACE                                
162500             IF SPAR-RAD1-FINNS                                           
162600                 COMPUTE SPAR-RAD-KVINLART =                              
162700                         SPAR-RAD-KVINLART + RAD-KVINLART                 
162800              ELSE                                                        
162900                 PERFORM S11-SKAPA-SPAR-RAD1                              
163000                 MOVE JA       TO SPAR-RAD1-SW                            
163100                                  SKAPA-RAD1-SW                           
163200             END-IF                                                       
163300             PERFORM IMS-DLET-INLA1-INLA21                                
163400             PERFORM S12-SKAPA-R6191-MID-FLY-RAD                          
163500         END-IF                                                           
163600         PERFORM IMS-GHNP-INLA1-INLA21-OKVAL                              
163700     END-PERFORM                                                          
163800                                                                          
163900     IF SKAPA-RAD1                                                        
164000         MOVE SPAR-RAD-W6D121      TO RAD-W6D121                          
164100         MOVE JA                   TO RAD-FLPRIO                          
164200         PERFORM IMS-ISRT-INLA1-INLA21                                    
164300         PERFORM S09A-ANROPA-W611PMRK-PARTI                               
164400      ELSE                                                                
164500         PERFORM IMS-GHNP-INLA1-INLA21-FIRST                              
164600         MOVE SPAR-RAD-W6D121      TO RAD-W6D121                          
164700         MOVE WS-ADINLOMR          TO RAD-ADINLOMR                        
164800         MOVE JA                   TO RAD-FLINLFP                         
164900                                      RAD-FLPRIO                          
165000         PERFORM IMS-REPL-INLA1-INLA21                                    
165100         PERFORM S09-ANROPA-W611PMRK                                      
165200     END-IF                                                               
165300     PERFORM S13-SKAPA-R6191-MID-RAD1                                     
165400                                                                          
165500     PERFORM S16-KOLLA-OM-FBRAPPUTSKR                                     
165600                                                                          
165700     IF FBRAPP                                                            
165800       PERFORM S14-SKAPA-6197-TRANS                                       
165900     END-IF                                                               
166000     .                                                                    
166100     EJECT                                                                
166200 HG-UPPDATERA-FPK    SECTION.                                             
166300                                                                          
166400     MOVE NEJ                    TO SPAR-RAD1-SW                          
166500                                    SKAPA-RAD1-SW                         
166600     MOVE +1                     TO 6191-IX                               
166700                                                                          
166800     PERFORM IMS-GU-INLA1-INLA11                                          
166900     MOVE 1                      TO W-IDRADNR                             
167000     PERFORM IMS-GHNP-INLA1-INLA21-KVAL                                   
167100     IF SEGMENT-FINNS                                                     
167200         MOVE RAD-ADINLOMR       TO W-ADINLOMR-OLD                        
167300         MOVE RAD-ADINLOMR-NXT   TO W-ADINLOMR-NXT-OLD                    
167400         MOVE RAD-KDINLSTA       TO W-KDINLSTA-OLD                        
167500         MOVE RAD-KVINLART       TO W-KVINLART-OLD                        
167600         MOVE RAD-W6D121         TO SPAR-RAD-W6D121                       
167700         MOVE JA                 TO SPAR-RAD1-SW                          
167800      ELSE                                                                
167900         MOVE SPACE              TO W-ADINLOMR-OLD                        
168000                                    W-ADINLOMR-NXT-OLD                    
168100                                    W-KDINLSTA-OLD                        
168200         MOVE ZERO               TO W-KVINLART-OLD                        
168300     END-IF                                                               
168400     MOVE REQU-IDRADNR-LINE(INDX) TO W-IDRADNR                            
168500     PERFORM IMS-GHNP-INLA1-INLA21-KVAL                                   
168600     MOVE RAD-KDINLPRIO          TO W-D1E1KY-KDINLPRIO                    
168700                                    W-D1E1KY-KDINLPRIO-MIN                
168800                                    W-D1G1KY-KDINLPRIO                    
168900                                    W-D1G1KY-KDINLPRIO-MIN                
169000     IF SEGMENT-FINNS AND RAD-KDINLSTA           = SPACE                  
169100         IF SPAR-RAD1-FINNS                                               
169200             COMPUTE SPAR-RAD-KVINLART =                                  
169300                     SPAR-RAD-KVINLART + RAD-KVINLART                     
169400          ELSE                                                            
169500             PERFORM S11-SKAPA-SPAR-RAD1                                  
169600             MOVE JA           TO SPAR-RAD1-SW                            
169700                                  SKAPA-RAD1-SW                           
169800         END-IF                                                           
169900         PERFORM IMS-DLET-INLA1-INLA21                                    
170000         PERFORM S12-SKAPA-R6191-MID-FLY-RAD                              
170100     END-IF                                                               
170200                                                                          
170300     IF SKAPA-RAD1                                                        
170400         MOVE SPAR-RAD-W6D121      TO RAD-W6D121                          
170500         MOVE JA                   TO RAD-FLPRIO                          
170600         PERFORM IMS-ISRT-INLA1-INLA21                                    
170700         PERFORM S09A-ANROPA-W611PMRK-PARTI                               
170800      ELSE                                                                
170900         PERFORM IMS-GHNP-INLA1-INLA21-FIRST                              
171000         MOVE SPAR-RAD-W6D121      TO RAD-W6D121                          
171100         MOVE WS-ADINLOMR          TO RAD-ADINLOMR                        
171200         MOVE JA                   TO RAD-FLINLFP                         
171300                                      RAD-FLPRIO                          
171400         PERFORM IMS-REPL-INLA1-INLA21                                    
171500         PERFORM S09-ANROPA-W611PMRK                                      
171600     END-IF                                                               
171700     PERFORM S13-SKAPA-R6191-MID-RAD1                                     
171800                                                                          
171900     PERFORM S16-KOLLA-OM-FBRAPPUTSKR                                     
172000                                                                          
172100     IF FBRAPP                                                            
172200       PERFORM S14-SKAPA-6197-TRANS                                       
172300     END-IF                                                               
172400                                                                          
172500     .                                                                    
172600     EJECT                                                                
172700 HH-UPPDATERA-E      SECTION.                                             
172800                                                                          
172900     PERFORM IMS-GHU-INLA1-INLA11                                         
173000     MOVE JA                     TO ART-FLETIKETT                         
173100     PERFORM IMS-REPL-INLA1-INLA11                                        
173200                                                                          
173300     MOVE REQU-IDRADNR-LINE(INDX) TO W-IDRADNR                            
173400     PERFORM IMS-GHNP-INLA1-INLA21-KVAL                                   
173500     MOVE RAD-KDINLPRIO          TO W-D1E1KY-KDINLPRIO                    
173600                                    W-D1E1KY-KDINLPRIO-MIN                
173700                                    W-D1G1KY-KDINLPRIO                    
173800                                    W-D1G1KY-KDINLPRIO-MIN                
173900     IF RAD-KDINLSTA             =  'SAK'                                 
174000         MOVE RAD-ADINLOMR-NXT   TO W-ADINLOMR-NXT-OLD                    
174100         MOVE RAD-KDINLSTA       TO W-KDINLSTA-OLD                        
174200         MOVE SPACE              TO RAD-KDINLSTA                          
174300         PERFORM IMS-REPL-INLA1-INLA21                                    
174400         PERFORM S07-SKAPA-R6191-MID                                      
174500     END-IF                                                               
174600     .                                                                    
174700     EJECT                                                                
174800 HI-UPPDATERA-EB   SECTION.                                               
174900                                                                          
175000     PERFORM IMS-GHU-INLA1-INLA11                                         
175100     MOVE NEJ                    TO ART-FLETIKETT                         
175200     PERFORM IMS-REPL-INLA1-INLA11                                        
175300                                                                          
175400     MOVE REQU-IDRADNR-LINE(INDX) TO W-IDRADNR                            
175500     PERFORM IMS-GHNP-INLA1-INLA21-KVAL                                   
175600     MOVE RAD-KDINLPRIO          TO W-D1E1KY-KDINLPRIO                    
175700                                    W-D1E1KY-KDINLPRIO-MIN                
175800                                    W-D1G1KY-KDINLPRIO                    
175900                                    W-D1G1KY-KDINLPRIO-MIN                
176000     IF RAD-KDINLSTA             =  'SAK'                                 
176100         MOVE RAD-ADINLOMR-NXT   TO W-ADINLOMR-NXT-OLD                    
176200         MOVE RAD-KDINLSTA       TO W-KDINLSTA-OLD                        
176300         MOVE SPACE              TO RAD-KDINLSTA                          
176400         PERFORM IMS-REPL-INLA1-INLA21                                    
176500         PERFORM S07-SKAPA-R6191-MID                                      
176600     END-IF                                                               
176700     .                                                                    
176800     EJECT                                                                
176900 S01-LAES-RADDATA      SECTION.                                           
177000                                                                          
177100     EVALUATE TRUE                                                        
177200       WHEN W-NYCKEL-KOMB      = 1 OR 4                                   
177300           PERFORM S01A-LAES-PLAC-INDEX                                   
177400                                                                          
177500       WHEN W-NYCKEL-KOMB      = 2 OR 5                                   
177600           PERFORM S01B-LAES-ADR-INDEX                                    
177700                                                                          
177800       WHEN W-NYCKEL-KOMB      = 3 OR 6                                   
177900           PERFORM S01C-MATCHA-PLAC-ADR-INDEX                             
178000     END-EVALUATE                                                         
178100     .                                                                    
178200     EJECT                                                                
178300 S01A-LAES-PLAC-INDEX      SECTION.                                       
178400                                                                          
178500     MOVE NEJ                  TO KOLLI-TRAEFF-SW                         
178600     MOVE HIGH-VALUE           TO W-W6D1E1KY-MAX-X                        
178700     MOVE WS-ADINLOMR          TO W-D1E1KY-ADINLOMR-MIN                   
178800                                  W-D1E1KY-ADINLOMR-MAX                   
178900                                  W-D1E1KY-ADINLOMR                       
179000                                  W-ADINLOMR                              
179100     PERFORM S21-VAELJ-LAESNING-INLF                                      
179200                                                                          
179300     MOVE STATUS-WS            TO INLF-STATUS-WS                          
179400     PERFORM UNTIL INLF-SEGMENT-SAKNAS OR KOLLI-TRAEFF                    
179500         PERFORM S01AA-FLYTTA-INLF-FAELT                                  
179600         IF PRIO-LAESNING AND WS-BEFT-FOM = SPACE AND                     
179700           (SEQE-KDINLSTA = '   ' OR 'FPK' OR 'SPC') AND                  
179800           EJ-TORG                                                        
179900           MOVE JA TO KOLLI-TRAEFF-SW                                     
180000         ELSE                                                             
180100           PERFORM IMS-GU-INLA1-INLA11                                    
180200           PERFORM S02-KOLLA-KOLLI-TRAEFF                                 
180300           IF EJ-KOLLI-TRAEFF                                             
180400               PERFORM S21-VAELJ-LAESNING-INLF                            
180500               MOVE STATUS-WS    TO INLF-STATUS-WS                        
180600           END-IF                                                         
180700           IF KOLLI-TRAEFF AND TORG-LAES                                  
180800             PERFORM IMS-GU-INLA1-INLA11                                  
180900             PERFORM S02A-KOLLA-KOLLI-TRAEFF-TORG                         
181000             IF EJ-KOLLI-TRAEFF                                           
181100               PERFORM S21-VAELJ-LAESNING-INLF                            
181200               MOVE STATUS-WS    TO INLF-STATUS-WS                        
181300             END-IF                                                       
181400           END-IF                                                         
181500         END-IF                                                           
181600     END-PERFORM                                                          
181700     .                                                                    
181800     EJECT                                                                
181900 S01AA-FLYTTA-INLF-FAELT       SECTION.                                   
182000                                                                          
182100     MOVE SEQE-IDDC            TO W-D101KY-IDDC                           
182200     MOVE SEQE-IDLEVNR         TO W-D101KY-IDLEVNR                        
182300                                  W-SPAR-IDLEVNR                          
182400     MOVE SEQE-IDFS            TO W-D101KY-IDFS                           
182500                                  W-SPAR-IDFS                             
182600     MOVE SEQE-TIAVIDAT        TO W-D101KY-TIAVIDAT                       
182700                                  W-SPAR-TIAVIDAT                         
182800     MOVE SEQE-IDRADNR-INL     TO W-IDRADNR-INL                           
182900                                  W-SPAR-IDRADNR-INL                      
183000     MOVE SEQE-IDRADNR         TO W-IDRADNR                               
183100                                  W-SPAR-IDRADNR                          
183200     MOVE SEQE-KDINLPRIO       TO W-SPAR-KDINLPRIO                        
183300     .                                                                    
183400     EJECT                                                                
183500 S01B-LAES-ADR-INDEX      SECTION.                                        
183600                                                                          
183700     MOVE NEJ                  TO KOLLI-TRAEFF-SW                         
183800     MOVE HIGH-VALUE           TO W-W6D1G1KY-MAX-X                        
183900     MOVE WS-ADINLOMR-NXT      TO W-D1G1KY-ADINLOMR-NXT-MIN               
184000                                  W-D1G1KY-ADINLOMR-NXT-MAX               
184100                                  W-D1G1KY-ADINLOMR-NXT                   
184200                                  W-ADINLOMR-NXT                          
184300     PERFORM S22-VAELJ-LAESNING-INLH                                      
184400     MOVE STATUS-WS            TO INLH-STATUS-WS                          
184500     PERFORM UNTIL INLH-SEGMENT-SAKNAS OR KOLLI-TRAEFF                    
184600         IF PRIO-LAESNING AND WS-BEFT-FOM = SPACE AND                     
184700           (SEQG-KDINLSTA = '   ' OR 'FPK' OR 'SPC') AND                  
184800           EJ-TORG                                                        
184900           MOVE JA TO KOLLI-TRAEFF-SW                                     
185000         ELSE                                                             
185100           PERFORM S01BA-FLYTTA-INLH-FAELT                                
185200           PERFORM IMS-GU-INLA1-INLA11                                    
185300           PERFORM S02-KOLLA-KOLLI-TRAEFF                                 
185400           IF EJ-KOLLI-TRAEFF                                             
185500               PERFORM S22-VAELJ-LAESNING-INLH                            
185600               MOVE STATUS-WS    TO INLH-STATUS-WS                        
185700           END-IF                                                         
185800           IF KOLLI-TRAEFF AND TORG-LAES                                  
185900             PERFORM IMS-GU-INLA1-INLA11                                  
186000             PERFORM S02A-KOLLA-KOLLI-TRAEFF-TORG                         
186100             IF EJ-KOLLI-TRAEFF                                           
186200               PERFORM S22-VAELJ-LAESNING-INLH                            
186300               MOVE STATUS-WS    TO INLH-STATUS-WS                        
186400             END-IF                                                       
186500           END-IF                                                         
186600         END-IF                                                           
186700     END-PERFORM                                                          
186800     .                                                                    
186900     EJECT                                                                
187000 S01BA-FLYTTA-INLH-FAELT       SECTION.                                   
187100                                                                          
187200     MOVE SEQG-IDDC            TO W-D101KY-IDDC                           
187300     MOVE SEQG-IDLEVNR         TO W-D101KY-IDLEVNR                        
187400                                  W-SPAR-IDLEVNR                          
187500     MOVE SEQG-IDFS            TO W-D101KY-IDFS                           
187600                                  W-SPAR-IDFS                             
187700     MOVE SEQG-TIAVIDAT        TO W-D101KY-TIAVIDAT                       
187800                                  W-SPAR-TIAVIDAT                         
187900     MOVE SEQG-IDRADNR-INL     TO W-IDRADNR-INL                           
188000                                  W-SPAR-IDRADNR-INL                      
188100     MOVE SEQG-IDRADNR         TO W-IDRADNR                               
188200                                  W-SPAR-IDRADNR                          
188300     MOVE SEQG-KDINLPRIO       TO W-SPAR-KDINLPRIO                        
188400     .                                                                    
188500     EJECT                                                                
188600 S01C-MATCHA-PLAC-ADR-INDEX      SECTION.                                 
188700                                                                          
188800     IF INDX                   = 1                                        
188900         PERFORM S01A-LAES-PLAC-INDEX                                     
189000         MOVE ART-W6D111       TO SEQE-ART-W6D111                         
189100         MOVE RAD-W6D121       TO SEQE-RAD-W6D121                         
189200                                                                          
189300         PERFORM S01B-LAES-ADR-INDEX                                      
189400         MOVE ART-W6D111       TO SEQG-ART-W6D111                         
189500         MOVE RAD-W6D121       TO SEQG-RAD-W6D121                         
189600      ELSE                                                                
189700         IF LAES-PLAC                                                     
189800             PERFORM S01A-LAES-PLAC-INDEX                                 
189900             MOVE ART-W6D111   TO SEQE-ART-W6D111                         
190000             MOVE RAD-W6D121   TO SEQE-RAD-W6D121                         
190100          ELSE                                                            
190200             PERFORM S01B-LAES-ADR-INDEX                                  
190300             MOVE ART-W6D111   TO SEQG-ART-W6D111                         
190400             MOVE RAD-W6D121   TO SEQG-RAD-W6D121                         
190500         END-IF                                                           
190600     END-IF                                                               
190700                                                                          
190800     IF INLF-SEGMENT-FINNS AND INLH-SEGMENT-FINNS                         
190900         IF (SEQE-KDINLPRIO    < SEQG-KDINLPRIO) OR                       
191000            (SEQE-KDINLPRIO    = SEQG-KDINLPRIO AND                       
191100             SEQE-W6D1E1       < SEQG-W6D1G1)                             
191200             PERFORM S01CA-FLYTTA-INLF-FAELT                              
191300          ELSE                                                            
191400             PERFORM S01CB-FLYTTA-INLH-FAELT                              
191500         END-IF                                                           
191600      ELSE                                                                
191700         IF INLF-SEGMENT-FINNS                                            
191800             PERFORM S01CA-FLYTTA-INLF-FAELT                              
191900         END-IF                                                           
192000         IF INLH-SEGMENT-FINNS                                            
192100             PERFORM S01CB-FLYTTA-INLH-FAELT                              
192200         END-IF                                                           
192300     END-IF                                                               
192400     .                                                                    
192500     EJECT                                                                
192600 S01CA-FLYTTA-INLF-FAELT       SECTION.                                   
192700                                                                          
192800     MOVE SEQE-ART-W6D111      TO ART-W6D111                              
192900     MOVE SEQE-RAD-W6D121      TO RAD-W6D121                              
193000     MOVE SEQE-IDLEVNR         TO W-SPAR-IDLEVNR                          
193100     MOVE SEQE-IDFS            TO W-SPAR-IDFS                             
193200     MOVE SEQE-TIAVIDAT        TO W-SPAR-TIAVIDAT                         
193300     MOVE SEQE-IDRADNR-INL     TO W-SPAR-IDRADNR-INL                      
193400     MOVE SEQE-IDRADNR         TO W-SPAR-IDRADNR                          
193500     MOVE SEQE-KDINLPRIO       TO W-SPAR-KDINLPRIO                        
193600     MOVE 'F'                  TO BAS-LAES-SW                             
193700     .                                                                    
193800     EJECT                                                                
193900 S01CB-FLYTTA-INLH-FAELT       SECTION.                                   
194000                                                                          
194100     MOVE SEQG-ART-W6D111      TO ART-W6D111                              
194200     MOVE SEQG-RAD-W6D121      TO RAD-W6D121                              
194300     MOVE SEQG-IDLEVNR         TO W-SPAR-IDLEVNR                          
194400     MOVE SEQG-IDFS            TO W-SPAR-IDFS                             
194500     MOVE SEQG-TIAVIDAT        TO W-SPAR-TIAVIDAT                         
194600     MOVE SEQG-IDRADNR-INL     TO W-SPAR-IDRADNR-INL                      
194700     MOVE SEQG-IDRADNR         TO W-SPAR-IDRADNR                          
194800     MOVE SEQG-KDINLPRIO       TO W-SPAR-KDINLPRIO                        
194900     MOVE 'H'                  TO BAS-LAES-SW                             
195000     .                                                                    
195100     EJECT                                                                
195200 S02-KOLLA-KOLLI-TRAEFF  SECTION.                                         
195300                                                                          
195400     MOVE NEJ                  TO KOLLI-TRAEFF-SW                         
195500     PERFORM IMS-GNP-INLA1-INLA21-KVAL                                    
195600     IF SEGMENT-FINNS                                                     
195700       IF  RAD-KDINLSTA = '   ' OR 'FPK' OR 'SPC'                         
195800         IF WS-BEFT-FOM = SPACE                                           
195900           MOVE JA TO KOLLI-TRAEFF-SW                                     
196000         ELSE                                                             
196100           IF RAD-KDINLSTA = '   ' AND                                    
196200              W-BEFT-FOM <= ART-BEFT AND                                  
196300              W-BEFT-TOM >= ART-BEFT                                      
196400             MOVE JA TO KOLLI-TRAEFF-SW                                   
196500           ELSE                                                           
196600             IF (RAD-KDINLSTA = 'FPK' OR 'SPC')  AND                      
196700               (WS-FLINLFB   = YES     OR                                 
196800                WS-FLINLFB   = JA )    AND                                
196900                W-BEFT-FOM <= ART-BEFT AND                                
197000                W-BEFT-TOM >= ART-BEFT                                    
197100               MOVE JA TO KOLLI-TRAEFF-SW                                 
197200             END-IF                                                       
197300           END-IF                                                         
197400         END-IF                                                           
197500       END-IF                                                             
197600     END-IF                                                               
197700     .                                                                    
197800     EJECT                                                                
197900 S02A-KOLLA-KOLLI-TRAEFF-TORG  SECTION.                                   
198000                                                                          
198100     MOVE NEJ                  TO KOLLI-TRAEFF-SW                         
198200     PERFORM IMS-GNP-INLA1-INLA21-KVAL                                    
198300     IF SEGMENT-FINNS                                                     
198400       IF  RAD-KDINLSTA = '   ' OR 'FPK'  OR 'SPC'                        
198500        IF ART-ADLAGOMR = TORG-ADLAGOMR                                   
198600         IF ART-ADGANG >= TORG-ADGANG-FOM AND                             
198700            ART-ADGANG <= TORG-ADGANG-TOM                                 
198800            MOVE JA TO KOLLI-TRAEFF-SW                                    
198900         END-IF                                                           
199000        END-IF                                                            
199100       END-IF                                                             
199200     END-IF                                                               
199300     .                                                                    
199400     EJECT                                                                
199500 S03-KOLLA-KDINLOMR  SECTION.                                             
199600                                                                          
199700     MOVE JA                      TO KDINLOMR-SW                          
199800     IF PLAA-6006-KDINLOMR        = 'FB ' OR 'FBP'                        
199900       CONTINUE                                                           
200000      ELSE                                                                
200100       IF PLAA-6006-KDINLOMR        = 'RTA'                               
200200         IF W-KDINLOMR-RTA-PAR = 'FB ' OR 'FBP'                           
200300           CONTINUE                                                       
200400         ELSE                                                             
200500           MOVE MFS-ALFA-FAELT-FEL TO RESP-KDCMDVAL-ATTR(INDX)            
200600           MOVE NEJ                 TO INDATA-SW                          
200700                                       KDINLOMR-SW                        
200800           MOVE ERR-UPD-NOT-ALLOWED TO RESP-IDMSG-ERROR                   
200900         END-IF                                                           
201000         MOVE WS-ADINLOMR           TO W-6006-ADINLOMR                    
201100         PERFORM IMS-GU-PLAA-PLAA11-BLANK                                 
201200       ELSE                                                               
201300         MOVE MFS-ALFA-FAELT-FEL  TO RESP-KDCMDVAL-ATTR(INDX)             
201400         MOVE NEJ                 TO INDATA-SW                            
201500                                     KDINLOMR-SW                          
201600         MOVE ERR-UPD-NOT-ALLOWED TO RESP-IDMSG-ERROR                     
201700       END-IF                                                             
201800     END-IF                                                               
201900     .                                                                    
202000     EJECT                                                                
202100 S04-KOLLA-KDINLOMR    SECTION.                                           
202200                                                                          
202300     MOVE JA                      TO KDINLOMR-SW                          
202400     IF (PLAA-6006-KDINLOMR       = 'F  ' OR 'FBP') OR                    
202500        (PLAA-6006-KDINLOMR = 'RTA' AND                                   
202600        (W-KDINLOMR-RTA-PAR = 'F  ' OR 'FBP'))                            
202700         CONTINUE                                                         
202800      ELSE                                                                
202900         MOVE MFS-ALFA-FAELT-FEL TO RESP-KDCMDVAL-ATTR(INDX)              
203000         MOVE NEJ                TO INDATA-SW                             
203100                                    KDINLOMR-SW                           
203200        MOVE ERR-UPD-NOT-ALLOWED TO RESP-IDMSG-ERROR                      
203300     END-IF                                                               
203400     .                                                                    
203500     EJECT                                                                
203600 S05-KOLLA-KOLLIN    SECTION.                                             
203700                                                                          
203800     IF REQU-IDLOPNRM-LINE (INDX) NUMERIC                                 
203900       MOVE REQU-IDLOPNRM-LINE(INDX) TO W-D1BSEQ-IDLOPNRM                 
204000       PERFORM IMS-GU-INLA2-INLA21-OKVAL                                  
204100       PERFORM UNTIL SEGMENT-SAKNAS OR INDATA-FEL                         
204200         IF RAD-KDINLSTA         = SPACE AND RAD-IDRADNR > 1              
204300           IF RAD-ADINLOMR     = WS-ADINLOMR                              
204400             IF REQU-KDCMDVAL-LINE(INDX) = 'FPP' OR 'PB'                  
204500                                                 OR 'SPL'                 
204600               IF RAD-FLSATS = NEJ                                        
204700                   CONTINUE                                               
204800               ELSE                                                       
204900                 MOVE NEJ   TO INDATA-SW                                  
205000                 MOVE MFS-ALFA-FAELT-FEL                                  
205100                            TO RESP-KDCMDVAL-ATTR(INDX)                   
205200                 MOVE ERR-KIT-MARK-CASE  TO RESP-IDMSG-ERROR              
205300               END-IF                                                     
205400             END-IF                                                       
205500           ELSE                                                           
205600             MOVE NEJ        TO INDATA-SW                                 
205700             MOVE MFS-ALFA-FAELT-FEL                                      
205800                             TO RESP-KDCMDVAL-ATTR(INDX)                  
205900             MOVE ERR-LOT-NOT-ON-LOC TO RESP-IDMSG-ERROR                  
206000           END-IF                                                         
206100         END-IF                                                           
206200         IF REQU-KDCMDVAL-LINE(INDX) = 'FPP' OR 'PB'                      
206300                                             OR 'SPL'                     
206400           IF RAD-KDINLSTA = '   ' OR 'SAK'                               
206500              MOVE JA      TO OPACKAD-SW                                  
206600           END-IF                                                         
206700         END-IF                                                           
206800         PERFORM IMS-GNP-INLA2-INLA21                                     
206900       END-PERFORM                                                        
207000     ELSE                                                                 
207100        MOVE NEJ                TO INDATA-SW                              
207200        MOVE MFS-ALFA-FAELT-FEL   TO RESP-KDCMDVAL-ATTR(INDX)             
207300        MOVE ERR-CORR-HILITE-FLDS TO RESP-IDMSG-ERROR                     
207400     END-IF                                                               
207500                                                                          
207600     IF REQU-KDCMDVAL-LINE(INDX) = 'FPP' OR 'PB'                          
207700                                         OR 'SPL'                         
207800       IF PACKAD                                                          
207900         MOVE NEJ                 TO INDATA-SW                            
208000         MOVE MFS-ALFA-FAELT-FEL  TO RESP-KDCMDVAL-ATTR(INDX)             
208100         MOVE ERR-UPD-NOT-ALLOWED TO RESP-IDMSG-ERROR                     
208200       END-IF                                                             
208300     END-IF                                                               
208400     EJECT                                                                
208500     .                                                                    
208600 S06-KOLLA-KOLLI     SECTION.                                             
208700                                                                          
208800     MOVE REQU-IDLOPNRM-LINE(INDX) TO W-D1BSEQ-IDLOPNRM                   
208900     MOVE REQU-IDRADNR-LINE(INDX) TO W-IDRADNR                            
209000     PERFORM IMS-GU-INLA2-INLA21                                          
209100     IF SEGMENT-FINNS AND                                                 
209200        RAD-ADINLOMR               =  WS-ADINLOMR                         
209300         IF REQU-KDCMDVAL-LINE(INDX) = 'FPK' OR 'PC' OR 'SPC'             
209400           IF RAD-KDINLSTA = 'FPK' OR 'SPC'                               
209500             MOVE NEJ                 TO INDATA-SW                        
209600             MOVE MFS-ALFA-FAELT-FEL  TO RESP-KDCMDVAL-ATTR(INDX)         
209700             MOVE ERR-UPD-NOT-ALLOWED TO RESP-IDMSG-ERROR                 
209800           ELSE                                                           
209900             IF RAD-FLSATS         = NEJ                                  
210000                 CONTINUE                                                 
210100              ELSE                                                        
210200                 MOVE NEJ            TO INDATA-SW                         
210300                 MOVE MFS-ALFA-FAELT-FEL                                  
210400                                   TO RESP-KDCMDVAL-ATTR(INDX)            
210500                 MOVE ERR-KIT-MARK-CASE  TO RESP-IDMSG-ERROR              
210600             END-IF                                                       
210700           END-IF                                                         
210800         END-IF                                                           
210900      ELSE                                                                
211000         MOVE NEJ                    TO INDATA-SW                         
211100         MOVE MFS-ALFA-FAELT-FEL                                          
211200                                 TO RESP-KDCMDVAL-ATTR(INDX)              
211300         MOVE ERR-LOT-NOT-ON-LOC     TO RESP-IDMSG-ERROR                  
211400     END-IF                                                               
211500     .                                                                    
211600     EJECT                                                                
211700 S07-SKAPA-R6191-MID   SECTION.                                           
211800                                                                          
211900     MOVE +1                   TO 6191-IX                                 
212000                                  MOD6191-MID-KVPOST                      
212100     MOVE 'W6013200'           TO MOD6191-MID-IDPGM                       
212200     MOVE DCS-IDDC             TO MOD6191-MID-IDDC                        
212300     MOVE ART-IDLOPNRM         TO MOD6191-MID-IDLOPNRM (6191-IX)          
212400     MOVE RAD-IDRADNR          TO MOD6191-MID-IDRADNR  (6191-IX)          
212500     MOVE ART-PRARTSTD         TO MOD6191-MID-PRARTSTD (6191-IX)          
212600     MOVE ART-KDINLPRIO        TO MOD6191-MID-KDINLPRIO(6191-IX)          
212700     MOVE +0                   TO MOD6191-MID-KVKOLLI  (6191-IX)          
212800     MOVE 'N'                  TO MOD6191-MID-FLINLI   (6191-IX)          
212900                                                                          
213000     MOVE W-ADINLOMR-OLD       TO MOD6191-MID-ADINLOMR-OLD                
213100                                                   (6191-IX)              
213200     MOVE W-ADINLOMR-NXT-OLD   TO MOD6191-MID-ADINLOMR-NXT-OLD            
213300                                                   (6191-IX)              
213400     MOVE W-KDINLSTA-OLD       TO MOD6191-MID-KDINLSTA-OLD                
213500                                                   (6191-IX)              
213600     MOVE RAD-KVINLART         TO MOD6191-MID-KVINLART-OLD                
213700                                                   (6191-IX)              
213800                                                                          
213900     MOVE RAD-ADINLOMR         TO MOD6191-MID-ADINLOMR-NEW                
214000                                                   (6191-IX)              
214100     MOVE RAD-ADINLOMR-NXT     TO MOD6191-MID-ADINLOMR-NXT-NEW            
214200                                                   (6191-IX)              
214300     MOVE RAD-KDINLSTA         TO MOD6191-MID-KDINLSTA-NEW                
214400                                                   (6191-IX)              
214500     MOVE RAD-KVINLART         TO MOD6191-MID-KVINLART-NEW                
214600                                                   (6191-IX)              
214700     .                                                                    
214800     EJECT                                                                
214900 S08-STARTA-W6T191         SECTION.                                       
215000                                                                          
215100     COMPUTE MOD6191-MID-KVPOST = 6191-IX - 1                             
215200     COMPUTE P-TO-P-KVLL       =  LNG-P-TO-P-PREFIX +                     
215300                                  17 + (MOD6191-MID-KVPOST * 64)          
215400     MOVE 'W6T191X '           TO P-TO-P-KDTRANS                          
215500     MOVE '6132'               TO P-TO-P-IDTRANS                          
215600     MOVE MFS-KDMFSFOR         TO P-TO-P-KDMFSFOR                         
215700                                                                          
215800     MOVE MOD6191-MID-W6I19101 TO P-TO-P-DATA                             
215900                                                                          
216000     IF FOERSTA-6191                                                      
216100         PERFORM IMS-ISRT-ALT-MSG-6191                                    
216200         MOVE NEJ               TO FOERSTA-6191-SW                        
216300      ELSE                                                                
216400         PERFORM IMS-PURG-ALT-MSG-6191                                    
216500     END-IF                                                               
216600     .                                                                    
216700     EJECT                                                                
216800 S09-ANROPA-W611PMRK  SECTION.                                            
216900                                                                          
217000     MOVE RAD-IDLEVNR-KOLLI    TO PMRK-IDLEVNR                            
217100     MOVE RAD-IDOKOLLI         TO PMRK-IDOKOLLI                           
217200     MOVE ZERO                 TO PMRK-IDLOPNRM                           
217300                                  PMRK-IDRADNR                            
217400     CALL W611PMRK USING PMRK-W611PMRK PMRK-INLB-PCB                      
217500                         PMRK-INLC-PCB PMRK-PLAA-PCB                      
217600     .                                                                    
217700     EJECT                                                                
217800 S09A-ANROPA-W611PMRK-PARTI  SECTION.                                     
217900                                                                          
218000     MOVE SPACE                TO PMRK-IDLEVNR                            
218100     MOVE ZERO                 TO PMRK-IDOKOLLI                           
218200     MOVE ART-IDLOPNRM         TO PMRK-IDLOPNRM                           
218300     MOVE RAD-IDRADNR          TO PMRK-IDRADNR                            
218400     CALL W611PMRK USING PMRK-W611PMRK PMRK-INLB-PCB                      
218500                         PMRK-INLC-PCB PMRK-PLAA-PCB                      
218600     .                                                                    
218700     EJECT                                                                
218800 S11-SKAPA-SPAR-RAD1  SECTION.                                            
218900                                                                          
219000     MOVE +1                   TO SPAR-RAD-IDRADNR                        
219100     MOVE WS-ADINLOMR          TO SPAR-RAD-ADINLOMR                       
219200     MOVE SPACE                TO SPAR-RAD-ADINLOMR-NXT                   
219300     MOVE NEJ                  TO SPAR-RAD-FLDIVKLI                       
219400                                  SPAR-RAD-FLKVAANT                       
219500                                  SPAR-RAD-FLSATS                         
219600                                  SPAR-RAD-FLINLFB                        
219700                                  SPAR-RAD-FLPRIO                         
219800                                  SPAR-RAD-FLSVSLS                        
219900     MOVE JA                   TO SPAR-RAD-FLINLFP                        
220000     MOVE ZERO                 TO SPAR-RAD-IDANSTNR                       
220100                                  SPAR-RAD-IDILIRAD                       
220200                                  SPAR-RAD-IDILIST                        
220300                                  SPAR-RAD-IDINLVGN                       
220400                                  SPAR-RAD-IDOKOLLI                       
220500     MOVE SPACE                TO SPAR-RAD-KDINLSTA                       
220600                                  SPAR-RAD-IDLEVNR-KOLLI                  
220700     MOVE RAD-KVINLART         TO SPAR-RAD-KVINLART                       
220800     MOVE ZERO                 TO SPAR-RAD-TIUPPDAT                       
220900                                                                          
221000     MOVE ART-KDINLPRIO    TO SPAR-RAD-KDINLPRIO                          
221100     .                                                                    
221200     EJECT                                                                
221300 S12-SKAPA-R6191-MID-FLY-RAD   SECTION.                                   
221400                                                                          
221500     MOVE 'W6013200'           TO MOD6191-MID-IDPGM                       
221600     MOVE DCS-IDDC             TO MOD6191-MID-IDDC                        
221700     MOVE ART-IDLOPNRM         TO MOD6191-MID-IDLOPNRM (6191-IX)          
221800     MOVE RAD-IDRADNR          TO MOD6191-MID-IDRADNR  (6191-IX)          
221900     MOVE ART-PRARTSTD         TO MOD6191-MID-PRARTSTD (6191-IX)          
222000     MOVE ART-KDINLPRIO        TO MOD6191-MID-KDINLPRIO(6191-IX)          
222100     MOVE +0                   TO MOD6191-MID-KVKOLLI  (6191-IX)          
222200     MOVE 'N'                  TO MOD6191-MID-FLINLI   (6191-IX)          
222300                                                                          
222400     MOVE RAD-ADINLOMR         TO MOD6191-MID-ADINLOMR-OLD                
222500                                                   (6191-IX)              
222600     MOVE RAD-ADINLOMR-NXT     TO MOD6191-MID-ADINLOMR-NXT-OLD            
222700                                                   (6191-IX)              
222800     MOVE RAD-KDINLSTA         TO MOD6191-MID-KDINLSTA-OLD                
222900                                                   (6191-IX)              
223000     MOVE RAD-KVINLART         TO MOD6191-MID-KVINLART-OLD                
223100                                                   (6191-IX)              
223200                                                                          
223300     MOVE SPAR-RAD-ADINLOMR      TO MOD6191-MID-ADINLOMR-NEW              
223400                                                   (6191-IX)              
223500     MOVE SPAR-RAD-ADINLOMR-NXT  TO MOD6191-MID-ADINLOMR-NXT-NEW          
223600                                                   (6191-IX)              
223700     MOVE RAD-KDINLSTA         TO MOD6191-MID-KDINLSTA-NEW                
223800                                                   (6191-IX)              
223900     MOVE ZERO                 TO MOD6191-MID-KVINLART-NEW                
224000                                                   (6191-IX)              
224100     ADD +1                    TO 6191-IX                                 
224200                                                                          
224300     IF 6191-IX                >  MAX-6191-IX                             
224400         PERFORM S08-STARTA-W6T191                                        
224500         MOVE +1               TO 6191-IX                                 
224600     END-IF                                                               
224700     .                                                                    
224800     EJECT                                                                
224900 S13-SKAPA-R6191-MID-RAD1   SECTION.                                      
225000                                                                          
225100     MOVE 'W6013200'           TO MOD6191-MID-IDPGM                       
225200     MOVE DCS-IDDC             TO MOD6191-MID-IDDC                        
225300     MOVE ART-IDLOPNRM         TO MOD6191-MID-IDLOPNRM (6191-IX)          
225400     MOVE RAD-IDRADNR          TO MOD6191-MID-IDRADNR  (6191-IX)          
225500     MOVE ART-PRARTSTD         TO MOD6191-MID-PRARTSTD (6191-IX)          
225600     MOVE ART-KDINLPRIO        TO MOD6191-MID-KDINLPRIO(6191-IX)          
225700     MOVE +0                   TO MOD6191-MID-KVKOLLI  (6191-IX)          
225800     MOVE 'N'                  TO MOD6191-MID-FLINLI   (6191-IX)          
225900                                                                          
226000     MOVE W-ADINLOMR-OLD       TO MOD6191-MID-ADINLOMR-OLD                
226100                                                   (6191-IX)              
226200     MOVE W-ADINLOMR-NXT-OLD   TO MOD6191-MID-ADINLOMR-NXT-OLD            
226300                                                   (6191-IX)              
226400     MOVE W-KDINLSTA-OLD       TO MOD6191-MID-KDINLSTA-OLD                
226500                                                   (6191-IX)              
226600     MOVE W-KVINLART-OLD       TO MOD6191-MID-KVINLART-OLD                
226700                                                   (6191-IX)              
226800                                                                          
226900     MOVE SPAR-RAD-ADINLOMR      TO MOD6191-MID-ADINLOMR-NEW              
227000                                                   (6191-IX)              
227100     MOVE SPAR-RAD-ADINLOMR-NXT  TO MOD6191-MID-ADINLOMR-NXT-NEW          
227200                                                   (6191-IX)              
227300     MOVE SPAR-RAD-KDINLSTA      TO MOD6191-MID-KDINLSTA-NEW              
227400                                                   (6191-IX)              
227500     MOVE SPAR-RAD-KVINLART    TO MOD6191-MID-KVINLART-NEW                
227600                                                   (6191-IX)              
227700     ADD +1                    TO 6191-IX                                 
227800                                                                          
227900     IF 6191-IX                >  MAX-6191-IX                             
228000         PERFORM S08-STARTA-W6T191                                        
228100         MOVE +1               TO 6191-IX                                 
228200     END-IF                                                               
228300     .                                                                    
228400     EJECT                                                                
228500 S14-SKAPA-6197-TRANS SECTION.                                            
228600                                                                          
228700     MOVE SEQB-IDDC     TO 6197-MID-IDDC                                  
228800     MOVE SEQB-IDLEVNR  TO 6197-MID-IDLEVNR(6197-IX)                      
228900     MOVE SEQB-IDFS     TO 6197-MID-IDFS(6197-IX)                         
229000     MOVE SEQB-TIAVIDAT TO 6197-MID-TIAVIDAT(6197-IX)                     
229100     MOVE SEQB-IDRADNR-INL TO 6197-MID-IDRADNR-INL(6197-IX)               
229200     ADD +1             TO 6197-IX                                        
229300                                                                          
229400     IF 6197-IX = MAX-6197-IX                                             
229500       PERFORM S15-STARTA-6197-TRANS                                      
229600     END-IF                                                               
229700     .                                                                    
229800     EJECT                                                                
229900 S15-STARTA-6197-TRANS  SECTION.                                          
230000                                                                          
230100     MOVE SPACE                 TO 6197-MID-IDPRTLST                      
230200     IF DCS-CDC                                                           
230300       MOVE '6F'                  TO 6197-MID-IDPRTLST(1:2)               
230400       IF WS-ADINLOMR = '585L' OR WS-ADINLOMR-NXT = '585L'                
230500         MOVE '5851'                TO 6197-MID-IDPRTLST(3:4)             
230600       ELSE                                                               
230700         IF WS-ADINLOMR = 'LEGO' OR                                       
230800          WS-ADINLOMR = 'F10U' OR WS-ADINLOMR-NXT = 'F10U'                
230900           MOVE '5851'                TO 6197-MID-IDPRTLST(3:4)           
231000         ELSE                                                             
231100           MOVE WS-ADINLOMR (1:3)     TO 6197-MID-IDPRTLST(3:4)           
231200         END-IF                                                           
231300       END-IF                                                             
231400     ELSE                                                                 
231500       MOVE '6L'                  TO 6197-MID-IDPRTLST(1:2)               
231600       MOVE 'PREV'                TO 6197-MID-IDPRTLST(3:4)               
231700     END-IF                                                               
231800     MOVE IDPGM                 TO 6197-MID-IDPGM                         
231900     MOVE 'N'                   TO 6197-MID-FLSVS                         
232000     COMPUTE 6197-MID-KVPOST    = 6197-IX - 1                             
232100     COMPUTE P-TO-P-KVLL        = LNG-P-TO-P-PREFIX +                     
232200                                  26 + (6197-MID-KVPOST * 24)             
232300     MOVE 'W6T197X '           TO P-TO-P-KDTRANS                          
232400     MOVE '6132'               TO P-TO-P-IDTRANS                          
232500     MOVE MFS-KDMFSFOR         TO P-TO-P-KDMFSFOR                         
232600     MOVE 6197-MID-W6I19701 TO P-TO-P-DATA                                
232700     IF FOERSTA-6197                                                      
232800         PERFORM IMS-ISRT-ALT-MSG-6197                                    
232900         MOVE NEJ              TO FOERSTA-6197-SW                         
233000      ELSE                                                                
233100         PERFORM IMS-PURG-ALT-MSG-6197                                    
233200     END-IF                                                               
233300     MOVE +1                   TO 6197-IX                                 
233400     .                                                                    
233500     EJECT                                                                
233600 S16-KOLLA-OM-FBRAPPUTSKR SECTION.                                        
233700                                                                          
233800     IF WS-ADINLOMR = 'FB1P' OR WS-ADINLOMR-NXT = 'FB1P' OR               
233900        WS-ADINLOMR = 'FB2P' OR WS-ADINLOMR-NXT = 'FB2P'                  
234000       MOVE NEJ TO FBRAPP-SW                                              
234100     ELSE                                                                 
234200       MOVE JA TO FBRAPP-SW                                               
234300       PERFORM IMS-GHNP-INLA1-INLA21-FIRST                                
234400       PERFORM UNTIL SEGMENT-SAKNAS OR (NOT FBRAPP)                       
234500         IF RAD-FLINLFP = JA AND RAD-IDRADNR NOT = +1                     
234600           MOVE NEJ TO FBRAPP-SW                                          
234700         END-IF                                                           
234800         PERFORM IMS-GNP-INLA1-INLA21                                     
234900       END-PERFORM                                                        
235000     END-IF                                                               
235100                                                                          
235200     .                                                                    
235300     EJECT                                                                
235400 S17-FIXA-HOPP-MID SECTION.                                               
235500                                                                          
235600     IF RAD-IDOKOLLI > +0                                                 
235700        MOVE RAD-IDLEVNR-KOLLI TO RESP-IDLEVNR-KOLLI-KEY                  
235800        MOVE RAD-IDOKOLLI      TO RESP-IDOKOLLI-KEY                       
235900     ELSE                                                                 
236000        MOVE SPACE             TO RESP-IDLEVNR-KOLLI-KEY                  
236100                                  RESP-IDOKOLLI-KEY                       
236200     END-IF                                                               
236300     MOVE REQU-IDLOPNRM-LINE(INDX) TO RESP-IDLOPNRM-KEY                   
236400     .                                                                    
236500     EJECT                                                                
236600 S20-BESTAEM-SOEKVAEG SECTION.                                            
236700                                                                          
236800     IF  WS-FLINLFB = JA OR YES                                           
236900       IF WS-KDINLQ = 'T'                                                 
237000         IF WS-BEFT-FOM = SPACE                                           
237100           MOVE '1' TO SOEKVAEG-KOD                                       
237200         ELSE                                                             
237300           MOVE '1' TO SOEKVAEG-KOD                                       
237400         END-IF                                                           
237500       ELSE                                                               
237600         IF WS-KDINLQ = 'K'                                               
237700           IF WS-BEFT-FOM = SPACE                                         
237800             MOVE '3' TO SOEKVAEG-KOD                                     
237900           ELSE                                                           
238000             MOVE '3' TO SOEKVAEG-KOD                                     
238100           END-IF                                                         
238200         ELSE                                                             
238300           IF WS-BEFT-FOM = SPACE                                         
238400             MOVE '5' TO SOEKVAEG-KOD                                     
238500           ELSE                                                           
238600             MOVE '5' TO SOEKVAEG-KOD                                     
238700           END-IF                                                         
238800         END-IF                                                           
238900       END-IF                                                             
239000     ELSE                                                                 
239100       IF W-KDINLOMR = 'F  ' OR W-KDINLOMR-RTA-PAR = 'F  '                
239200         IF WS-KDINLQ = 'T'                                               
239300           IF WS-BEFT-FOM = SPACE                                         
239400             MOVE '7' TO SOEKVAEG-KOD                                     
239500           ELSE                                                           
239600             MOVE '8' TO SOEKVAEG-KOD                                     
239700           END-IF                                                         
239800         ELSE                                                             
239900           IF WS-KDINLQ = 'K'                                             
240000             IF WS-BEFT-FOM = SPACE                                       
240100               MOVE '9' TO SOEKVAEG-KOD                                   
240200             ELSE                                                         
240300               MOVE '10' TO SOEKVAEG-KOD                                  
240400             END-IF                                                       
240500           ELSE                                                           
240600             IF WS-BEFT-FOM = SPACE                                       
240700               MOVE '11' TO SOEKVAEG-KOD                                  
240800             ELSE                                                         
240900               MOVE '12' TO SOEKVAEG-KOD                                  
241000             END-IF                                                       
241100           END-IF                                                         
241200         END-IF                                                           
241300       ELSE                                                               
241400         IF W-KDINLOMR = 'FB ' OR W-KDINLOMR-RTA-PAR = 'FB '              
241500           IF WS-KDINLQ = 'T'                                             
241600             IF WS-BEFT-FOM = SPACE                                       
241700               MOVE '13' TO SOEKVAEG-KOD                                  
241800             ELSE                                                         
241900               MOVE '14' TO SOEKVAEG-KOD                                  
242000             END-IF                                                       
242100           ELSE                                                           
242200             IF WS-KDINLQ = 'K'                                           
242300               IF WS-BEFT-FOM = SPACE                                     
242400                 MOVE '15' TO SOEKVAEG-KOD                                
242500               ELSE                                                       
242600                 MOVE '16' TO SOEKVAEG-KOD                                
242700               END-IF                                                     
242800             ELSE                                                         
242900               IF WS-BEFT-FOM = SPACE                                     
243000                 MOVE '17' TO SOEKVAEG-KOD                                
243100               ELSE                                                       
243200                 MOVE '18' TO SOEKVAEG-KOD                                
243300               END-IF                                                     
243400             END-IF                                                       
243500           END-IF                                                         
243600         ELSE                                                             
243700           IF W-KDINLOMR = 'FBP' OR W-KDINLOMR-RTA-PAR = 'FBP'            
243800             IF WS-KDINLQ = 'T'                                           
243900               IF WS-BEFT-FOM = SPACE                                     
244000                 MOVE '19' TO SOEKVAEG-KOD                                
244100               ELSE                                                       
244200                 MOVE '20' TO SOEKVAEG-KOD                                
244300               END-IF                                                     
244400             ELSE                                                         
244500               IF WS-KDINLQ = 'K'                                         
244600                 IF WS-BEFT-FOM = SPACE                                   
244700                   MOVE '21' TO SOEKVAEG-KOD                              
244800                 ELSE                                                     
244900                   MOVE '22' TO SOEKVAEG-KOD                              
245000                 END-IF                                                   
245100               ELSE                                                       
245200                 IF WS-BEFT-FOM = SPACE                                   
245300                   MOVE '23' TO SOEKVAEG-KOD                              
245400                 ELSE                                                     
245500                   MOVE '24' TO SOEKVAEG-KOD                              
245600                 END-IF                                                   
245700               END-IF                                                     
245800             END-IF                                                       
245900           ELSE                                                           
246000             IF WS-KDINLQ = 'T'                                           
246100               IF WS-BEFT-FOM = SPACE                                     
246200                 MOVE '1' TO SOEKVAEG-KOD                                 
246300               ELSE                                                       
246400                 MOVE '2' TO SOEKVAEG-KOD                                 
246500               END-IF                                                     
246600             ELSE                                                         
246700               IF WS-KDINLQ = 'K'                                         
246800                 IF WS-BEFT-FOM = SPACE                                   
246900                   MOVE '3' TO SOEKVAEG-KOD                               
247000                 ELSE                                                     
247100                   MOVE '4' TO SOEKVAEG-KOD                               
247200                 END-IF                                                   
247300               ELSE                                                       
247400                 IF WS-BEFT-FOM = SPACE                                   
247500                   MOVE '5' TO SOEKVAEG-KOD                               
247600                 ELSE                                                     
247700                   MOVE '6' TO SOEKVAEG-KOD                               
247800                 END-IF                                                   
247900               END-IF                                                     
248000             END-IF                                                       
248100           END-IF                                                         
248200         END-IF                                                           
248300       END-IF                                                             
248400     END-IF                                                               
248500     .                                                                    
248600     EJECT                                                                
248700 S21-VAELJ-LAESNING-INLF SECTION.                                         
248800                                                                          
248900     EVALUATE SOEKVAEG-KOD                                                
249000       WHEN '01'                                                          
249100          PERFORM IMS-GN-INLF-INLF01-V01                                  
249200       WHEN '02'                                                          
249300          PERFORM IMS-GN-INLF-INLF01-V02                                  
249400       WHEN '03'                                                          
249500          PERFORM IMS-GN-INLF-INLF01-V03                                  
249600       WHEN '04'                                                          
249700          PERFORM IMS-GN-INLF-INLF01-V04                                  
249800       WHEN '05'                                                          
249900          PERFORM IMS-GN-INLF-INLF01-V05                                  
250000       WHEN '06'                                                          
250100          PERFORM IMS-GN-INLF-INLF01-V06                                  
250200       WHEN '07'                                                          
250300          PERFORM IMS-GN-INLF-INLF01-V07                                  
250400       WHEN '08'                                                          
250500          PERFORM IMS-GN-INLF-INLF01-V08                                  
250600       WHEN '09'                                                          
250700          PERFORM IMS-GN-INLF-INLF01-V09                                  
250800       WHEN '10'                                                          
250900          PERFORM IMS-GN-INLF-INLF01-V10                                  
251000       WHEN '11'                                                          
251100          PERFORM IMS-GN-INLF-INLF01-V11                                  
251200       WHEN '12'                                                          
251300          PERFORM IMS-GN-INLF-INLF01-V12                                  
251400       WHEN '13'                                                          
251500          PERFORM IMS-GN-INLF-INLF01-V13                                  
251600       WHEN '14'                                                          
251700          PERFORM IMS-GN-INLF-INLF01-V14                                  
251800       WHEN '15'                                                          
251900          PERFORM IMS-GN-INLF-INLF01-V15                                  
252000       WHEN '16'                                                          
252100          PERFORM IMS-GN-INLF-INLF01-V16                                  
252200       WHEN '17'                                                          
252300          PERFORM IMS-GN-INLF-INLF01-V17                                  
252400       WHEN '18'                                                          
252500          PERFORM IMS-GN-INLF-INLF01-V18                                  
252600       WHEN '19'                                                          
252700          PERFORM IMS-GN-INLF-INLF01-V19                                  
252800       WHEN '20'                                                          
252900          PERFORM IMS-GN-INLF-INLF01-V20                                  
253000       WHEN '21'                                                          
253100          PERFORM IMS-GN-INLF-INLF01-V21                                  
253200       WHEN '22'                                                          
253300          PERFORM IMS-GN-INLF-INLF01-V22                                  
253400       WHEN '23'                                                          
253500          PERFORM IMS-GN-INLF-INLF01-V23                                  
253600       WHEN '24'                                                          
253700          PERFORM IMS-GN-INLF-INLF01-V24                                  
253800       WHEN OTHER                                                         
253900          MOVE 'FEL ISECTION S21-' TO FELTEXT                             
254000     END-EVALUATE                                                         
254100     .                                                                    
254200     EJECT                                                                
254300 S22-VAELJ-LAESNING-INLH SECTION.                                         
254400                                                                          
254500     EVALUATE SOEKVAEG-KOD                                                
254600       WHEN '01'                                                          
254700          PERFORM IMS-GN-INLH-INLH01-V01                                  
254800       WHEN '02'                                                          
254900          PERFORM IMS-GN-INLH-INLH01-V02                                  
255000       WHEN '03'                                                          
255100          PERFORM IMS-GN-INLH-INLH01-V03                                  
255200       WHEN '04'                                                          
255300          PERFORM IMS-GN-INLH-INLH01-V04                                  
255400       WHEN '05'                                                          
255500          PERFORM IMS-GN-INLH-INLH01-V05                                  
255600       WHEN '06'                                                          
255700          PERFORM IMS-GN-INLH-INLH01-V06                                  
255800       WHEN '07'                                                          
255900          PERFORM IMS-GN-INLH-INLH01-V07                                  
256000       WHEN '08'                                                          
256100          PERFORM IMS-GN-INLH-INLH01-V08                                  
256200       WHEN '09'                                                          
256300          PERFORM IMS-GN-INLH-INLH01-V09                                  
256400       WHEN '10'                                                          
256500          PERFORM IMS-GN-INLH-INLH01-V10                                  
256600       WHEN '11'                                                          
256700          PERFORM IMS-GN-INLH-INLH01-V11                                  
256800       WHEN '12'                                                          
256900          PERFORM IMS-GN-INLH-INLH01-V12                                  
257000       WHEN '13'                                                          
257100          PERFORM IMS-GN-INLH-INLH01-V13                                  
257200       WHEN '14'                                                          
257300          PERFORM IMS-GN-INLH-INLH01-V14                                  
257400       WHEN '15'                                                          
257500          PERFORM IMS-GN-INLH-INLH01-V15                                  
257600       WHEN '16'                                                          
257700          PERFORM IMS-GN-INLH-INLH01-V16                                  
257800       WHEN '17'                                                          
257900          PERFORM IMS-GN-INLH-INLH01-V17                                  
258000       WHEN '18'                                                          
258100          PERFORM IMS-GN-INLH-INLH01-V18                                  
258200       WHEN '19'                                                          
258300          PERFORM IMS-GN-INLH-INLH01-V19                                  
258400       WHEN '20'                                                          
258500          PERFORM IMS-GN-INLH-INLH01-V20                                  
258600       WHEN '21'                                                          
258700          PERFORM IMS-GN-INLH-INLH01-V21                                  
258800       WHEN '22'                                                          
258900          PERFORM IMS-GN-INLH-INLH01-V22                                  
259000       WHEN '23'                                                          
259100          PERFORM IMS-GN-INLH-INLH01-V23                                  
259200       WHEN '24'                                                          
259300          PERFORM IMS-GN-INLH-INLH01-V24                                  
259400       WHEN OTHER                                                         
259500          MOVE 'FEL ISECTION S22-' TO FELTEXT                             
259600     END-EVALUATE                                                         
259700     .                                                                    
259800     EJECT                                                                
259900 MFS-RENSA-FAELT-IN SECTION.                                              
260000                                                                          
260100     MOVE +1                   TO INDX                                    
260200     PERFORM UNTIL INDX        >  MAX-KVRADER                             
260300         MOVE SPACE            TO RESP-KDCMDVAL-LINE (INDX)               
260400         ADD +1                TO INDX                                    
260500     END-PERFORM                                                          
260600     .                                                                    
260700     EJECT                                                                
260800 MFS-FORM-ATTR SECTION.                                                   
260900                                                                          
261000     MOVE +1                     TO INDX                                  
261100     PERFORM UNTIL INDX          >  MAX-KVRADER                           
261200         MOVE MFS-FORMATETS-ATTR TO RESP-KDCMDVAL-ATTR (INDX)             
261300         ADD +1                  TO INDX                                  
261400     END-PERFORM                                                          
261500     .                                                                    
261600     SKIP2                                                                
261700 MFS-LAES-IN-IGEN SECTION.                                                
261800                                                                          
261900     MOVE +1                     TO INDX                                  
262000     PERFORM UNTIL INDX          >  MAX-KVRADER                           
262100         MOVE MFS-ADD-LAES-IN-FAELT                                       
262200                                 TO RESP-KDCMDVAL-ATTR (INDX)             
262300         ADD +1                  TO INDX                                  
262400     END-PERFORM                                                          
262500     .                                                                    
262600     EJECT                                                                
262700* --- IMS SEKTIONER ---                                                   
262800     SKIP3                                                                
262900 IMS-ISRT-ALT-MSG-6191  SECTION.                                          
263000     MOVE SPACE TO GODK-STATUSKODER                                       
263100     CALL  CBLTDLI  USING ISRT ALT-PCB P-TO-P-SW                          
263200     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
263300     PERFORM IMS-STATUSKONTROLL                                           
263400     .                                                                    
263500     SKIP3                                                                
263600 IMS-PURG-ALT-MSG-6191  SECTION.                                          
263700     MOVE SPACE TO GODK-STATUSKODER                                       
263800     CALL  CBLTDLI  USING PURG ALT-PCB P-TO-P-SW                          
263900     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
264000     PERFORM IMS-STATUSKONTROLL                                           
264100     .                                                                    
264200     SKIP3                                                                
264300 IMS-ISRT-ALT-MSG-6197  SECTION.                                          
264400     MOVE SPACE TO GODK-STATUSKODER                                       
264500     CALL  CBLTDLI  USING ISRT 6197-PCB P-TO-P-SW                         
264600     MOVE 6197-STATUS-CODE TO STATUS-WS                                   
264700     PERFORM IMS-STATUSKONTROLL                                           
264800     .                                                                    
264900     SKIP3                                                                
265000 IMS-PURG-ALT-MSG-6197  SECTION.                                          
265100     MOVE SPACE TO GODK-STATUSKODER                                       
265200     CALL  CBLTDLI  USING PURG 6197-PCB P-TO-P-SW                         
265300     MOVE 6197-STATUS-CODE TO STATUS-WS                                   
265400     PERFORM IMS-STATUSKONTROLL                                           
265500     .                                                                    
265600     EJECT                                                                
265700 IMS-GU-PLAA-PLAA11 SECTION.                                              
265800     STRING 'W6PLAA01(W6GXKEY  =' W-W6GXKEY-6005-X ')'                    
265900          DELIMITED BY SIZE INTO SSA1                                     
266000     STRING 'W6PLAA11(W6GXKEY  =' W-W6GXKEY-6006-X ')'                    
266100          DELIMITED BY SIZE INTO SSA2                                     
266200     MOVE '  GE' TO GODK-STATUSKODER                                      
266300     CALL CBLTDLI USING GU PLAA-PCB DLI-IO-AREA1 SSA1 SSA2                
266400     MOVE PLAA-STATUS-CODE TO STATUS-WS                                   
266500     PERFORM IMS-STATUSKONTROLL                                           
266600     .                                                                    
266700     SKIP3                                                                
266800 IMS-GU-PLAA-PLAA11-BLANK SECTION.                                        
266900     STRING 'W6PLAA01(W6GXKEY  =' W-W6GXKEY-6005-X ')'                    
267000          DELIMITED BY SIZE INTO SSA1                                     
267100     STRING 'W6PLAA11(W6GXKEY  =' W-W6GXKEY-6006-X ')'                    
267200          DELIMITED BY SIZE INTO SSA2                                     
267300     MOVE '  ' TO GODK-STATUSKODER                                        
267400     CALL CBLTDLI USING GU PLAA-PCB DLI-IO-AREA1 SSA1 SSA2                
267500     MOVE PLAA-STATUS-CODE TO STATUS-WS                                   
267600     PERFORM IMS-STATUSKONTROLL                                           
267700     .                                                                    
267800     SKIP3                                                                
267900 IMS-GU-INLA1-INLA11 SECTION.                                             
268000     STRING 'W6INLA01(W6D101KY =' W-W6D101KY-X ')'                        
268100          DELIMITED BY SIZE INTO SSA1                                     
268200     STRING 'W6INLA11(IDRADNRI =' W-IDRADNR-INL-X ')'                     
268300          DELIMITED BY SIZE INTO SSA2                                     
268400     MOVE '    ' TO GODK-STATUSKODER                                      
268500     CALL CBLTDLI USING GU INLA1-PCB DLI-IO-AREA2 SSA1 SSA2               
268600     MOVE INLA1-STATUS-CODE TO STATUS-WS                                  
268700     PERFORM IMS-STATUSKONTROLL                                           
268800     .                                                                    
268900     EJECT                                                                
269000 IMS-GHU-INLA1-INLA11 SECTION.                                            
269100     STRING 'W6INLA01(W6D101KY =' W-W6D101KY-X ')'                        
269200          DELIMITED BY SIZE INTO SSA1                                     
269300     STRING 'W6INLA11(IDRADNRI =' W-IDRADNR-INL-X ')'                     
269400          DELIMITED BY SIZE INTO SSA2                                     
269500     MOVE '    ' TO GODK-STATUSKODER                                      
269600     CALL CBLTDLI USING GHU INLA1-PCB DLI-IO-AREA2 SSA1 SSA2              
269700     MOVE INLA1-STATUS-CODE TO STATUS-WS                                  
269800     PERFORM IMS-STATUSKONTROLL                                           
269900     .                                                                    
270000     SKIP3                                                                
270100 IMS-REPL-INLA1-INLA11 SECTION.                                           
270200     MOVE '    ' TO GODK-STATUSKODER                                      
270300     CALL CBLTDLI USING REPL INLA1-PCB DLI-IO-AREA2                       
270400     MOVE INLA1-STATUS-CODE TO STATUS-WS                                  
270500     PERFORM IMS-STATUSKONTROLL                                           
270600     .                                                                    
270700     SKIP3                                                                
270800 IMS-GNP-INLA1-INLA21-KVAL SECTION.                                       
270900     STRING 'W6INLA21(IDRADNR  =' W-IDRADNR-X ')'                         
271000          DELIMITED BY SIZE INTO SSA1                                     
271100     MOVE '  GE' TO GODK-STATUSKODER                                      
271200     CALL CBLTDLI USING GNP INLA1-PCB DLI-IO-AREA3 SSA1                   
271300     MOVE INLA1-STATUS-CODE TO STATUS-WS                                  
271400     PERFORM IMS-STATUSKONTROLL                                           
271500     .                                                                    
271600     EJECT                                                                
271700 IMS-GNP-INLA1-INLA21-FIRST SECTION.                                      
271800     MOVE   'W6INLA21*F'         TO SSA1                                  
271900     MOVE '  GE' TO GODK-STATUSKODER                                      
272000     CALL CBLTDLI USING GNP INLA1-PCB DLI-IO-AREA3 SSA1                   
272100     MOVE INLA1-STATUS-CODE TO STATUS-WS                                  
272200     PERFORM IMS-STATUSKONTROLL                                           
272300     .                                                                    
272400     SKIP3                                                                
272500 IMS-GNP-INLA1-INLA21 SECTION.                                            
272600     MOVE   'W6INLA21'         TO SSA1                                    
272700     MOVE '  GE' TO GODK-STATUSKODER                                      
272800     CALL CBLTDLI USING GNP INLA1-PCB DLI-IO-AREA3 SSA1                   
272900     MOVE INLA1-STATUS-CODE TO STATUS-WS                                  
273000     PERFORM IMS-STATUSKONTROLL                                           
273100     .                                                                    
273200     SKIP3                                                                
273300 IMS-GHU-INLA1-INLA21-KVAL SECTION.                                       
273400     STRING 'W6INLA01(W6D101KY =' W-W6D101KY-X ')'                        
273500          DELIMITED BY SIZE INTO SSA1                                     
273600     STRING 'W6INLA11*P(IDRADNRI =' W-IDRADNR-INL-X ')'                   
273700          DELIMITED BY SIZE INTO SSA2                                     
273800     STRING 'W6INLA21(IDRADNR  =' W-IDRADNR-X ')'                         
273900          DELIMITED BY SIZE INTO SSA3                                     
274000     MOVE '    ' TO GODK-STATUSKODER                                      
274100     CALL CBLTDLI USING GHU INLA1-PCB DLI-IO-AREA3 SSA1 SSA2 SSA3         
274200     MOVE INLA1-STATUS-CODE TO STATUS-WS                                  
274300     PERFORM IMS-STATUSKONTROLL                                           
274400     .                                                                    
274500     SKIP3                                                                
274600 IMS-GHU-INLA1-INLA21-OKVAL SECTION.                                      
274700     STRING 'W6INLA01(W6D101KY =' W-W6D101KY-X ')'                        
274800          DELIMITED BY SIZE INTO SSA1                                     
274900     STRING 'W6INLA11*P(IDRADNRI =' W-IDRADNR-INL-X ')'                   
275000          DELIMITED BY SIZE INTO SSA2                                     
275100     MOVE 'W6INLA21' TO SSA3                                              
275200     MOVE '    ' TO GODK-STATUSKODER                                      
275300     CALL CBLTDLI USING GHU INLA1-PCB DLI-IO-AREA3 SSA1 SSA2 SSA3         
275400     MOVE INLA1-STATUS-CODE TO STATUS-WS                                  
275500     PERFORM IMS-STATUSKONTROLL                                           
275600     .                                                                    
275700     EJECT                                                                
275800 IMS-GHNP-INLA1-INLA21-KVAL SECTION.                                      
275900     STRING 'W6INLA21(IDRADNR  =' W-IDRADNR-X ')'                         
276000          DELIMITED BY SIZE INTO SSA1                                     
276100     MOVE '  GE' TO GODK-STATUSKODER                                      
276200     CALL CBLTDLI USING GHNP INLA1-PCB DLI-IO-AREA3 SSA1                  
276300     MOVE INLA1-STATUS-CODE TO STATUS-WS                                  
276400     PERFORM IMS-STATUSKONTROLL                                           
276500     .                                                                    
276600     SKIP3                                                                
276700 IMS-GHNP-INLA1-INLA21-OKVAL SECTION.                                     
276800     MOVE 'W6INLA21'           TO SSA1                                    
276900     MOVE '  GE' TO GODK-STATUSKODER                                      
277000     CALL CBLTDLI USING GHNP INLA1-PCB DLI-IO-AREA3 SSA1                  
277100     MOVE INLA1-STATUS-CODE TO STATUS-WS                                  
277200     PERFORM IMS-STATUSKONTROLL                                           
277300     .                                                                    
277400     SKIP3                                                                
277500 IMS-GHNP-INLA1-INLA21-FIRST SECTION.                                     
277600     MOVE  'W6INLA21*F'     TO SSA1                                       
277700     MOVE '    ' TO GODK-STATUSKODER                                      
277800     CALL CBLTDLI USING GHNP INLA1-PCB DLI-IO-AREA3 SSA1                  
277900     MOVE INLA1-STATUS-CODE TO STATUS-WS                                  
278000     PERFORM IMS-STATUSKONTROLL                                           
278100     .                                                                    
278200     EJECT                                                                
278300 IMS-REPL-INLA1-INLA21 SECTION.                                           
278400     MOVE '    ' TO GODK-STATUSKODER                                      
278500     CALL CBLTDLI USING REPL INLA1-PCB DLI-IO-AREA3                       
278600     MOVE INLA1-STATUS-CODE TO STATUS-WS                                  
278700     PERFORM IMS-STATUSKONTROLL                                           
278800     .                                                                    
278900     SKIP3                                                                
279000 IMS-DLET-INLA1-INLA21 SECTION.                                           
279100     MOVE '    ' TO GODK-STATUSKODER                                      
279200     CALL CBLTDLI USING DLET INLA1-PCB DLI-IO-AREA3                       
279300     MOVE INLA1-STATUS-CODE TO STATUS-WS                                  
279400     PERFORM IMS-STATUSKONTROLL                                           
279500     .                                                                    
279600     SKIP3                                                                
279700 IMS-ISRT-INLA1-INLA21 SECTION.                                           
279800     MOVE 'W6INLA21'           TO SSA1                                    
279900     MOVE '    ' TO GODK-STATUSKODER                                      
280000     CALL CBLTDLI USING ISRT INLA1-PCB DLI-IO-AREA3 SSA1                  
280100     MOVE INLA1-STATUS-CODE TO STATUS-WS                                  
280200     PERFORM IMS-STATUSKONTROLL                                           
280300     .                                                                    
280400     EJECT                                                                
280500 IMS-GU-INLC-INLC01 SECTION.                                              
280600     STRING 'W6INLC01(W6D1B1KY =' W-IDLOPNRM-X ')'                        
280700          DELIMITED BY SIZE INTO SSA1                                     
280800     MOVE '  GE' TO GODK-STATUSKODER                                      
280900     CALL CBLTDLI USING GU INLC-PCB DLI-IO-AREA5 SSA1                     
281000     MOVE INLC-STATUS-CODE TO STATUS-WS                                   
281100     PERFORM IMS-STATUSKONTROLL                                           
281200     .                                                                    
281300     SKIP3                                                                
281400 IMS-GU-INLA2-INLA21 SECTION.                                             
281500     STRING 'W6INLA11*P(W6D1BSEQ =' W-W6D1BSEQ-X                          
281600                      '&IDDC     =' W-IDDC-X ')'                          
281700          DELIMITED BY SIZE INTO SSA1                                     
281800     STRING 'W6INLA21(IDRADNR  =' W-IDRADNR-X ')'                         
281900          DELIMITED BY SIZE INTO SSA2                                     
282000     MOVE '  GE' TO GODK-STATUSKODER                                      
282100     CALL CBLTDLI USING GU INLA2-PCB DLI-IO-AREA3 SSA1 SSA2               
282200     MOVE INLA2-STATUS-CODE TO STATUS-WS                                  
282300     PERFORM IMS-STATUSKONTROLL                                           
282400     .                                                                    
282500     SKIP3                                                                
282600 IMS-GU-INLA2-INLA21-OKVAL SECTION.                                       
282700     STRING 'W6INLA11*P(W6D1BSEQ =' W-W6D1BSEQ-X                          
282800                      '&IDDC     =' W-IDDC-X ')'                          
282900          DELIMITED BY SIZE INTO SSA1                                     
283000     MOVE 'W6INLA21'          TO SSA2                                     
283100     MOVE '  GE' TO GODK-STATUSKODER                                      
283200     CALL CBLTDLI USING GU INLA2-PCB DLI-IO-AREA3 SSA1 SSA2               
283300     MOVE INLA2-STATUS-CODE TO STATUS-WS                                  
283400     PERFORM IMS-STATUSKONTROLL                                           
283500     .                                                                    
283600     SKIP3                                                                
283700 IMS-GNP-INLA2-INLA21   SECTION.                                          
283800     MOVE 'W6INLA21'          TO SSA1                                     
283900     MOVE '  GE' TO GODK-STATUSKODER                                      
284000     CALL CBLTDLI USING GNP INLA2-PCB DLI-IO-AREA3 SSA1                   
284100     MOVE INLA2-STATUS-CODE TO STATUS-WS                                  
284200     PERFORM IMS-STATUSKONTROLL                                           
284300     .                                                                    
284400     SKIP3                                                                
284500 IMS-GU-INLA3-INLA11 SECTION.                                             
284600     STRING 'W6INLA11(W6D1CSEQ =' W-W6D1CSEQ-X                            
284700                    '&IDDC     =' W-IDDC-X ')'                            
284800          DELIMITED BY SIZE INTO SSA1                                     
284900     MOVE '  GE' TO GODK-STATUSKODER                                      
285000     CALL CBLTDLI USING GU INLA3-PCB DLI-IO-AREA3 SSA1                    
285100     MOVE INLA3-STATUS-CODE TO STATUS-WS                                  
285200     PERFORM IMS-STATUSKONTROLL                                           
285300     .                                                                    
285400     SKIP3                                                                
285500 IMS-GN-INLA3-INLA11 SECTION.                                             
285600     STRING 'W6INLA11(W6D1CSEQ =' W-W6D1CSEQ-X                            
285700                    '&IDDC     =' W-IDDC-X ')'                            
285800          DELIMITED BY SIZE INTO SSA1                                     
285900     MOVE '  GE' TO GODK-STATUSKODER                                      
286000     CALL CBLTDLI USING GN INLA3-PCB DLI-IO-AREA3 SSA1                    
286100     MOVE INLA3-STATUS-CODE TO STATUS-WS                                  
286200     PERFORM IMS-STATUSKONTROLL                                           
286300     .                                                                    
286400     SKIP3                                                                
286500 IMS-GHNP-INLA3-INLA21 SECTION.                                           
286600     STRING 'W6INLA21(IDOKOLLI =' W-IDOKOLLI-X                            
286700                    '&IDLEVNRK =' W-IDLEVNRK-X ')'                        
286800          DELIMITED BY SIZE INTO SSA1                                     
286900     MOVE '  GE' TO GODK-STATUSKODER                                      
287000     CALL CBLTDLI USING GHNP INLA3-PCB DLI-IO-AREA3 SSA1                  
287100     MOVE INLA3-STATUS-CODE TO STATUS-WS                                  
287200     PERFORM IMS-STATUSKONTROLL                                           
287300     .                                                                    
287400     SKIP2                                                                
287500 IMS-REPL-INLA3-INLA21 SECTION.                                           
287600     MOVE '    ' TO GODK-STATUSKODER                                      
287700     CALL CBLTDLI USING REPL INLA3-PCB DLI-IO-AREA3                       
287800     MOVE INLA3-STATUS-CODE TO STATUS-WS                                  
287900     PERFORM IMS-STATUSKONTROLL                                           
288000     .                                                                    
288100     EJECT                                                                
288200 IMS-GN-INLF-INLF01-V01 SECTION.                                          
288300     STRING 'W6INLF01(W6D1E1KY>=' W-W6D1E1KY-MIN-X                        
288400                    '&W6D1E1KY<=' W-W6D1E1KY-MAX-X                        
288500                    '&IDDC     =' W-IDDC-X ')'                            
288600          DELIMITED BY SIZE INTO SSA1                                     
288700     MOVE '  GBGE' TO GODK-STATUSKODER                                    
288800     CALL CBLTDLI USING GN INLF-PCB DLI-IO-AREA1 SSA1                     
288900     MOVE INLF-STATUS-CODE TO STATUS-WS                                   
289000     PERFORM IMS-STATUSKONTROLL                                           
289100     .                                                                    
289200     SKIP2                                                                
289300 IMS-GN-INLF-INLF01-V02 SECTION.                                          
289400     STRING 'W6INLF01(W6D1E1KY>=' W-W6D1E1KY-MIN-X                        
289500                    '&W6D1E1KY<=' W-W6D1E1KY-MAX-X                        
289600                    '&IDDC     =' W-IDDC-X                                
289700                    '&KDINLSTA =' W-KDINLSTA ')'                          
289800          DELIMITED BY SIZE INTO SSA1                                     
289900     MOVE '  GBGE' TO GODK-STATUSKODER                                    
290000     CALL CBLTDLI USING GN INLF-PCB DLI-IO-AREA1 SSA1                     
290100     MOVE INLF-STATUS-CODE TO STATUS-WS                                   
290200     PERFORM IMS-STATUSKONTROLL                                           
290300     .                                                                    
290400     SKIP2                                                                
290500 IMS-GN-INLF-INLF01-V03 SECTION.                                          
290600     STRING 'W6INLF01(W6D1E1KY>=' W-W6D1E1KY-MIN-X                        
290700                    '&W6D1E1KY<=' W-W6D1E1KY-MAX-X                        
290800                    '&IDDC     =' W-IDDC-X                                
290900                    '&IDINLVGN =' W-IDINLVGN ')'                          
291000          DELIMITED BY SIZE INTO SSA1                                     
291100     MOVE '  GBGE' TO GODK-STATUSKODER                                    
291200     CALL CBLTDLI USING GN INLF-PCB DLI-IO-AREA1 SSA1                     
291300     MOVE INLF-STATUS-CODE TO STATUS-WS                                   
291400     PERFORM IMS-STATUSKONTROLL                                           
291500     .                                                                    
291600     SKIP2                                                                
291700 IMS-GN-INLF-INLF01-V04 SECTION.                                          
291800     STRING 'W6INLF01(W6D1E1KY>=' W-W6D1E1KY-MIN-X                        
291900                    '&W6D1E1KY<=' W-W6D1E1KY-MAX-X                        
292000                    '&IDDC     =' W-IDDC-X                                
292100                    '&IDINLVGN =' W-IDINLVGN                              
292200                    '&KDINLSTA =' W-KDINLSTA ')'                          
292300          DELIMITED BY SIZE INTO SSA1                                     
292400     MOVE '  GBGE' TO GODK-STATUSKODER                                    
292500     CALL CBLTDLI USING GN INLF-PCB DLI-IO-AREA1 SSA1                     
292600     MOVE INLF-STATUS-CODE TO STATUS-WS                                   
292700     PERFORM IMS-STATUSKONTROLL                                           
292800     .                                                                    
292900     SKIP2                                                                
293000 IMS-GN-INLF-INLF01-V05 SECTION.                                          
293100     STRING 'W6INLF01(W6D1E1KY>=' W-W6D1E1KY-MIN-X                        
293200                    '&W6D1E1KY<=' W-W6D1E1KY-MAX-X                        
293300                    '&IDDC     =' W-IDDC-X                                
293400                    '&IDINLVGN >' W-IDINLVGN ')'                          
293500          DELIMITED BY SIZE INTO SSA1                                     
293600     MOVE '  GBGE' TO GODK-STATUSKODER                                    
293700     CALL CBLTDLI USING GN INLF-PCB DLI-IO-AREA1 SSA1                     
293800     MOVE INLF-STATUS-CODE TO STATUS-WS                                   
293900     PERFORM IMS-STATUSKONTROLL                                           
294000     .                                                                    
294100     SKIP2                                                                
294200 IMS-GN-INLF-INLF01-V06 SECTION.                                          
294300     STRING 'W6INLF01(W6D1E1KY>=' W-W6D1E1KY-MIN-X                        
294400                    '&W6D1E1KY<=' W-W6D1E1KY-MAX-X                        
294500                    '&IDDC     =' W-IDDC-X                                
294600                    '&IDINLVGN >' W-IDINLVGN                              
294700                    '&KDINLSTA =' W-KDINLSTA ')'                          
294800          DELIMITED BY SIZE INTO SSA1                                     
294900     MOVE '  GBGE' TO GODK-STATUSKODER                                    
295000     CALL CBLTDLI USING GN INLF-PCB DLI-IO-AREA1 SSA1                     
295100     MOVE INLF-STATUS-CODE TO STATUS-WS                                   
295200     PERFORM IMS-STATUSKONTROLL                                           
295300     .                                                                    
295400     SKIP2                                                                
295500 IMS-GN-INLF-INLF01-V07 SECTION.                                          
295600     STRING 'W6INLF01(W6D1E1KY>=' W-W6D1E1KY-MIN-X                        
295700                    '&W6D1E1KY<=' W-W6D1E1KY-MAX-X                        
295800                    '&IDDC     =' W-IDDC-X                                
295900                    '&FLINLFP  =' W-FLINLFP ')'                           
296000          DELIMITED BY SIZE INTO SSA1                                     
296100     MOVE '  GBGE' TO GODK-STATUSKODER                                    
296200     CALL CBLTDLI USING GN INLF-PCB DLI-IO-AREA1 SSA1                     
296300     MOVE INLF-STATUS-CODE TO STATUS-WS                                   
296400     PERFORM IMS-STATUSKONTROLL                                           
296500     .                                                                    
296600     SKIP2                                                                
296700 IMS-GN-INLF-INLF01-V08 SECTION.                                          
296800     STRING 'W6INLF01(W6D1E1KY>=' W-W6D1E1KY-MIN-X                        
296900                    '&W6D1E1KY<=' W-W6D1E1KY-MAX-X                        
297000                    '&IDDC     =' W-IDDC-X                                
297100                    '&FLINLFP  =' W-FLINLFP                               
297200                    '&KDINLSTA =' W-KDINLSTA ')'                          
297300          DELIMITED BY SIZE INTO SSA1                                     
297400     MOVE '  GBGE' TO GODK-STATUSKODER                                    
297500     CALL CBLTDLI USING GN INLF-PCB DLI-IO-AREA1 SSA1                     
297600     MOVE INLF-STATUS-CODE TO STATUS-WS                                   
297700     PERFORM IMS-STATUSKONTROLL                                           
297800     .                                                                    
297900     SKIP2                                                                
298000 IMS-GN-INLF-INLF01-V09 SECTION.                                          
298100     STRING 'W6INLF01(W6D1E1KY>=' W-W6D1E1KY-MIN-X                        
298200                    '&W6D1E1KY<=' W-W6D1E1KY-MAX-X                        
298300                    '&IDDC     =' W-IDDC-X                                
298400                    '&FLINLFP  =' W-FLINLFP                               
298500                    '&IDINLVGN =' W-IDINLVGN ')'                          
298600          DELIMITED BY SIZE INTO SSA1                                     
298700     MOVE '  GBGE' TO GODK-STATUSKODER                                    
298800     CALL CBLTDLI USING GN INLF-PCB DLI-IO-AREA1 SSA1                     
298900     MOVE INLF-STATUS-CODE TO STATUS-WS                                   
299000     PERFORM IMS-STATUSKONTROLL                                           
299100     .                                                                    
299200     SKIP2                                                                
299300 IMS-GN-INLF-INLF01-V10 SECTION.                                          
299400     STRING 'W6INLF01(W6D1E1KY>=' W-W6D1E1KY-MIN-X                        
299500                    '&W6D1E1KY<=' W-W6D1E1KY-MAX-X                        
299600                    '&IDDC     =' W-IDDC-X                                
299700                    '&FLINLFP  =' W-FLINLFP                               
299800                    '&IDINLVGN =' W-IDINLVGN                              
299900                    '&KDINLSTA =' W-KDINLSTA ')'                          
300000          DELIMITED BY SIZE INTO SSA1                                     
300100     MOVE '  GBGE' TO GODK-STATUSKODER                                    
300200     CALL CBLTDLI USING GN INLF-PCB DLI-IO-AREA1 SSA1                     
300300     MOVE INLF-STATUS-CODE TO STATUS-WS                                   
300400     PERFORM IMS-STATUSKONTROLL                                           
300500     .                                                                    
300600     SKIP2                                                                
300700 IMS-GN-INLF-INLF01-V11 SECTION.                                          
300800     STRING 'W6INLF01(W6D1E1KY>=' W-W6D1E1KY-MIN-X                        
300900                    '&W6D1E1KY<=' W-W6D1E1KY-MAX-X                        
301000                    '&IDDC     =' W-IDDC-X                                
301100                    '&FLINLFP  =' W-FLINLFP                               
301200                    '&IDINLVGN >' W-IDINLVGN ')'                          
301300          DELIMITED BY SIZE INTO SSA1                                     
301400     MOVE '  GBGE' TO GODK-STATUSKODER                                    
301500     CALL CBLTDLI USING GN INLF-PCB DLI-IO-AREA1 SSA1                     
301600     MOVE INLF-STATUS-CODE TO STATUS-WS                                   
301700     PERFORM IMS-STATUSKONTROLL                                           
301800     .                                                                    
301900     SKIP2                                                                
302000 IMS-GN-INLF-INLF01-V12 SECTION.                                          
302100     STRING 'W6INLF01(W6D1E1KY>=' W-W6D1E1KY-MIN-X                        
302200                    '&W6D1E1KY<=' W-W6D1E1KY-MAX-X                        
302300                    '&IDDC     =' W-IDDC-X                                
302400                    '&FLINLFP  =' W-FLINLFP                               
302500                    '&IDINLVGN >' W-IDINLVGN                              
302600                    '&KDINLSTA =' W-KDINLSTA ')'                          
302700          DELIMITED BY SIZE INTO SSA1                                     
302800     MOVE '  GBGE' TO GODK-STATUSKODER                                    
302900     CALL CBLTDLI USING GN INLF-PCB DLI-IO-AREA1 SSA1                     
303000     MOVE INLF-STATUS-CODE TO STATUS-WS                                   
303100     PERFORM IMS-STATUSKONTROLL                                           
303200     .                                                                    
303300     SKIP2                                                                
303400 IMS-GN-INLF-INLF01-V13 SECTION.                                          
303500     STRING 'W6INLF01(W6D1E1KY>=' W-W6D1E1KY-MIN-X                        
303600                    '&W6D1E1KY<=' W-W6D1E1KY-MAX-X                        
303700                    '&IDDC     =' W-IDDC-X                                
303800                    '&FLINLFB  =' W-FLINLFB ')'                           
303900          DELIMITED BY SIZE INTO SSA1                                     
304000     MOVE '  GBGE' TO GODK-STATUSKODER                                    
304100     CALL CBLTDLI USING GN INLF-PCB DLI-IO-AREA1 SSA1                     
304200     MOVE INLF-STATUS-CODE TO STATUS-WS                                   
304300     PERFORM IMS-STATUSKONTROLL                                           
304400     .                                                                    
304500     SKIP2                                                                
304600 IMS-GN-INLF-INLF01-V14 SECTION.                                          
304700     STRING 'W6INLF01(W6D1E1KY>=' W-W6D1E1KY-MIN-X                        
304800                    '&W6D1E1KY<=' W-W6D1E1KY-MAX-X                        
304900                    '&IDDC     =' W-IDDC-X                                
305000                    '&FLINLFB  =' W-FLINLFB                               
305100                    '&KDINLSTA =' W-KDINLSTA ')'                          
305200          DELIMITED BY SIZE INTO SSA1                                     
305300     MOVE '  GBGE' TO GODK-STATUSKODER                                    
305400     CALL CBLTDLI USING GN INLF-PCB DLI-IO-AREA1 SSA1                     
305500     MOVE INLF-STATUS-CODE TO STATUS-WS                                   
305600     PERFORM IMS-STATUSKONTROLL                                           
305700     .                                                                    
305800     SKIP2                                                                
305900 IMS-GN-INLF-INLF01-V15 SECTION.                                          
306000     STRING 'W6INLF01(W6D1E1KY>=' W-W6D1E1KY-MIN-X                        
306100                    '&W6D1E1KY<=' W-W6D1E1KY-MAX-X                        
306200                    '&IDDC     =' W-IDDC-X                                
306300                    '&FLINLFB  =' W-FLINLFB                               
306400                    '&IDINLVGN =' W-IDINLVGN ')'                          
306500          DELIMITED BY SIZE INTO SSA1                                     
306600     MOVE '  GBGE' TO GODK-STATUSKODER                                    
306700     CALL CBLTDLI USING GN INLF-PCB DLI-IO-AREA1 SSA1                     
306800     MOVE INLF-STATUS-CODE TO STATUS-WS                                   
306900     PERFORM IMS-STATUSKONTROLL                                           
307000     .                                                                    
307100     SKIP2                                                                
307200 IMS-GN-INLF-INLF01-V16 SECTION.                                          
307300     STRING 'W6INLF01(W6D1E1KY>=' W-W6D1E1KY-MIN-X                        
307400                    '&W6D1E1KY<=' W-W6D1E1KY-MAX-X                        
307500                    '&IDDC     =' W-IDDC-X                                
307600                    '&FLINLFB  =' W-FLINLFB                               
307700                    '&IDINLVGN =' W-IDINLVGN                              
307800                    '&KDINLSTA =' W-KDINLSTA ')'                          
307900          DELIMITED BY SIZE INTO SSA1                                     
308000     MOVE '  GBGE' TO GODK-STATUSKODER                                    
308100     CALL CBLTDLI USING GN INLF-PCB DLI-IO-AREA1 SSA1                     
308200     MOVE INLF-STATUS-CODE TO STATUS-WS                                   
308300     PERFORM IMS-STATUSKONTROLL                                           
308400     .                                                                    
308500     SKIP2                                                                
308600 IMS-GN-INLF-INLF01-V17 SECTION.                                          
308700     STRING 'W6INLF01(W6D1E1KY>=' W-W6D1E1KY-MIN-X                        
308800                    '&W6D1E1KY<=' W-W6D1E1KY-MAX-X                        
308900                    '&IDDC     =' W-IDDC-X                                
309000                    '&FLINLFB  =' W-FLINLFB                               
309100                    '&IDINLVGN >' W-IDINLVGN ')'                          
309200          DELIMITED BY SIZE INTO SSA1                                     
309300     MOVE '  GBGE' TO GODK-STATUSKODER                                    
309400     CALL CBLTDLI USING GN INLF-PCB DLI-IO-AREA1 SSA1                     
309500     MOVE INLF-STATUS-CODE TO STATUS-WS                                   
309600     PERFORM IMS-STATUSKONTROLL                                           
309700     .                                                                    
309800     SKIP2                                                                
309900 IMS-GN-INLF-INLF01-V18 SECTION.                                          
310000     STRING 'W6INLF01(W6D1E1KY>=' W-W6D1E1KY-MIN-X                        
310100                    '&W6D1E1KY<=' W-W6D1E1KY-MAX-X                        
310200                    '&IDDC     =' W-IDDC-X                                
310300                    '&FLINLFB  =' W-FLINLFB                               
310400                    '&IDINLVGN >' W-IDINLVGN                              
310500                    '&KDINLSTA =' W-KDINLSTA ')'                          
310600          DELIMITED BY SIZE INTO SSA1                                     
310700     MOVE '  GBGE' TO GODK-STATUSKODER                                    
310800     CALL CBLTDLI USING GN INLF-PCB DLI-IO-AREA1 SSA1                     
310900     MOVE INLF-STATUS-CODE TO STATUS-WS                                   
311000     PERFORM IMS-STATUSKONTROLL                                           
311100     .                                                                    
311200     SKIP2                                                                
311300 IMS-GN-INLF-INLF01-V19 SECTION.                                          
311400     STRING 'W6INLF01(W6D1E1KY>=' W-W6D1E1KY-MIN-X                        
311500                    '&W6D1E1KY<=' W-W6D1E1KY-MAX-X                        
311600                    '&IDDC     =' W-IDDC-X                                
311700                    '&FLINLFB  =' W-FLINLFB                               
311800                    '&FLINLFP  =' W-FLINLFP  ')'                          
311900          DELIMITED BY SIZE INTO SSA1                                     
312000     MOVE '  GBGE' TO GODK-STATUSKODER                                    
312100     CALL CBLTDLI USING GN INLF-PCB DLI-IO-AREA1 SSA1                     
312200     MOVE INLF-STATUS-CODE TO STATUS-WS                                   
312300     PERFORM IMS-STATUSKONTROLL                                           
312400     .                                                                    
312500     SKIP2                                                                
312600 IMS-GN-INLF-INLF01-V20 SECTION.                                          
312700     STRING 'W6INLF01(W6D1E1KY>=' W-W6D1E1KY-MIN-X                        
312800                    '&W6D1E1KY<=' W-W6D1E1KY-MAX-X                        
312900                    '&IDDC     =' W-IDDC-X                                
313000                    '&FLINLFB  =' W-FLINLFB                               
313100                    '&FLINLFP  =' W-FLINLFP                               
313200                    '&KDINLSTA =' W-KDINLSTA ')'                          
313300          DELIMITED BY SIZE INTO SSA1                                     
313400     MOVE '  GBGE' TO GODK-STATUSKODER                                    
313500     CALL CBLTDLI USING GN INLF-PCB DLI-IO-AREA1 SSA1                     
313600     MOVE INLF-STATUS-CODE TO STATUS-WS                                   
313700     PERFORM IMS-STATUSKONTROLL                                           
313800     .                                                                    
313900     SKIP2                                                                
314000 IMS-GN-INLF-INLF01-V21 SECTION.                                          
314100     STRING 'W6INLF01(W6D1E1KY>=' W-W6D1E1KY-MIN-X                        
314200                    '&W6D1E1KY<=' W-W6D1E1KY-MAX-X                        
314300                    '&IDDC     =' W-IDDC-X                                
314400                    '&FLINLFB  =' W-FLINLFB                               
314500                    '&FLINLFP  =' W-FLINLFP                               
314600                    '&IDINLVGN =' W-IDINLVGN ')'                          
314700          DELIMITED BY SIZE INTO SSA1                                     
314800     MOVE '  GBGE' TO GODK-STATUSKODER                                    
314900     CALL CBLTDLI USING GN INLF-PCB DLI-IO-AREA1 SSA1                     
315000     MOVE INLF-STATUS-CODE TO STATUS-WS                                   
315100     PERFORM IMS-STATUSKONTROLL                                           
315200     .                                                                    
315300     SKIP2                                                                
315400 IMS-GN-INLF-INLF01-V22 SECTION.                                          
315500     STRING 'W6INLF01(W6D1E1KY>=' W-W6D1E1KY-MIN-X                        
315600                    '&W6D1E1KY<=' W-W6D1E1KY-MAX-X                        
315700                    '&IDDC     =' W-IDDC-X                                
315800                    '&FLINLFB  =' W-FLINLFB                               
315900                    '&FLINLFP  =' W-FLINLFP                               
316000                    '&IDINLVGN =' W-IDINLVGN                              
316100                    '&KDINLSTA =' W-KDINLSTA ')'                          
316200          DELIMITED BY SIZE INTO SSA1                                     
316300     MOVE '  GBGE' TO GODK-STATUSKODER                                    
316400     CALL CBLTDLI USING GN INLF-PCB DLI-IO-AREA1 SSA1                     
316500     MOVE INLF-STATUS-CODE TO STATUS-WS                                   
316600     PERFORM IMS-STATUSKONTROLL                                           
316700     .                                                                    
316800     SKIP2                                                                
316900 IMS-GN-INLF-INLF01-V23 SECTION.                                          
317000     STRING 'W6INLF01(W6D1E1KY>=' W-W6D1E1KY-MIN-X                        
317100                    '&W6D1E1KY<=' W-W6D1E1KY-MAX-X                        
317200                    '&IDDC     =' W-IDDC-X                                
317300                    '&FLINLFB  =' W-FLINLFB                               
317400                    '&FLINLFP  =' W-FLINLFP                               
317500                    '&IDINLVGN >' W-IDINLVGN ')'                          
317600          DELIMITED BY SIZE INTO SSA1                                     
317700     MOVE '  GBGE' TO GODK-STATUSKODER                                    
317800     CALL CBLTDLI USING GN INLF-PCB DLI-IO-AREA1 SSA1                     
317900     MOVE INLF-STATUS-CODE TO STATUS-WS                                   
318000     PERFORM IMS-STATUSKONTROLL                                           
318100     .                                                                    
318200     SKIP2                                                                
318300 IMS-GN-INLF-INLF01-V24 SECTION.                                          
318400     STRING 'W6INLF01(W6D1E1KY>=' W-W6D1E1KY-MIN-X                        
318500                    '&W6D1E1KY<=' W-W6D1E1KY-MAX-X                        
318600                    '&IDDC     =' W-IDDC-X                                
318700                    '&FLINLFB  =' W-FLINLFB                               
318800                    '&FLINLFP  =' W-FLINLFP                               
318900                    '&IDINLVGN >' W-IDINLVGN                              
319000                    '&KDINLSTA =' W-KDINLSTA ')'                          
319100          DELIMITED BY SIZE INTO SSA1                                     
319200     MOVE '  GBGE' TO GODK-STATUSKODER                                    
319300     CALL CBLTDLI USING GN INLF-PCB DLI-IO-AREA1 SSA1                     
319400     MOVE INLF-STATUS-CODE TO STATUS-WS                                   
319500     PERFORM IMS-STATUSKONTROLL                                           
319600     .                                                                    
319700     SKIP2                                                                
319800     SKIP2                                                                
319900 IMS-GN-INLH-INLH01-V01 SECTION.                                          
320000     STRING 'W6INLH01(W6D1G1KY>=' W-W6D1G1KY-MIN-X                        
320100                    '&W6D1G1KY<=' W-W6D1G1KY-MAX-X                        
320200                    '&IDDC     =' W-IDDC-X ')'                            
320300          DELIMITED BY SIZE INTO SSA1                                     
320400     MOVE '  GBGE' TO GODK-STATUSKODER                                    
320500     CALL CBLTDLI USING GN INLH-PCB DLI-IO-AREA4 SSA1                     
320600     MOVE INLH-STATUS-CODE TO STATUS-WS                                   
320700     PERFORM IMS-STATUSKONTROLL                                           
320800     .                                                                    
320900     SKIP2                                                                
321000 IMS-GN-INLH-INLH01-V02 SECTION.                                          
321100     STRING 'W6INLH01(W6D1G1KY>=' W-W6D1G1KY-MIN-X                        
321200                    '&W6D1G1KY<=' W-W6D1G1KY-MAX-X                        
321300                    '&IDDC     =' W-IDDC-X                                
321400                    '&KDINLSTA =' W-KDINLSTA ')'                          
321500          DELIMITED BY SIZE INTO SSA1                                     
321600     MOVE '  GBGE' TO GODK-STATUSKODER                                    
321700     CALL CBLTDLI USING GN INLH-PCB DLI-IO-AREA4 SSA1                     
321800     MOVE INLH-STATUS-CODE TO STATUS-WS                                   
321900     PERFORM IMS-STATUSKONTROLL                                           
322000     .                                                                    
322100     SKIP2                                                                
322200 IMS-GN-INLH-INLH01-V03 SECTION.                                          
322300     STRING 'W6INLH01(W6D1G1KY>=' W-W6D1G1KY-MIN-X                        
322400                    '&W6D1G1KY<=' W-W6D1G1KY-MAX-X                        
322500                    '&IDDC     =' W-IDDC-X                                
322600                    '&IDINLVGN =' W-IDINLVGN ')'                          
322700          DELIMITED BY SIZE INTO SSA1                                     
322800     MOVE '  GBGE' TO GODK-STATUSKODER                                    
322900     CALL CBLTDLI USING GN INLH-PCB DLI-IO-AREA4 SSA1                     
323000     MOVE INLH-STATUS-CODE TO STATUS-WS                                   
323100     PERFORM IMS-STATUSKONTROLL                                           
323200     .                                                                    
323300     SKIP2                                                                
323400 IMS-GN-INLH-INLH01-V04 SECTION.                                          
323500     STRING 'W6INLH01(W6D1G1KY>=' W-W6D1G1KY-MIN-X                        
323600                    '&W6D1G1KY<=' W-W6D1G1KY-MAX-X                        
323700                    '&IDDC     =' W-IDDC-X                                
323800                    '&IDINLVGN =' W-IDINLVGN                              
323900                    '&KDINLSTA =' W-KDINLSTA ')'                          
324000          DELIMITED BY SIZE INTO SSA1                                     
324100     MOVE '  GBGE' TO GODK-STATUSKODER                                    
324200     CALL CBLTDLI USING GN INLH-PCB DLI-IO-AREA4 SSA1                     
324300     MOVE INLH-STATUS-CODE TO STATUS-WS                                   
324400     PERFORM IMS-STATUSKONTROLL                                           
324500     .                                                                    
324600     SKIP2                                                                
324700 IMS-GN-INLH-INLH01-V05 SECTION.                                          
324800     STRING 'W6INLH01(W6D1G1KY>=' W-W6D1G1KY-MIN-X                        
324900                    '&W6D1G1KY<=' W-W6D1G1KY-MAX-X                        
325000                    '&IDDC     =' W-IDDC-X                                
325100                    '&IDINLVGN >' W-IDINLVGN ')'                          
325200          DELIMITED BY SIZE INTO SSA1                                     
325300     MOVE '  GBGE' TO GODK-STATUSKODER                                    
325400     CALL CBLTDLI USING GN INLH-PCB DLI-IO-AREA4 SSA1                     
325500     MOVE INLH-STATUS-CODE TO STATUS-WS                                   
325600     PERFORM IMS-STATUSKONTROLL                                           
325700     .                                                                    
325800     SKIP2                                                                
325900 IMS-GN-INLH-INLH01-V06 SECTION.                                          
326000     STRING 'W6INLH01(W6D1G1KY>=' W-W6D1G1KY-MIN-X                        
326100                    '&W6D1G1KY<=' W-W6D1G1KY-MAX-X                        
326200                    '&IDDC     =' W-IDDC-X                                
326300                    '&IDINLVGN >' W-IDINLVGN                              
326400                    '&KDINLSTA =' W-KDINLSTA ')'                          
326500          DELIMITED BY SIZE INTO SSA1                                     
326600     MOVE '  GBGE' TO GODK-STATUSKODER                                    
326700     CALL CBLTDLI USING GN INLH-PCB DLI-IO-AREA4 SSA1                     
326800     MOVE INLH-STATUS-CODE TO STATUS-WS                                   
326900     PERFORM IMS-STATUSKONTROLL                                           
327000     .                                                                    
327100     SKIP2                                                                
327200 IMS-GN-INLH-INLH01-V07 SECTION.                                          
327300     STRING 'W6INLH01(W6D1G1KY>=' W-W6D1G1KY-MIN-X                        
327400                    '&W6D1G1KY<=' W-W6D1G1KY-MAX-X                        
327500                    '&IDDC     =' W-IDDC-X                                
327600                    '&FLINLFP  =' W-FLINLFP ')'                           
327700          DELIMITED BY SIZE INTO SSA1                                     
327800     MOVE '  GBGE' TO GODK-STATUSKODER                                    
327900     CALL CBLTDLI USING GN INLH-PCB DLI-IO-AREA4 SSA1                     
328000     MOVE INLH-STATUS-CODE TO STATUS-WS                                   
328100     PERFORM IMS-STATUSKONTROLL                                           
328200     .                                                                    
328300     SKIP2                                                                
328400 IMS-GN-INLH-INLH01-V08 SECTION.                                          
328500     STRING 'W6INLH01(W6D1G1KY>=' W-W6D1G1KY-MIN-X                        
328600                    '&W6D1G1KY<=' W-W6D1G1KY-MAX-X                        
328700                    '&IDDC     =' W-IDDC-X                                
328800                    '&FLINLFP  =' W-FLINLFP                               
328900                    '&KDINLSTA =' W-KDINLSTA ')'                          
329000          DELIMITED BY SIZE INTO SSA1                                     
329100     MOVE '  GBGE' TO GODK-STATUSKODER                                    
329200     CALL CBLTDLI USING GN INLH-PCB DLI-IO-AREA4 SSA1                     
329300     MOVE INLH-STATUS-CODE TO STATUS-WS                                   
329400     PERFORM IMS-STATUSKONTROLL                                           
329500     .                                                                    
329600     SKIP2                                                                
329700 IMS-GN-INLH-INLH01-V09 SECTION.                                          
329800     STRING 'W6INLH01(W6D1G1KY>=' W-W6D1G1KY-MIN-X                        
329900                    '&W6D1G1KY<=' W-W6D1G1KY-MAX-X                        
330000                    '&IDDC     =' W-IDDC-X                                
330100                    '&FLINLFP  =' W-FLINLFP                               
330200                    '&IDINLVGN =' W-IDINLVGN ')'                          
330300          DELIMITED BY SIZE INTO SSA1                                     
330400     MOVE '  GBGE' TO GODK-STATUSKODER                                    
330500     CALL CBLTDLI USING GN INLH-PCB DLI-IO-AREA4 SSA1                     
330600     MOVE INLH-STATUS-CODE TO STATUS-WS                                   
330700     PERFORM IMS-STATUSKONTROLL                                           
330800     .                                                                    
330900     SKIP2                                                                
331000 IMS-GN-INLH-INLH01-V10 SECTION.                                          
331100     STRING 'W6INLH01(W6D1G1KY>=' W-W6D1G1KY-MIN-X                        
331200                    '&W6D1G1KY<=' W-W6D1G1KY-MAX-X                        
331300                    '&IDDC     =' W-IDDC-X                                
331400                    '&FLINLFP  =' W-FLINLFP                               
331500                    '&IDINLVGN =' W-IDINLVGN                              
331600                    '&KDINLSTA =' W-KDINLSTA ')'                          
331700          DELIMITED BY SIZE INTO SSA1                                     
331800     MOVE '  GBGE' TO GODK-STATUSKODER                                    
331900     CALL CBLTDLI USING GN INLH-PCB DLI-IO-AREA4 SSA1                     
332000     MOVE INLH-STATUS-CODE TO STATUS-WS                                   
332100     PERFORM IMS-STATUSKONTROLL                                           
332200     .                                                                    
332300     SKIP2                                                                
332400 IMS-GN-INLH-INLH01-V11 SECTION.                                          
332500     STRING 'W6INLH01(W6D1G1KY>=' W-W6D1G1KY-MIN-X                        
332600                    '&W6D1G1KY<=' W-W6D1G1KY-MAX-X                        
332700                    '&IDDC     =' W-IDDC-X                                
332800                    '&FLINLFP  =' W-FLINLFP                               
332900                    '&IDINLVGN >' W-IDINLVGN ')'                          
333000          DELIMITED BY SIZE INTO SSA1                                     
333100     MOVE '  GBGE' TO GODK-STATUSKODER                                    
333200     CALL CBLTDLI USING GN INLH-PCB DLI-IO-AREA4 SSA1                     
333300     MOVE INLH-STATUS-CODE TO STATUS-WS                                   
333400     PERFORM IMS-STATUSKONTROLL                                           
333500     .                                                                    
333600     SKIP2                                                                
333700 IMS-GN-INLH-INLH01-V12 SECTION.                                          
333800     STRING 'W6INLH01(W6D1G1KY>=' W-W6D1G1KY-MIN-X                        
333900                    '&W6D1G1KY<=' W-W6D1G1KY-MAX-X                        
334000                    '&IDDC     =' W-IDDC-X                                
334100                    '&FLINLFP  =' W-FLINLFP                               
334200                    '&IDINLVGN >' W-IDINLVGN                              
334300                    '&KDINLSTA =' W-KDINLSTA ')'                          
334400          DELIMITED BY SIZE INTO SSA1                                     
334500     MOVE '  GBGE' TO GODK-STATUSKODER                                    
334600     CALL CBLTDLI USING GN INLH-PCB DLI-IO-AREA4 SSA1                     
334700     MOVE INLH-STATUS-CODE TO STATUS-WS                                   
334800     PERFORM IMS-STATUSKONTROLL                                           
334900     .                                                                    
335000     SKIP2                                                                
335100 IMS-GN-INLH-INLH01-V13 SECTION.                                          
335200     STRING 'W6INLH01(W6D1G1KY>=' W-W6D1G1KY-MIN-X                        
335300                    '&W6D1G1KY<=' W-W6D1G1KY-MAX-X                        
335400                    '&IDDC     =' W-IDDC-X                                
335500                    '&FLINLFB  =' W-FLINLFB ')'                           
335600          DELIMITED BY SIZE INTO SSA1                                     
335700     MOVE '  GBGE' TO GODK-STATUSKODER                                    
335800     CALL CBLTDLI USING GN INLH-PCB DLI-IO-AREA4 SSA1                     
335900     MOVE INLH-STATUS-CODE TO STATUS-WS                                   
336000     PERFORM IMS-STATUSKONTROLL                                           
336100     .                                                                    
336200     SKIP2                                                                
336300 IMS-GN-INLH-INLH01-V14 SECTION.                                          
336400     STRING 'W6INLH01(W6D1G1KY>=' W-W6D1G1KY-MIN-X                        
336500                    '&W6D1G1KY<=' W-W6D1G1KY-MAX-X                        
336600                    '&IDDC     =' W-IDDC-X                                
336700                    '&FLINLFB  =' W-FLINLFB                               
336800                    '&KDINLSTA =' W-KDINLSTA ')'                          
336900          DELIMITED BY SIZE INTO SSA1                                     
337000     MOVE '  GBGE' TO GODK-STATUSKODER                                    
337100     CALL CBLTDLI USING GN INLH-PCB DLI-IO-AREA4 SSA1                     
337200     MOVE INLH-STATUS-CODE TO STATUS-WS                                   
337300     PERFORM IMS-STATUSKONTROLL                                           
337400     .                                                                    
337500     SKIP2                                                                
337600 IMS-GN-INLH-INLH01-V15 SECTION.                                          
337700     STRING 'W6INLH01(W6D1G1KY>=' W-W6D1G1KY-MIN-X                        
337800                    '&W6D1G1KY<=' W-W6D1G1KY-MAX-X                        
337900                    '&IDDC     =' W-IDDC-X                                
338000                    '&FLINLFB  =' W-FLINLFB                               
338100                    '&IDINLVGN =' W-IDINLVGN ')'                          
338200          DELIMITED BY SIZE INTO SSA1                                     
338300     MOVE '  GBGE' TO GODK-STATUSKODER                                    
338400     CALL CBLTDLI USING GN INLH-PCB DLI-IO-AREA4 SSA1                     
338500     MOVE INLH-STATUS-CODE TO STATUS-WS                                   
338600     PERFORM IMS-STATUSKONTROLL                                           
338700     .                                                                    
338800     SKIP2                                                                
338900 IMS-GN-INLH-INLH01-V16 SECTION.                                          
339000     STRING 'W6INLH01(W6D1G1KY>=' W-W6D1G1KY-MIN-X                        
339100                    '&W6D1G1KY<=' W-W6D1G1KY-MAX-X                        
339200                    '&IDDC     =' W-IDDC-X                                
339300                    '&FLINLFB  =' W-FLINLFB                               
339400                    '&IDINLVGN =' W-IDINLVGN                              
339500                    '&KDINLSTA =' W-KDINLSTA ')'                          
339600          DELIMITED BY SIZE INTO SSA1                                     
339700     MOVE '  GBGE' TO GODK-STATUSKODER                                    
339800     CALL CBLTDLI USING GN INLH-PCB DLI-IO-AREA4 SSA1                     
339900     MOVE INLH-STATUS-CODE TO STATUS-WS                                   
340000     PERFORM IMS-STATUSKONTROLL                                           
340100     .                                                                    
340200     SKIP2                                                                
340300 IMS-GN-INLH-INLH01-V17 SECTION.                                          
340400     STRING 'W6INLH01(W6D1G1KY>=' W-W6D1G1KY-MIN-X                        
340500                    '&W6D1G1KY<=' W-W6D1G1KY-MAX-X                        
340600                    '&IDDC     =' W-IDDC-X                                
340700                    '&FLINLFB  =' W-FLINLFB                               
340800                    '&IDINLVGN >' W-IDINLVGN ')'                          
340900          DELIMITED BY SIZE INTO SSA1                                     
341000     MOVE '  GBGE' TO GODK-STATUSKODER                                    
341100     CALL CBLTDLI USING GN INLH-PCB DLI-IO-AREA4 SSA1                     
341200     MOVE INLH-STATUS-CODE TO STATUS-WS                                   
341300     PERFORM IMS-STATUSKONTROLL                                           
341400     .                                                                    
341500     SKIP2                                                                
341600 IMS-GN-INLH-INLH01-V18 SECTION.                                          
341700     STRING 'W6INLH01(W6D1G1KY>=' W-W6D1G1KY-MIN-X                        
341800                    '&W6D1G1KY<=' W-W6D1G1KY-MAX-X                        
341900                    '&IDDC     =' W-IDDC-X                                
342000                    '&FLINLFB  =' W-FLINLFB                               
342100                    '&IDINLVGN >' W-IDINLVGN                              
342200                    '&KDINLSTA =' W-KDINLSTA ')'                          
342300          DELIMITED BY SIZE INTO SSA1                                     
342400     MOVE '  GBGE' TO GODK-STATUSKODER                                    
342500     CALL CBLTDLI USING GN INLH-PCB DLI-IO-AREA4 SSA1                     
342600     MOVE INLH-STATUS-CODE TO STATUS-WS                                   
342700     PERFORM IMS-STATUSKONTROLL                                           
342800     .                                                                    
342900     SKIP2                                                                
343000 IMS-GN-INLH-INLH01-V19 SECTION.                                          
343100     STRING 'W6INLH01(W6D1G1KY>=' W-W6D1G1KY-MIN-X                        
343200                    '&W6D1G1KY<=' W-W6D1G1KY-MAX-X                        
343300                    '&IDDC     =' W-IDDC-X                                
343400                    '&FLINLFB  =' W-FLINLFB                               
343500                    '&FLINLFP  =' W-FLINLFP  ')'                          
343600          DELIMITED BY SIZE INTO SSA1                                     
343700     MOVE '  GBGE' TO GODK-STATUSKODER                                    
343800     CALL CBLTDLI USING GN INLH-PCB DLI-IO-AREA4 SSA1                     
343900     MOVE INLH-STATUS-CODE TO STATUS-WS                                   
344000     PERFORM IMS-STATUSKONTROLL                                           
344100     .                                                                    
344200     SKIP2                                                                
344300 IMS-GN-INLH-INLH01-V20 SECTION.                                          
344400     STRING 'W6INLH01(W6D1G1KY>=' W-W6D1G1KY-MIN-X                        
344500                    '&W6D1G1KY<=' W-W6D1G1KY-MAX-X                        
344600                    '&IDDC     =' W-IDDC-X                                
344700                    '&FLINLFB  =' W-FLINLFB                               
344800                    '&FLINLFP  =' W-FLINLFP                               
344900                    '&KDINLSTA =' W-KDINLSTA ')'                          
345000          DELIMITED BY SIZE INTO SSA1                                     
345100     MOVE '  GBGE' TO GODK-STATUSKODER                                    
345200     CALL CBLTDLI USING GN INLH-PCB DLI-IO-AREA4 SSA1                     
345300     MOVE INLH-STATUS-CODE TO STATUS-WS                                   
345400     PERFORM IMS-STATUSKONTROLL                                           
345500     .                                                                    
345600     SKIP2                                                                
345700 IMS-GN-INLH-INLH01-V21 SECTION.                                          
345800     STRING 'W6INLH01(W6D1G1KY>=' W-W6D1G1KY-MIN-X                        
345900                    '&W6D1G1KY<=' W-W6D1G1KY-MAX-X                        
346000                    '&IDDC     =' W-IDDC-X                                
346100                    '&FLINLFB  =' W-FLINLFB                               
346200                    '&FLINLFP  =' W-FLINLFP                               
346300                    '&IDINLVGN =' W-IDINLVGN ')'                          
346400          DELIMITED BY SIZE INTO SSA1                                     
346500     MOVE '  GBGE' TO GODK-STATUSKODER                                    
346600     CALL CBLTDLI USING GN INLH-PCB DLI-IO-AREA4 SSA1                     
346700     MOVE INLH-STATUS-CODE TO STATUS-WS                                   
346800     PERFORM IMS-STATUSKONTROLL                                           
346900     .                                                                    
347000     SKIP2                                                                
347100 IMS-GN-INLH-INLH01-V22 SECTION.                                          
347200     STRING 'W6INLH01(W6D1G1KY>=' W-W6D1G1KY-MIN-X                        
347300                    '&W6D1G1KY<=' W-W6D1G1KY-MAX-X                        
347400                    '&IDDC     =' W-IDDC-X                                
347500                    '&FLINLFB  =' W-FLINLFB                               
347600                    '&FLINLFP  =' W-FLINLFP                               
347700                    '&IDINLVGN =' W-IDINLVGN                              
347800                    '&KDINLSTA =' W-KDINLSTA ')'                          
347900          DELIMITED BY SIZE INTO SSA1                                     
348000     MOVE '  GBGE' TO GODK-STATUSKODER                                    
348100     CALL CBLTDLI USING GN INLH-PCB DLI-IO-AREA4 SSA1                     
348200     MOVE INLH-STATUS-CODE TO STATUS-WS                                   
348300     PERFORM IMS-STATUSKONTROLL                                           
348400     .                                                                    
348500     SKIP2                                                                
348600 IMS-GN-INLH-INLH01-V23 SECTION.                                          
348700     STRING 'W6INLH01(W6D1G1KY>=' W-W6D1G1KY-MIN-X                        
348800                    '&W6D1G1KY<=' W-W6D1G1KY-MAX-X                        
348900                    '&IDDC     =' W-IDDC-X                                
349000                    '&FLINLFB  =' W-FLINLFB                               
349100                    '&FLINLFP  =' W-FLINLFP                               
349200                    '&IDINLVGN >' W-IDINLVGN ')'                          
349300          DELIMITED BY SIZE INTO SSA1                                     
349400     MOVE '  GBGE' TO GODK-STATUSKODER                                    
349500     CALL CBLTDLI USING GN INLH-PCB DLI-IO-AREA4 SSA1                     
349600     MOVE INLH-STATUS-CODE TO STATUS-WS                                   
349700     PERFORM IMS-STATUSKONTROLL                                           
349800     .                                                                    
349900     SKIP2                                                                
350000 IMS-GN-INLH-INLH01-V24 SECTION.                                          
350100     STRING 'W6INLH01(W6D1G1KY>=' W-W6D1G1KY-MIN-X                        
350200                    '&W6D1G1KY<=' W-W6D1G1KY-MAX-X                        
350300                    '&IDDC     =' W-IDDC-X                                
350400                    '&FLINLFB  =' W-FLINLFB                               
350500                    '&FLINLFP  =' W-FLINLFP                               
350600                    '&IDINLVGN >' W-IDINLVGN                              
350700                    '&KDINLSTA =' W-KDINLSTA ')'                          
350800          DELIMITED BY SIZE INTO SSA1                                     
350900     MOVE '  GBGE' TO GODK-STATUSKODER                                    
351000     CALL CBLTDLI USING GN INLH-PCB DLI-IO-AREA4 SSA1                     
351100     MOVE INLH-STATUS-CODE TO STATUS-WS                                   
351200     PERFORM IMS-STATUSKONTROLL                                           
351300     .                                                                    
351400     SKIP2                                                                
351500 IMS-GU-WDK611 SECTION.                                                   
351600     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
351700          DELIMITED BY SIZE INTO SSA1                                     
351800     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY ')'                          
351900          DELIMITED BY SIZE INTO SSA2                                     
352000     MOVE '  GE' TO GODK-STATUSKODER                                      
352100     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-AREA-WDK6 SSA1 SSA2            
352200     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
352300     PERFORM IMS-STATUSKONTROLL                                           
352400     .                                                                    
352500     SKIP2                                                                
352600 IMS-GU-WDK711 SECTION.                                                   
352700     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
352800          DELIMITED BY SIZE INTO SSA1                                     
352900     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
353000          DELIMITED BY SIZE INTO SSA2                                     
353100     MOVE '  GE' TO GODK-STATUSKODER                                      
353200     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-AREA-WDK7 SSA1 SSA2            
353300     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
353400     PERFORM IMS-STATUSKONTROLL                                           
353500     .                                                                    
353600     EJECT                                                                
353700 IMS-GU-WDB601    SECTION.                                                
353800     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
353900          DELIMITED BY SIZE INTO SSA1                                     
354000     MOVE '  GE' TO GODK-STATUSKODER                                      
354100     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
354200     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
354300     PERFORM IMS-STATUSKONTROLL                                           
354400     IF SEGMENT-SAKNAS                                                    
354500         MOVE SPACE TO DCS-KDDC                                           
354600     END-IF                                                               
354700     .                                                                    
354800 IMS-GU-WDD311 SECTION.                                                   
354900     STRING 'WDD301  (WDD3BSEQ =' W-IDARTNR-X ')'                         
355000             DELIMITED BY SIZE INTO SSA1                                  
355100     STRING 'WDD311  (IDSKYLT  =' W-IDSKYLT-X ')'                         
355200              DELIMITED BY SIZE INTO SSA2                                 
355300     MOVE '  GE' TO GODK-STATUSKODER                                      
355400     CALL CBLTDLI USING GU WDD3-PCB DLI-IO-WDD311 SSA1 SSA2               
355500     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
355600     PERFORM IMS-STATUSKONTROLL                                           
355700     .                                                                    
355800     EJECT                                                                
355900 IMS-STATUSKONTROLL SECTION.                                              
356000                                                                          
356100     SET STATUS-IX TO 1                                                   
356200     SEARCH GODK-STATUS                                                   
356300       AT END                                                             
356400         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
356500         DELIMITED BY SIZE INTO FELTEXT                                   
356600         CALL FELLOG                                                      
356700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
356800         CONTINUE                                                         
356900     END-SEARCH                                                           
357000     .                                                                    
