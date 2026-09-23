000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W6110100.                                                
000400*AUTHOR.         LARS THELL.                                              
000500*DATE-WRITTEN.   92/02/05.                                                
000600*                                                                         
000700*                                                                         
000800*                                                                         
000900*    REMARKS.                                                             
001000*                                                                         
001100*    FUNKTION:                                                            
001200*        PROGRAMMET TAR EMOT EN ODETTEFIL OCH LÄGGER UPP AVISERING        
001300*        PÅ INLEVERANSREGISTRET.                                          
001400*                                                                         
001500*        PROGRAMMET            LÄSER      WDK7                            
001600*        PROGRAMMET            LÄSER      WDB6                            
001700*        PROGRAMMET            LÄSER      WLARTC (WDK6)                   
001800*        PROGRAMMET            LÄSER      WLBENA (WDD3)                   
001900*        PROGRAMMET            LÄSER      WLLEVA (WDF1)                   
002000*        PROGRAMMET            LÄSER      WLINLB (WDD9)                   
002100*        PROGRAMMET            UPPDATERAR W6INLA (W6D1)                   
002200*        PROGRAMMET            UPPDATERAR W6CKPA (W6G2)                   
002300*    SUB PROGRAMMET W611LEVP   UPPDATERAR WLINLB (WDD9)                   
002400*                                                                         
002500*    ABENDKODER:                                                          
002600*        U0016 -  . . . .                                                 
002700*        U1000 -  . . . .                                                 
002800*                                                                         
002900                                                                          
003000                                                                          
003100     SKIP3                                                                
003200 ENVIRONMENT DIVISION.                                                    
003300     SKIP2                                                                
003400 INPUT-OUTPUT SECTION.                                                    
003500                                                                          
003600 FILE-CONTROL.                                                            
003700     SKIP2                                                                
003800*          --- ODETTE FIL                                                 
003900     SELECT W61101                     ASSIGN TO W61101D1.                
004000     SELECT W61101UT                   ASSIGN TO W61101D2.                
004100     EJECT                                                                
004200 DATA DIVISION.                                                           
004300     SKIP3                                                                
004400 FILE SECTION.                                                            
004500     SKIP3                                                                
004600 FD  W61101                                                               
004700     RECORDING       F                                                    
004800     BLOCK CONTAINS  0.                                                   
004900                                                                          
005000 01  INPOST.                                                              
005100   03  POSTTYP                   PIC X(3).                                
005200   03  FILLER                    PIC X(1006).                             
005300                                                                          
005400 FD  W61101UT                                                             
005500     RECORDING       V                                                    
005600     BLOCK CONTAINS  0.                                                   
005700                                                                          
005800*01  POST -COPY W161TEXT  -PRE  UT-  -L.                                  
005900                                                                          
006000     EJECT                                                                
006100 WORKING-STORAGE SECTION.                                                 
006200     SKIP2                                                                
006300 77  IDPGM                       PIC X(8)    VALUE 'W6110100'.            
006400 01  FELTEXT.                                                             
006500     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
006600     03  FELTEXT-STR             PIC X(72)  VALUE SPACE.                  
006700                                                                          
006800 01  WS-ADRESS-FLS               PIC X(50)                                
006900                        VALUE 'APIOUT.FLS.GCERRORNOTIFY'.                 
007000 01  CHKP-VAR.                                                            
007100  03 CHKP-MSG-IO-AREA-LENGTH     PIC S9(9)   VALUE +32 COMP SYNC.         
007200  03 CHKP-MSG-IO-AREA            PIC X(32)   VALUE SPACE.                 
007300  03 CHKP-AREA-LENGTH            PIC S9(9)   VALUE +32 COMP SYNC.         
007400  03 CHKP-AREA                   PIC X(32)   VALUE SPACE.                 
007500  03 CHKP-ANT                    PIC S9(3)   VALUE +0.                    
007600  03 CHKP-MAX                    PIC S9(3)   VALUE +100.                  
007700 77  JA                          PIC X       VALUE 'J'.                   
007800 77  YES                         PIC X       VALUE 'Y'.                   
007900 77  NEJ                         PIC X       VALUE 'N'.                   
008000 77  W-IDRADNR-INL-ACC           PIC S9(5)   VALUE ZERO.                  
008100 77  W-CL1-IDARTNR-EMBQ3         PIC S9(9)   VALUE ZERO COMP-3.           
008200 77  W-CL1-ART-EMBQ3             PIC 9(9).                                
008300 77  W-ANTAL-KOLLI               PIC S9(5)   VALUE ZERO COMP-3.           
008400 77  W-IX                        PIC S9(5)   VALUE ZERO COMP-3.           
008500 77  NOLL-IX-IN                  PIC S9(3)   VALUE ZERO COMP-3.           
008600 77  NOLL-IX-UT                  PIC S9(3)   VALUE ZERO COMP-3.           
008700 77  FIX-IX                      PIC S9(3)   VALUE ZERO COMP-3.           
008800 77  FIX-TAB-MAX                 PIC S9(3)   VALUE +100.                  
008900 77  INDX                        PIC S9(3)   VALUE +0.                    
009000 77  W-KVPOST                    PIC S9(7)   VALUE ZERO COMP-3.           
009100 77  W-KVAVIS                    PIC S9(7)   VALUE ZERO COMP-3.           
009200 77  W-INLA21-KVINLART           PIC S9(7)   VALUE ZERO COMP-3.           
009300 77  TCO-ANTAL-KOLLI             PIC S9(7)   VALUE ZERO COMP-3.           
009400 77  NCO-ANTAL-KOLLI             PIC S9(7)   VALUE ZERO COMP-3.           
009500 77  W-KVDAGAR-INLEV             PIC S9(3)   VALUE ZERO COMP-3.           
009600 77  W-FS-IDFTG                  PIC  9(2)   VALUE ZERO.                  
009700 77  W-FLFEL                     PIC X       VALUE SPACE.                 
009800 77  FL-IDFS-OK                  PIC X       VALUE 'N'.                   
009900 77  RED-IX-IN                   PIC S9(3)   VALUE +0   COMP-3.           
010000 77  RED-IX-UT                   PIC S9(3)   VALUE +0   COMP-3.           
010100 77  NCO-MAX                     PIC S9(3)   VALUE +99  COMP-3.           
010200 77  WS-KVAVROP                  PIC S9(7)   COMP-3 VALUE ZERO.           
010300 77  KDRC-DISPLAY                PIC Z(5).                                
010400 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
010500 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
010600 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
010700     SKIP2                                                                
010800 77  W61101-EOF-SW               PIC X       VALUE 'N'.                   
010900     88  END-OF-W61101                       VALUE 'J'.                   
011000     EJECT                                                                
011100*      --- VALID IDDC CODES                                               
011200*                                                                         
011300*01    -COPY WWDC99                                                       
011400*01    -COPY WWDCKONS                                                     
011500*01    -COPY WWIDFTG                                                      
011600       EJECT                                                              
011700*      --- EMBALLAGE-KODER MED ÖVERSÄTTNINGAR                             
011800*01    -COPY W611EMB3                                                     
011900       EJECT                                                              
012000 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
012100 01  FILLER REDEFINES DAGENS-DATUM.                                       
012200     03  DAGENS-DATUM-AAR        PIC 9(2).                                
012300     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
012400     03  DAGENS-DATUM-DAG        PIC 9(2).                                
012500     EJECT                                                                
012600 01  DYNAMISKA-SUBPROGRAM.                                                
012700*                                                                         
012800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
012900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
013000     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
013100     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
013200     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
013300     03  W611LEVP                PIC X(8)    VALUE 'W611LEVP'.            
013400     03  WORKDAY                 PIC X(8)    VALUE 'WORKDAY '.            
013500     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
013600     EJECT                                                                
013700*    --- PARAMETRAR TILL ABEND                                            
013800                                                                          
013900 01  RKOD-ABEND                  PIC S9(4)  VALUE +33 COMP SYNC.          
014000                                                                          
014100 01  FILLER                      PIC X(16)   VALUE 'SEND-CONTROL'.        
014200*01  -COPY WZ01SEND                                                       
014300     EJECT                                                                
014400*01  -COPY WAPIINFO                                                       
014500                                                                          
014600 01  FILLER                      PIC X(16)   VALUE 'WGC-ERR-DATA'.        
014700 01  WGC-ERR-DATA.                                                        
014800*03  -COPY WGC00Q01 -PRE WGC-                                             
014900*    --- PARAMETRAR TILL POSTSUM                                          
015000*                                                                         
015100*01  -COPY W0005   -PRE  POSTSUM-                                         
015200     EJECT                                                                
015300*01  -COPY WDATAREA                                                       
015400     EJECT                                                                
015500*01  -COPY W611LEVP                                                       
015600     EJECT                                                                
015700 01  FILLER             PIC X(16) VALUE 'WORKAREA     '.                  
015800*01   -COPY WORKAREA.                                                     
015900     EJECT                                                                
016000**************************************************                        
016100*    DETTA FÄLT ÄR PARAMETER VID ANROP AV CHECK, *                        
016200*    SOM RÄKNAR UT KONTROLLSIFFRAN FÖR LÖPNUMMER *                        
016300**************************************************                        
016400*                                                                         
016500     EJECT                                                                
016600 01  FILLER                      PIC X(16)   VALUE 'IN-AREA'.             
016700 01  WIN-AREA                    PIC X(1009).                             
016800                                                                          
016900 01  FILLER REDEFINES WIN-AREA.                                           
017000*  03  -COPY W611MID                                                      
017100     EJECT                                                                
017200 01  FILLER REDEFINES WIN-AREA.                                           
017300*  03  -COPY W611CDT                                                      
017400     EJECT                                                                
017500 01  FILLER REDEFINES WIN-AREA.                                           
017600*  03  -COPY W611SDT                                                      
017700     EJECT                                                                
017800 01  FILLER REDEFINES WIN-AREA.                                           
017900*  03  -COPY W611CSG                                                      
018000     EJECT                                                                
018100 01  FILLER REDEFINES WIN-AREA.                                           
018200*  03  -COPY W611DTR                                                      
018300     EJECT                                                                
018400 01  FILLER REDEFINES WIN-AREA.                                           
018500*  03  -COPY W611ARD                                                      
018600     EJECT                                                                
018700 01  FILLER REDEFINES WIN-AREA.                                           
018800*  03  -COPY W611TCO                                                      
018900     EJECT                                                                
019000 01  FILLER REDEFINES WIN-AREA.                                           
019100*  03  -COPY W611NCO                                                      
019200     EJECT                                                                
019300 01  SPAR-AREA.                                                           
019400   03  SPAR-IDFROM                 PIC X(35).                             
019500   03  SPAR-IDDC                   PIC X(2).                              
019600   03  SPAR-IDFS                   PIC X(8).                              
019700   03  SPAR-IDLEVNR-FULL           PIC X(20)  VALUE SPACE.                
019800   03  SPAR-IDLEVNR                PIC X(5)   VALUE SPACE.                
019900   03  SPAR-IDLEVNR-601            PIC X(5)   VALUE SPACE.                
020000   03  SPAR-IDLEVNR-SHIP           PIC X(5)   VALUE SPACE.                
020100   03  SPAR-IDRADNR                PIC 9(4).                              
020200   03  SPAR-IDLBBET                PIC X(17).                             
020300   03  SPAR-KDSTATUS               PIC X(1).                              
020400   03  SPAR-KDRT                   PIC X(2).                              
020500   03  SPAR-IDARTNR                PIC 9(8).                              
020600   03  WS-ART-IDARTNR              PIC 9(8).                              
020700   03  SPAR-IDPLF                  PIC 9(2)   VALUE ZERO.                 
020800   03  SPAR-KVAVIS                 PIC 9(7).                              
020900   03  SPAR-KVINLART               PIC 9(7).                              
021000   03  FIX-MG-KVINLART             PIC 9(7).                              
021100   03  SPAR-TIAVISTA               PIC 9(6).                              
021200   03  SPAR-TIANKDAG               PIC 9(6).                              
021300   03  SPAR-TIAVIDAT               PIC 9(6).                              
021400   03  SPAR-DATUM                  PIC 9(6).                              
021500   03  W-DA-YYMMDD                 PIC 9(6).                              
021600   03  SPAR-ANTAL-KOLLI            PIC 9(4).                              
021700   03  SPAR-ANTAL-I-KOLLI          PIC X(7).                              
021800   03  SPAR-IDXKOLLI               PIC X(10).                             
021900   03  SPAR-TRPSAETT               PIC X(2)         VALUE SPACE.          
022000   03  SPAR-FS-FLFEL               PIC X(1)         VALUE 'N'.            
022100   03  SPAR-PARTI-FLFEL            PIC X(1)         VALUE 'N'.            
022200   03  SPAR-KVDAGAR-TTC1           PIC 9(3)        VALUE ZERO.            
022300   03  SPAR-KVDAGAR-INLEV          PIC 9(3)        VALUE ZERO.            
022400   03  SPAR-VLARTNTO           PIC S9(8)V9(1) COMP-3 VALUE ZERO.          
022500   03  SPAR-VKART              PIC S9(7)      COMP-3 VALUE ZERO.          
022600   03  SPAR-ADLAGOMR           PIC S9(3)      COMP-3 VALUE ZERO.          
022700   03  SPAR-ADGANG             PIC S9(3)      COMP-3 VALUE ZERO.          
022800   03  SPAR-ADPLATS            PIC S9(5)      COMP-3 VALUE ZERO.          
022900   03  SPAR-BEFT               PIC S9(3)      COMP-3 VALUE ZERO.          
023000   03  SPAR-IDFTG              PIC  9(2)             VALUE ZERO.          
023100   03  SPAR-PRARTSTD           PIC S9(7)V9(2) COMP-3 VALUE ZERO.          
023200   03  SPAR-IDFKNGRP           PIC S9(5)      COMP-3 VALUE ZERO.          
023300   03  SPAR-BEART              PIC X(25)             VALUE SPACE.         
023400   03  SPAR-CL1-KDLAGEMB       PIC X(4)              VALUE SPACE.         
023500   03  SPAR-KDARTURS           PIC X(2)              VALUE SPACE.         
023600   03  SPAR-KDFARLIG           PIC S9(1)      COMP-3 VALUE ZERO.          
023700   03  SPAR-KDSORT             PIC X(2)              VALUE SPACE.         
023800   03  SPAR-KVMP               PIC S9(7)      COMP-3 VALUE ZERO.          
023900   03  SPAR-KDERS              PIC S9(3)      COMP-3 VALUE ZERO.          
024000   03  SPAR-COUNTRY-OF-ORIGIN  PIC X(2)              VALUE SPACE.         
024100   03  W-TEMP-IDLEVNR          PIC X(5)              VALUE SPACE.         
024200   03  TRAFF                   PIC X(1)              VALUE SPACE.         
024300                                                                          
024400   EJECT                                                                  
024500                                                                          
024600 01  DAGENS-TIDPUNKT.                                                     
024700   03  DAGENS-TID-TIMMA          PIC 9(2).                                
024800   03  DAGENS-TID-MINUT          PIC 9(2).                                
024900   03  DAGENS-TID-SEKUND         PIC 9(2).                                
025000   03  DAGENS-TID-TH             PIC 9(2).                                
025100                                                                          
025200     EJECT                                                                
025300 01  FILLER                      PIC X(16)   VALUE 'SWITCHAR '.           
025400 01  SWITCHAR.                                                            
025500    03    ODETTE-POST-LAEST-SW   PIC X(1)    VALUE 'N'.                   
025600      88  ODETTE-POST-LAEST                  VALUE 'J'.                   
025700    03    INLA01-DUBLETT-SW      PIC X(1)    VALUE 'N'.                   
025800      88  INLA01-DUBLETT                     VALUE 'J'.                   
025900    03    INLA01-SW              PIC X(1)    VALUE 'N'.                   
026000      88  INLA01-SKRIVEN                     VALUE 'J'.                   
026100    03    INLA21-SW              PIC X(1)    VALUE 'N'.                   
026200      88  INLA21-SKRIVEN                     VALUE 'J'.                   
026300    03    TCO-SW                 PIC X(1)    VALUE 'N'.                   
026400      88  TCO-FINNS                          VALUE 'J'.                   
026500    03    FOERSTA-NCO-SW         PIC X(1)    VALUE 'J'.                   
026600      88  FOERSTA-NCO                        VALUE 'J'.                   
026700    03    KOLLI-SW               PIC X(1)    VALUE 'J'.                   
026800      88  KOLLI-OK                           VALUE 'J'.                   
026900      88  KOLLI-FEL                          VALUE 'N'.                   
027000    03    OMSTART-SW             PIC X(1)    VALUE 'N'.                   
027100      88  OMSTART                            VALUE 'J'.                   
027200                                                                          
027300    03    TCO-FEL-SW             PIC X(1)    VALUE 'N'.                   
027400      88  TCO-FEL                            VALUE 'J'.                   
027500      88  TCO-OK                             VALUE 'N'.                   
027600                                                                          
027700    03    FOERSTA-MKOLLI-SW      PIC X(1)    VALUE 'J'.                   
027800      88  FOERSTA-MKOLLI                     VALUE 'J'.                   
027900      88  FLERA-MKOLLI                       VALUE 'N'.                   
028000                                                                          
028100 01  FILLER                      PIC X(16)   VALUE 'INDEX '.              
028200 01  EMB-IX                      PIC S9(4)   VALUE +0 COMP SYNC.          
028300 01  NCO-IX                      PIC S9(4)   VALUE +0 COMP SYNC.          
028400                                                                          
028500 01  W-NCO-TAB.                                                           
028600     03 NCO-IDOKOLLI-TAB         PIC X(10)   OCCURS 99.                   
028700     EJECT                                                                
028800 01  FILLER.                                                              
028900     03  SPAR-INLA01-AREA.                                                
029000*        05  -COPY W6D101 -PRE SPAR-                                      
029100     EJECT                                                                
029200 01  UT-AREA-START              PIC X(24)   VALUE                         
029300                                 'UT-AREA-START  '.                       
029400     SKIP2                                                                
029500                                                                          
029600*01  AREA -COPY W161TEXT   -PRE UT-                                       
029700*                                                                         
029800     EJECT                                                                
029900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
030000 01  NYCKLAR-TILL-DLI.                                                    
030100     03  W-W6D101KY-X.                                                    
030200         05  W-D101KY-IDDC       PIC  X(2)   VALUE SPACE.                 
030300         05  W-D101KY-IDLEVNR    PIC  X(5)   VALUE SPACE.                 
030400         05  W-D101KY-IDFS       PIC  X(8)   VALUE SPACE.                 
030500         05  W-D101KY-TIAVIDAT   PIC S9(7)   VALUE ZERO COMP-3.           
030600                                                                          
030700     03  W-W6D1C1KY-MIN-X.                                                
030800      05  W-D1C1KY-IDLEVNR-KOLLI-MIN PIC  X(5) VALUE SPACE.               
030900      05  W-D1C1KY-IDOKOLLI-MIN      PIC  9(9) VALUE ZERO.                
031000      05  W-D1C1KY-IDRADNR-INL-MIN   PIC S9(5) COMP-3 VALUE ZERO.         
031100      05  W-D1C1KY-IDDC-MIN          PIC X(2)  VALUE SPACE.               
031200      05  W-D1C1KY-IDLEVNR-MIN       PIC  X(5) VALUE SPACE.               
031300      05  W-D1C1KY-IDFS-MIN          PIC X(8)  VALUE SPACE.               
031400      05  W-D1C1KY-TIAVIDAT-MIN      PIC S9(7) COMP-3 VALUE ZERO.         
031500      05  W-D1C1KY-IDRADNR-MIN       PIC S9(5) COMP-3 VALUE ZERO.         
031600                                                                          
031700     03  W-W6D1C1KY-MAX-X.                                                
031800      05  W-D1C1KY-IDLEVNR-KOLLI-MAX PIC  X(5) VALUE SPACE.               
031900      05  W-D1C1KY-IDOKOLLI-MAX      PIC  9(9) VALUE ZERO.                
032000      05  W-D1C1KY-IDRADNR-INL-MAX   PIC S9(5) COMP-3 VALUE ZERO.         
032100      05  W-D1C1KY-IDDC-MAX          PIC X(2)  VALUE SPACE.               
032200      05  W-D1C1KY-IDLEVNR-MAX       PIC  X(5) VALUE SPACE.               
032300      05  W-D1C1KY-IDFS-MAX          PIC X(8)  VALUE SPACE.               
032400      05  W-D1C1KY-TIAVIDAT-MAX      PIC S9(7) COMP-3 VALUE ZERO.         
032500      05  W-D1C1KY-IDRADNR-MAX       PIC S9(5) COMP-3 VALUE ZERO.         
032600                                                                          
032700     03  W-IDRADNR-INL-X.                                                 
032800         05  W-IDRADNR-INL       PIC S9(5)   VALUE ZERO COMP-3.           
032900     03  W-IDARTNR-X.                                                     
033000         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
033100     03  W-WDD901KY-X.                                                    
033200         05  W-IDARTNR-D9        PIC S9(9)   VALUE ZERO COMP-3.           
033300         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
033400     03  W-IDLAND-X.                                                      
033500         05  W-IDLAND            PIC X(2)    VALUE SPACE.                 
033600     03  W-IDRADNR-X.                                                     
033700         05  W-IDRADNR           PIC S9(5)   VALUE ZERO COMP-3.           
033800     03  W-IDSKYLT-X.                                                     
033900         05  W-IDSKYLT           PIC X(3)    VALUE SPACE.                 
034000     03  W-WDD3BSEQ-X.                                                    
034100         05  W-IDARTNR-WDD3      PIC S9(9)   VALUE ZERO COMP-3.           
034200     03  W-KDSEGKEY-X.                                                    
034300         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
034400     03  W-IDLEVNR-X.                                                     
034500         05  W-IDLEVNR           PIC  X(5)   VALUE SPACE.                 
034600     03  W-IDLEVNRDC-X.                                                   
034700         05  W-IDLEVNRDC         PIC  X(5)   VALUE SPACE.                 
034800     03  W-W6D1CSEQ-X.                                                    
034900         05  W-IDLEVNRK          PIC  X(5)   VALUE SPACE.                 
035000         05  W-IDOKOLLI          PIC  9(9)   VALUE ZERO.                  
035100     03  W-IDLEVNRK-C-X.                                                  
035200         05  W-IDLEVNR-KOLLI     PIC  X(5)   VALUE SPACE.                 
035300     03  W-IDOKOLLI-C-X.                                                  
035400         05  W-IDOKOLLINR        PIC  9(9)   VALUE ZERO.                  
035500     03  W-WDGX-0613-KEY-X.                                               
035600         05  W-IDHTYP-0614       PIC X(04)   VALUE '0613'.                
035700         05  FILLER              PIC X(26)   VALUE LOW-VALUE.             
035800     03  W-W6GX-6013-KEY-X.                                               
035900         05  W-IDHTYP-6013       PIC X(04)   VALUE '6013'.                
036000         05  FILLER              PIC X(26)   VALUE LOW-VALUE.             
036100     03  W-W6GX-6014-KEY-X.                                               
036200         05  FILLER              PIC X       VALUE '1'.                   
036300     03  W-KDAVROP-X.                                                     
036400         05  W-KDAVROP           PIC S9(1)    VALUE ZERO COMP-3.          
036500     03 W-WDGXKEY-0103-X.                                                 
036600        05  W-IDHTYP-0103       PIC X(4)    VALUE '0103'.                 
036700        05  FILLER              PIC X(26)   VALUE LOW-VALUE.              
036800     03 W-KY0104-X.                                                       
036900        05  W-ADDISPABS         PIC X(50)    VALUE SPACES.                
037000*                                                                         
037100     EJECT                                                                
037200*    --- STATUS-KOD FRÅN IMS                                              
037300 01  STATUS-WS                   PIC XX.                                  
037400     88  SEGMENT-FINNS                       VALUE '  '.                  
037500     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
037600     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
037700     88  SEGMENT-SLUT                        VALUE 'GB'.                  
037800     88  IMS-EJ-OK                           VALUE 'XD'.                  
037900     SKIP2                                                                
038000 01  GODK-STATUSKODER.                                                    
038100     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
038200     SKIP3                                                                
038300 01  SSA1                        PIC X(128).                              
038400 01  SSA2                        PIC X(128).                              
038500 01  SSA3                        PIC X(64).                               
038600     EJECT                                                                
038700*    --- IMS FUNKTIONSKODER                                               
038800*01  -COPY W0003                                                          
038900     EJECT                                                                
039000*    ---  DLI INPUT-OUTPUT AREA                                           
039100 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
039200     SKIP3                                                                
039300 01  DLI-IO-AREA.                                                         
039400     03  IO-AREA                 PIC X(900)  VALUE SPACE.                 
039500     SKIP3                                                                
039600     03  WLARTC01 REDEFINES IO-AREA.                                      
039700*        05  -COPY WDK601  -PRE ARTC-                                     
039800     EJECT                                                                
039900     03  WLARTC11 REDEFINES IO-AREA.                                      
040000*        05  -COPY WDK611  -PRE ARTC-                                     
040100     EJECT                                                                
040200     03  WLARTC23 REDEFINES IO-AREA.                                      
040300*        05  -COPY WDK623  -PRE ARTC-                                     
040400     EJECT                                                                
040500     03  WLBENA11 REDEFINES IO-AREA.                                      
040600*        05  -COPY WDD311  -PRE BENA11-                                   
040700     EJECT                                                                
040800     03  W6INLA01 REDEFINES IO-AREA.                                      
040900*        05  -COPY W6D101                                                 
041000     EJECT                                                                
041100     03  W6INLA11 REDEFINES IO-AREA.                                      
041200*        05  -COPY W6D111                                                 
041300     EJECT                                                                
041400     03  W6INLA21 REDEFINES IO-AREA.                                      
041500*        05  -COPY W6D121                                                 
041600     EJECT                                                                
041700     03  W6CKPA11 REDEFINES IO-AREA.                                      
041800*        05  -COPY W6GX6014  -PRE CKPA-                                   
041900     EJECT                                                                
042000 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDD9'.           
042100     SKIP3                                                                
042200 01  DLI-IO-WDD9.                                                         
042300     03  WDD9.                                                            
042400*        05  -COPY WDD905                                                 
042500     EJECT                                                                
042600 01  DLI-IO-WDB601.                                                       
042700     03  WDB601.                                                          
042800*        05  -COPY WDB601                                                 
042900     EJECT                                                                
043000 01  DLI-IO-WDK711.                                                       
043100     03  WDK711.                                                          
043200*        05  -COPY WDK711                                                 
043300     EJECT                                                                
043400 01  DLI-IO-WDK712.                                                       
043500     03  WDK712.                                                          
043600*        05  -COPY WDK712                                                 
043700     EJECT                                                                
043800 01  DLI-IO-WDF101.                                                       
043900     03  WDF101.                                                          
044000*        05  -COPY WDF101                                                 
044100     EJECT                                                                
044200 01  DLI-IO-WDF116.                                                       
044300     03  WDF116.                                                          
044400*        05  -COPY WDF116                                                 
044500     EJECT                                                                
044600 01  DLI-IO-WDGX0104.                                                     
044700*    03   -COPY WDGX0104                                                  
044800                                                                          
044900 LINKAGE SECTION.                                                         
045000                                                                          
045100*01  -COPY W0009   -PRE MSG-                                              
045200     EJECT                                                                
045300 01  GC-ERROR-PCB                PIC X.                                   
045400*01  -COPY W0008  -PRE ATAB-                                              
045500     05  FILLER                  PIC X.                                   
045600*01  -COPY W0008  -PRE ARTC-                                              
045700     05  FILLER                  PIC X.                                   
045800     EJECT                                                                
045900*01  -COPY W0008  -PRE BENA-                                              
046000     05  FILLER                  PIC X.                                   
046100     EJECT                                                                
046200*01  -COPY W0008  -PRE INLA-                                              
046300     05  FILLER                  PIC X.                                   
046400     EJECT                                                                
046500*01  -COPY W0008  -PRE INLD-                                              
046600     05  FILLER                  PIC X.                                   
046700     EJECT                                                                
046800*01  -COPY W0008  -PRE WDF1-                                              
046900     05  FILLER                  PIC X.                                   
047000     EJECT                                                                
047100*01  -COPY W0008  -PRE WDD9-                                              
047200     05  FILLER                  PIC X.                                   
047300     EJECT                                                                
047400*01  -COPY W0008  -PRE CKPA-                                              
047500     05  FILLER                  PIC X.                                   
047600                                                                          
047700 01  LEVP-INLB-PCB               PIC X.                                   
047800     EJECT                                                                
047900                                                                          
048000*01  -COPY W0008  -PRE WDB6-                                              
048100     05  FILLER                  PIC X.                                   
048200                                                                          
048300*01  -COPY W0008  -PRE WDK7-                                              
048400     05  FILLER                  PIC X.                                   
048500                                                                          
048600                                                                          
048700 PROCEDURE DIVISION  USING MSG-PCB  GC-ERROR-PCB                          
048800                           ATAB-PCB ARTC-PCB                              
048900                           BENA-PCB INLA-PCB INLD-PCB WDF1-PCB            
049000                           WDD9-PCB CKPA-PCB LEVP-INLB-PCB                
049100                           WDB6-PCB WDK7-PCB.                             
049200                                                                          
049300     PERFORM A-INIT                                                       
049400     PERFORM B-BEHANDLA-AVISERING                                         
049500                                                                          
049600     PERFORM Z-FINIT                                                      
049700                                                                          
049800     MOVE ZERO TO RETURN-CODE                                             
049900     GOBACK                                                               
050000     .                                                                    
050100     EJECT                                                                
050200 A-INIT SECTION.                                                          
050300     SKIP2                                                                
050400                                                                          
050500     OPEN INPUT  W61101                                                   
050600     OPEN OUTPUT W61101UT                                                 
050700                                                                          
050800     MOVE ZERO                 TO CHKP-ANT                                
050900     ACCEPT DAGENS-DATUM       FROM DATE                                  
051000                                                                          
051100     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
051200                                                                          
051300     PERFORM IMS-RESTART                                                  
051400     PERFORM IMS-LAS-ATERSTART                                            
051500                                                                          
051600     IF SEGMENT-FINNS                                                     
051700       IF CKPA-6014-KVPOST > ZERO                                         
051800         PERFORM AA-ATERSTART-EFTER-ABEND                                 
051900       END-IF                                                             
052000     END-IF                                                               
052100     MOVE JA                   TO INLA21-SW                               
052200     .                                                                    
052300     EJECT                                                                
052400 AA-ATERSTART-EFTER-ABEND SECTION.                                        
052500     SKIP2                                                                
052600     PERFORM UNTIL W-KVPOST    = CKPA-6014-KVPOST OR                      
052700                                 END-OF-W61101                            
052800       PERFORM S01-LAES-W61101                                            
052900       IF POSTTYP = 'MID'                                                 
053000         PERFORM AAA-SPARA-MID                                            
053100       END-IF                                                             
053200       IF POSTTYP = 'CDT'                                                 
053300         PERFORM AAB-SPARA-CDT                                            
053400       END-IF                                                             
053500       IF POSTTYP = 'SDT'                                                 
053600         PERFORM AAE-SPARA-SDT                                            
053700       END-IF                                                             
053800       IF POSTTYP = 'CSG'                                                 
053900         PERFORM AAD-SPARA-CSG                                            
054000       END-IF                                                             
054100     END-PERFORM                                                          
054200                                                                          
054300     IF POSTTYP                = 'MID '                                   
054400       PERFORM S05-BEHANDLA-MID-POST                                      
054500      ELSE                                                                
054600       IF POSTTYP            = 'ARD'                                      
054700         PERFORM AAC-INITIERA                                             
054800       ELSE                                                               
054900         STRING 'FEL FRÅN SECTION AA- ' DELIMITED BY SIZE                 
055000         INTO FELTEXT                                                     
055100         CALL ABEND USING RKOD-ABEND                                      
055200       END-IF                                                             
055300     END-IF                                                               
055400                                                                          
055500     MOVE JA TO OMSTART-SW                                                
055600     .                                                                    
055700     EJECT                                                                
055800 AAA-SPARA-MID SECTION.                                                   
055900     MOVE +1 TO RED-IX-IN                                                 
056000                RED-IX-UT                                                 
056100     MOVE SPACE TO SPAR-IDFS                                              
056200     MOVE NEJ   TO FL-IDFS-OK                                             
056300                                                                          
056400     PERFORM UNTIL RED-IX-IN > 8                                          
056500       IF MID-1004(RED-IX-IN:1) = SPACE OR ZERO                           
056600         CONTINUE                                                         
056700       ELSE                                                               
056800         PERFORM UNTIL RED-IX-IN > 8                                      
056900           MOVE MID-1004(RED-IX-IN:1) TO SPAR-IDFS(RED-IX-UT:1)           
057000           IF MID-1004(RED-IX-IN:1) NUMERIC                               
057100             IF MID-1004(RED-IX-IN:1) > ZERO                              
057200               MOVE JA TO FL-IDFS-OK                                      
057300             END-IF                                                       
057400           END-IF                                                         
057500           ADD +1 TO RED-IX-IN                                            
057600                     RED-IX-UT                                            
057700         END-PERFORM                                                      
057800       END-IF                                                             
057900       ADD +1 TO RED-IX-IN                                                
058000     END-PERFORM                                                          
058100                                                                          
058200     IF FL-IDFS-OK = NEJ                                                  
058300       MOVE 'INGEN GILTIG SIFFRA I IDFS:' TO FELTEXT-STR(2:27)            
058400       MOVE SPAR-IDFS TO FELTEXT-STR(29:8)                                
058500       DISPLAY FELTEXT                                                    
058600       CALL FELLOG                                                        
058700     ELSE                                                                 
058800       MOVE SPAR-IDFS            TO W-D101KY-IDFS                         
058900     END-IF                                                               
059000                                                                          
059100     MOVE MID-2007             TO SPAR-TIAVIDAT                           
059200                                  W-D101KY-TIAVIDAT                       
059300     MOVE FUNCTION CURRENT-DATE(3:6) TO SPAR-DATUM                        
059400     COMPUTE W-DA-YYMMDD = SPAR-DATUM - 10000                             
059500     IF SPAR-TIAVIDAT NUMERIC AND SPAR-TIAVIDAT > ZERO AND                
059600        SPAR-TIAVIDAT NOT > SPAR-DATUM AND                                
059700        SPAR-TIAVIDAT > W-DA-YYMMDD                                       
059800       MOVE SPAR-TIAVIDAT TO DAT-I-TIDATUM                                
059900       MOVE 'AAMMDD' TO DAT-KDDATFORM                                     
060000                                                                          
060100       CALL WDATKONV USING DAT-KDDATFORM,                                 
060200                           DAT-I-TIDATUM,                                 
060300                           DAT-O-TIDATUM,                                 
060400                           DAT-KDSVAR                                     
060500       IF DAT-KDSVAR-OK                                                   
060600         CONTINUE                                                         
060700       ELSE                                                               
060800         MOVE FUNCTION CURRENT-DATE(3:6) TO SPAR-TIAVIDAT                 
060900                                            W-D101KY-TIAVIDAT             
061000       END-IF                                                             
061100     ELSE                                                                 
061200       MOVE FUNCTION CURRENT-DATE(3:6) TO SPAR-TIAVIDAT                   
061300                                          W-D101KY-TIAVIDAT               
061400     END-IF                                                               
061500     .                                                                    
061600     EJECT                                                                
061700 AAB-SPARA-CDT SECTION.                                                   
061800     MOVE CDT-000-GRP          TO SPAR-IDLEVNR-FULL                       
061900     MOVE CDT-3296             TO SPAR-IDLEVNR                            
062000     MOVE SPAR-IDLEVNR         TO W-IDLEVNR                               
062100     .                                                                    
062200     EJECT                                                                
062300 AAC-INITIERA SECTION.                                                    
062400     MOVE JA TO ODETTE-POST-LAEST-SW                                      
062500                INLA01-SW                                                 
062600     PERFORM IMS-GU-INLA01                                                
062700     MOVE DLI-IO-AREA TO SPAR-INL-W6D101                                  
062800     .                                                                    
062900     EJECT                                                                
063000 AAD-SPARA-CSG        SECTION.                                            
063100     MOVE CSG-3296             TO W-IDLEVNRDC                             
063200     PERFORM IMS-GU-WDB601-LEVNR-DC                                       
063300     IF SEGMENT-FINNS                                                     
063400       MOVE DCS-IDDC           TO SPAR-IDDC                               
063500                                  W-D101KY-IDDC                           
063600                                  WS-IDDC                                 
063700                                  W-IDDC                                  
063800       MOVE DCS-IDFTG          TO SPAR-IDFTG                              
063900       MOVE DCS-IDLANDX2       TO W-IDLAND                                
064000     ELSE                                                                 
064100       IF W-IDLEVNRDC = 'CUGZQ'                                           
064200         MOVE WC-NDC-US-RU       TO SPAR-IDDC                             
064300                                    W-D101KY-IDDC                         
064400                                    WS-IDDC                               
064500                                    W-IDDC                                
064600         MOVE WC-IDFTG-US        TO SPAR-IDFTG                            
064700         MOVE 'US'               TO W-IDLAND                              
064800       ELSE                                                               
064900         IF W-IDLEVNRDC = 'CUGZR'                                         
065000           MOVE WC-NDC-US-LA       TO SPAR-IDDC                           
065100                                      W-D101KY-IDDC                       
065200                                      WS-IDDC                             
065300                                      W-IDDC                              
065400           MOVE WC-IDFTG-US        TO SPAR-IDFTG                          
065500           MOVE 'US'               TO W-IDLAND                            
065600         ELSE                                                             
065700           MOVE WC-CDC-SE          TO SPAR-IDDC                           
065800                                      W-D101KY-IDDC                       
065900                                      WS-IDDC                             
066000                                      W-IDDC                              
066100           MOVE WC-IDFTG-PV        TO SPAR-IDFTG                          
066200         END-IF                                                           
066300       END-IF                                                             
066400     END-IF                                                               
066500     .                                                                    
066600     EJECT                                                                
066700 AAE-SPARA-SDT SECTION.                                                   
066800     MOVE SDT-000-GRP          TO SPAR-IDLEVNR-FULL                       
066900     MOVE SDT-3296             TO SPAR-IDLEVNR                            
067000     MOVE SPAR-IDLEVNR         TO W-IDLEVNR                               
067100                                  W-D101KY-IDLEVNR                        
067200     .                                                                    
067300     EJECT                                                                
067400 B-BEHANDLA-AVISERING   SECTION.                                          
067500     IF NOT OMSTART                                                       
067600       PERFORM S01-LAES-W61101                                            
067700     END-IF                                                               
067800     PERFORM UNTIL END-OF-W61101                                          
067900        EVALUATE POSTTYP                                                  
068000                                                                          
068100        WHEN 'MID '                                                       
068200         PERFORM BA-INIT-FOELJESEDEL-AREA                                 
068300         PERFORM S05-BEHANDLA-MID-POST                                    
068400                                                                          
068500        WHEN 'CDT '                                                       
068600         PERFORM BC-BEHANDLA-CDT-POST                                     
068700                                                                          
068800        WHEN 'SDT '                                                       
068900         PERFORM BCA-BEHANDLA-SDT-POST                                    
069000                                                                          
069100        WHEN 'CSG '                                                       
069200         PERFORM BD-BEHANDLA-CSG-POST                                     
069300                                                                          
069400        WHEN 'DTR '                                                       
069500         PERFORM BE-BEHANDLA-DTR-POST                                     
069600                                                                          
069700        WHEN 'ARD '                                                       
069800         PERFORM BG-BEHANDLA-ART-KOLLI-INFO                               
069900                                                                          
070000         IF INLA01-DUBLETT OR SPAR-TIANKDAG = ZERO                        
070100             CONTINUE                                                     
070200          ELSE                                                            
070300             PERFORM BH-UPPDATERA-LEVERANSPLAN                            
070400         END-IF                                                           
070500                                                                          
070600        END-EVALUATE                                                      
070700                                                                          
070800        IF ODETTE-POST-LAEST                                              
070900            CONTINUE                                                      
071000         ELSE                                                             
071100            PERFORM S01-LAES-W61101                                       
071200        END-IF                                                            
071300                                                                          
071400        IF END-OF-W61101 OR POSTTYP = 'MID '                              
071500            IF INLA21-SKRIVEN                                             
071600                CONTINUE                                                  
071700             ELSE                                                         
071800                PERFORM S10-SKAPA-INLA21                                  
071900            END-IF                                                        
072000            IF SPAR-FS-FLFEL      = JA                                    
072100                PERFORM BI-UPPDATERA-INLA01                               
072200                PERFORM C-ERROR-PROCESSING                                
072300            END-IF                                                        
072400        END-IF                                                            
072500                                                                          
072600        IF (POSTTYP  = 'MID ' OR 'ARD') AND                               
072700           (CHKP-ANT > CHKP-MAX)                                          
072800            PERFORM X-TAG-CHECKPOINT                                      
072900        END-IF                                                            
073000                                                                          
073100     END-PERFORM                                                          
073200     .                                                                    
073300     EJECT                                                                
073400 BA-INIT-FOELJESEDEL-AREA   SECTION.                                      
073500     MOVE ZERO                 TO W-FS-IDFTG                              
073600     MOVE NEJ                  TO ODETTE-POST-LAEST-SW                    
073700                                  INLA01-SW                               
073800                                  INLA01-DUBLETT-SW                       
073900                                  SPAR-FS-FLFEL                           
074000                                  SPAR-PARTI-FLFEL                        
074100     .                                                                    
074200     EJECT                                                                
074300                                                                          
074400 BC-BEHANDLA-CDT-POST SECTION.                                            
074500     MOVE CDT-000-GRP          TO SPAR-IDLEVNR-FULL                       
074600     MOVE CDT-3296             TO SPAR-IDLEVNR                            
074700     MOVE SPAR-IDLEVNR         TO W-IDLEVNR                               
074800     .                                                                    
074900     EJECT                                                                
075000 BCA-BEHANDLA-SDT-POST SECTION.                                           
075100     MOVE SDT-000-GRP          TO SPAR-IDLEVNR-FULL                       
075200     MOVE SDT-3296             TO SPAR-IDLEVNR                            
075300     MOVE SPAR-IDLEVNR         TO W-IDLEVNR                               
075400                                                                          
075500     .                                                                    
075600     EJECT                                                                
075700 BD-BEHANDLA-CSG-POST     SECTION.                                        
075800     UNSTRING CSG-3296         DELIMITED BY SPACE                         
075900                               INTO W-IDLEVNRDC                           
076000                                                                          
076100     PERFORM IMS-GU-WDB601-LEVNR-DC                                       
076200     IF SEGMENT-FINNS                                                     
076300       MOVE DCS-IDDC           TO SPAR-IDDC                               
076400                                  W-D101KY-IDDC                           
076500                                  WS-IDDC                                 
076600                                  W-IDDC                                  
076700       MOVE DCS-IDFTG          TO SPAR-IDFTG                              
076800       MOVE DCS-IDLANDX2       TO W-IDLAND                                
076900     ELSE                                                                 
077000       IF W-IDLEVNRDC = 'CUGZQ'                                           
077100         MOVE WC-NDC-US-RU       TO SPAR-IDDC                             
077200                                    W-D101KY-IDDC                         
077300                                    WS-IDDC                               
077400                                    W-IDDC                                
077500         MOVE WC-IDFTG-US        TO SPAR-IDFTG                            
077600         MOVE 'US'               TO W-IDLAND                              
077700       ELSE                                                               
077800         IF W-IDLEVNRDC = 'CUGZR'                                         
077900           MOVE WC-NDC-US-LA       TO SPAR-IDDC                           
078000                                      W-D101KY-IDDC                       
078100                                      WS-IDDC                             
078200                                      W-IDDC                              
078300           MOVE WC-IDFTG-US        TO SPAR-IDFTG                          
078400           MOVE 'US'               TO W-IDLAND                            
078500         ELSE                                                             
078600           MOVE WC-CDC-SE          TO SPAR-IDDC                           
078700                                      W-D101KY-IDDC                       
078800                                      WS-IDDC                             
078900                                      W-IDDC                              
079000           MOVE WC-IDFTG-PV        TO SPAR-IDFTG                          
079100         END-IF                                                           
079200       END-IF                                                             
079300     END-IF                                                               
079400     .                                                                    
079500     EJECT                                                                
079600 BE-BEHANDLA-DTR-POST  SECTION.                                           
079700     IF DTR-8164 NOT = SPACE AND LOW-VALUE                                
079800       MOVE DTR-8164 TO SPAR-IDLBBET                                      
079900     ELSE                                                                 
080000       IF DTR-8212 NOT = SPACE AND LOW-VALUE                              
080100         MOVE DTR-8212 TO SPAR-IDLBBET                                    
080200       ELSE                                                               
080300         IF NDC                                                           
080400           MOVE 'TRPC3' TO SPAR-IDLBBET                                   
080500         ELSE                                                             
080600           MOVE 'TRPC1' TO SPAR-IDLBBET                                   
080700         END-IF                                                           
080800       END-IF                                                             
080900     END-IF                                                               
081000     .                                                                    
081100     EJECT                                                                
081200 BG-BEHANDLA-ART-KOLLI-INFO  SECTION.                                     
081300     PERFORM BGA-BEHANDLA-ARD-POST                                        
081400     IF INLA01-DUBLETT                                                    
081500         PERFORM S01-LAES-W61101                                          
081600         PERFORM UNTIL END-OF-W61101 OR POSTTYP = 'MID '                  
081700             PERFORM S01-LAES-W61101                                      
081800         END-PERFORM                                                      
081900         MOVE JA               TO INLA21-SW                               
082000      ELSE                                                                
082100         PERFORM S01-LAES-W61101                                          
082200         MOVE NEJ TO W-FLFEL                                              
082300         PERFORM UNTIL END-OF-W61101 OR POSTTYP = 'MID ' OR 'ARD '        
082400           EVALUATE POSTTYP                                               
082500            WHEN 'TCO '                                                   
082600               PERFORM BGB-BEHANDLA-TCO-POST                              
082700            WHEN 'NCO '                                                   
082800               PERFORM BGC-BEHANDLA-NCO-POST                              
082900           END-EVALUATE                                                   
083000           PERFORM S01-LAES-W61101                                        
083100         END-PERFORM                                                      
083200                                                                          
083300         IF TCO-ANTAL-KOLLI = NCO-ANTAL-KOLLI                             
083400           CONTINUE                                                       
083500         ELSE                                                             
083600           MOVE JA TO W-FLFEL                                             
083700         END-IF                                                           
083800                                                                          
083900         MOVE +0             TO TCO-ANTAL-KOLLI                           
084000                                NCO-ANTAL-KOLLI                           
084100                                                                          
084200         IF TCO-FINNS AND INLA21-SKRIVEN                                  
084300             IF SPAR-KVAVIS =  W-KVAVIS  AND                              
084400                SPAR-KVAVIS =  W-INLA21-KVINLART  AND                     
084500                W-FLFEL = NEJ                                             
084600                 CONTINUE                                                 
084700              ELSE                                                        
084800                 PERFORM IMS-GHU-INLA11                                   
084900                 MOVE JA       TO ART-FLFEL                               
085000                                  SPAR-FS-FLFEL                           
085100                 PERFORM IMS-REPL-INLA11                                  
085200                 ADD +1        TO CHKP-ANT                                
085300             END-IF                                                       
085400         END-IF                                                           
085500                                                                          
085600         IF INLA21-SKRIVEN                                                
085700             CONTINUE                                                     
085800          ELSE                                                            
085900             PERFORM S11-SKAPA-INLA21-RAD1                                
086000         END-IF                                                           
086100                                                                          
086200         IF SPAR-PARTI-FLFEL   =  JA                                      
086300             MOVE JA           TO SPAR-FS-FLFEL                           
086400         END-IF                                                           
086500     END-IF                                                               
086600     MOVE JA                   TO ODETTE-POST-LAEST-SW                    
086700     MOVE SPAR-IDFS            TO W-D101KY-IDFS                           
086800     .                                                                    
086900     EJECT                                                                
087000 BGA-BEHANDLA-ARD-POST  SECTION.                                          
087100                                                                          
087200     MOVE ZERO                 TO W-KVAVIS                                
087300                                  W-INLA21-KVINLART                       
087400     MOVE NEJ                  TO TCO-SW                                  
087500                                  INLA21-SW                               
087600                                  SPAR-PARTI-FLFEL                        
087700     MOVE JA                   TO FOERSTA-NCO-SW                          
087800                                                                          
087900     UNSTRING ARD-7304 DELIMITED BY SPACE INTO SPAR-IDARTNR               
088000     MOVE ARD-6270             TO SPAR-KVAVIS                             
088100                                                                          
088200     MOVE ARD-3239             TO SPAR-COUNTRY-OF-ORIGIN                  
088300                                                                          
088400                                                                          
088500     IF SPAR-IDARTNR           NOT NUMERIC                                
088600         MOVE ZERO             TO SPAR-IDARTNR                            
088700     END-IF                                                               
088800                                                                          
088900     IF SPAR-KVAVIS            NOT NUMERIC                                
089000         MOVE ZERO             TO SPAR-KVAVIS                             
089100     END-IF                                                               
089200     IF SPAR-KVAVIS            = ZERO                                     
089300         MOVE NEJ              TO SPAR-PARTI-FLFEL                        
089400     END-IF                                                               
089500                                                                          
089600     PERFORM BGAA-HAEMTA-ART-INFO                                         
089700     PERFORM BGAC-BERAEKNA-ANKDAG                                         
089800     PERFORM BGAD-KONTROLLERA-ART-INFO                                    
089900     MOVE SPAR-IDLEVNR         TO W-D101KY-IDLEVNR                        
090000     MOVE SPAR-IDFS            TO W-D101KY-IDFS                           
090100     MOVE SPAR-TIAVIDAT        TO W-D101KY-TIAVIDAT                       
090200     MOVE SPAR-IDARTNR         TO W-IDARTNR                               
090300     IF INLA01-SKRIVEN                                                    
090400         CONTINUE                                                         
090500      ELSE                                                                
090600         PERFORM BGAD-SKAPA-INLA01                                        
090700     END-IF                                                               
090800     IF INLA01-DUBLETT                                                    
090900         CONTINUE                                                         
091000      ELSE                                                                
091100         PERFORM BGAE-SKAPA-INLA11                                        
091200     END-IF                                                               
091300     .                                                                    
091400     EJECT                                                                
091500 BGAA-HAEMTA-ART-INFO   SECTION.                                          
091600     MOVE SPAR-IDARTNR         TO W-IDARTNR                               
091700     PERFORM BGAAA-HAEMTA-ARTC-INFO                                       
091800     PERFORM BGAAB-HAEMTA-BENA-INFO                                       
091900     .                                                                    
092000     EJECT                                                                
092100 BGAAA-HAEMTA-ARTC-INFO  SECTION.                                         
092200     PERFORM IMS-GU-ARTC01                                                
092300     IF SEGMENT-FINNS                                                     
092400*      MOVE ARTC-ART-IDFTG     TO SPAR-IDFTG                              
092500       MOVE ARTC-ART-IDFKNGRP  TO SPAR-IDFKNGRP                           
092600       MOVE ARTC-ART-KDSORT    TO SPAR-KDSORT                             
092700       MOVE ARTC-ART-IDLEVNR   TO SPAR-IDLEVNR-601                        
092800*                                                                         
092900       PERFORM IMS-GNP-ARTC11                                             
093000       IF SEGMENT-FINNS                                                   
093100         IF CDC                                                           
093200           MOVE ARTC-CLAG-KVDAGAR-INLEV TO W-KVDAGAR-INLEV                
093300           MOVE ARTC-CLAG-KDARTURS    TO SPAR-KDARTURS                    
093400           MOVE ARTC-CLAG-ADLAGOMR    TO SPAR-ADLAGOMR                    
093500           MOVE ARTC-CLAG-ADGANG      TO SPAR-ADGANG                      
093600           MOVE ARTC-CLAG-ADPLATS     TO SPAR-ADPLATS                     
093700         END-IF                                                           
093800         MOVE ARTC-CLAG-KVMP          TO SPAR-KVMP                        
093900         MOVE ARTC-CLAG-PRARTSTD      TO SPAR-PRARTSTD                    
094000         MOVE ARTC-CLAG-KDFARLIG      TO SPAR-KDFARLIG                    
094100         MOVE ARTC-CLAG-VLARTNTO      TO SPAR-VLARTNTO                    
094200         MOVE ARTC-CLAG-VKART         TO SPAR-VKART                       
094300         MOVE ARTC-CLAG-BEFT          TO SPAR-BEFT                        
094400         MOVE ARTC-CLAG-IDARTNR-EMBQ3 TO W-CL1-IDARTNR-EMBQ3              
094500         MOVE ARTC-CLAG-KDERS         TO SPAR-KDERS                       
094600         MOVE ARTC-CLAG-IDLEVNR-SHIP  TO SPAR-IDLEVNR-SHIP                
094700**                                                                        
094800**       TO FIND THE SHIP SUPPLIER                                        
094900         MOVE W-IDLEVNR                   TO W-TEMP-IDLEVNR               
095000                                                                          
095100         IF W-IDLEVNR = SPAR-IDLEVNR-601                                  
095200           MOVE SPAR-IDLEVNR-SHIP         TO W-IDLEVNR                    
095300         ELSE                                                             
095400           PERFORM IMS-GNP-ARTC23                                         
095500           MOVE NEJ TO TRAFF                                              
095600           PERFORM UNTIL SEGMENT-SAKNAS OR TRAFF = JA                     
095700             IF W-IDLEVNR = ARTC-AVT-IDLEVNR-AVT                          
095800               MOVE ARTC-AVT-IDLEVNR-SHIP TO W-IDLEVNR                    
095900               MOVE JA TO TRAFF                                           
096000             END-IF                                                       
096100             PERFORM IMS-GNP-ARTC23                                       
096200           END-PERFORM                                                    
096300         END-IF                                                           
096400                                                                          
096500**       GET VALUES FOM WDK712 FOR CHINSE AND US DC'S                     
096600         IF NDC-CN OR NDC-US                                              
096700           PERFORM IMS-GU-WDK712                                          
096800           MOVE LART-KVDAGAR-INLEV TO W-KVDAGAR-INLEV                     
096900           MOVE LART-KDARTURS      TO SPAR-KDARTURS                       
097000*                                                                         
097100           IF LART-VKART > 0                                              
097200             MOVE LART-VKART       TO SPAR-VKART                          
097300           END-IF                                                         
097400           IF LART-VLARTNTO > 0                                           
097500             MOVE LART-VLARTNTO    TO SPAR-VLARTNTO                       
097600           END-IF                                                         
097700           IF LART-BEFT > 0                                               
097800             MOVE LART-BEFT        TO SPAR-BEFT                           
097900           END-IF                                                         
098000*                                                                         
098100           PERFORM IMS-GU-WDK711                                          
098200           IF SEGMENT-FINNS                                               
098300             MOVE SLAG-ADLAGOMR      TO SPAR-ADLAGOMR                     
098400             MOVE SLAG-ADGANG        TO SPAR-ADGANG                       
098500             MOVE SLAG-ADPLATS       TO SPAR-ADPLATS                      
098600           END-IF                                                         
098700         END-IF                                                           
098800                                                                          
098900**       USE SHIP SUPPLIER TO GET KVDAGAR-TTC1                            
099000         IF NDC-CN OR NDC-US                                              
099100           PERFORM IMS-GU-WDF116                                          
099200           IF SEGMENT-FINNS AND W-IDLEVNR   NOT = SPACE                   
099300             MOVE NDC-KVDAGAR-TT   TO SPAR-KVDAGAR-TTC1                   
099400           ELSE                                                           
099500             MOVE ZERO                    TO SPAR-KVDAGAR-TTC1            
099600             MOVE JA                      TO SPAR-FS-FLFEL                
099700           END-IF                                                         
099800         ELSE                                                             
099900           PERFORM IMS-GU-WDF101                                          
100000           IF SEGMENT-FINNS AND W-IDLEVNR   NOT = SPACE                   
100100             MOVE LEV-KVDAGAR-TTC1        TO SPAR-KVDAGAR-TTC1            
100200           ELSE                                                           
100300             MOVE ZERO                    TO SPAR-KVDAGAR-TTC1            
100400             MOVE JA                      TO SPAR-FS-FLFEL                
100500           END-IF                                                         
100600         END-IF                                                           
100700         MOVE W-TEMP-IDLEVNR              TO W-IDLEVNR                    
100800       ELSE                                                               
100900         MOVE SPACE        TO SPAR-IDLEVNR-SHIP                           
101000         MOVE ZERO         TO W-KVDAGAR-INLEV                             
101100                              SPAR-KVMP                                   
101200                              SPAR-PRARTSTD                               
101300                              SPAR-KDERS                                  
101400                              SPAR-KDFARLIG                               
101500                              SPAR-VLARTNTO                               
101600                              SPAR-VKART                                  
101700                              SPAR-BEFT                                   
101800                              W-CL1-IDARTNR-EMBQ3                         
101900                              SPAR-ADLAGOMR                               
102000                              SPAR-ADGANG                                 
102100                              SPAR-ADPLATS                                
102200                              SPAR-KDARTURS                               
102300                              SPAR-KVDAGAR-TTC1                           
102400         MOVE JA           TO SPAR-PARTI-FLFEL                            
102500                              SPAR-FS-FLFEL                               
102600       END-IF                                                             
102700     ELSE                                                                 
102800       MOVE JA               TO SPAR-PARTI-FLFEL                          
102900                                SPAR-FS-FLFEL                             
103000       MOVE SPACE            TO SPAR-KDSORT                               
103100                                SPAR-KDARTURS                             
103200                                SPAR-IDLEVNR-601                          
103300       MOVE ZERO             TO SPAR-IDFTG                                
103400                                SPAR-IDFKNGRP                             
103500                                SPAR-ADLAGOMR                             
103600                                SPAR-ADGANG                               
103700                                SPAR-ADPLATS                              
103800     END-IF                                                               
103900     .                                                                    
104000     EJECT                                                                
104100 BGAAB-HAEMTA-BENA-INFO  SECTION.                                         
104200     MOVE SPAR-IDARTNR          TO W-IDARTNR-WDD3                         
104300                                                                          
104400     MOVE SPAR-IDDC             TO WS-IDDC                                
104500     IF CDC-SE                                                            
104600       MOVE 'S  '               TO W-IDSKYLT                              
104700     ELSE                                                                 
104800       MOVE 'GB '               TO W-IDSKYLT                              
104900     END-IF                                                               
105000                                                                          
105100     PERFORM IMS-GU-BENA11                                                
105200     IF SEGMENT-FINNS                                                     
105300         MOVE BENA11-TEXT-BEART TO SPAR-BEART                             
105400      ELSE                                                                
105500         MOVE SPACE             TO SPAR-BEART                             
105600     END-IF                                                               
105700     .                                                                    
105800     EJECT                                                                
105900 BGAC-BERAEKNA-ANKDAG   SECTION.                                          
106000     IF SPAR-TIAVIDAT > ZERO                                              
106100       MOVE 002               TO WORK-KDCALL                              
106200       MOVE SPAR-KVDAGAR-TTC1 TO WORK-KVWORKD                             
106300       MOVE SPAR-IDDC         TO WORK-IDDC                                
106400       MOVE SPAR-TIAVIDAT     TO WORK-TIAAMMDD-FOM                        
106500       CALL WORKDAY    USING WORK-KDCALL,                                 
106600                             WORK-DATE-AREA,                              
106700                             WORK-KDSVAR                                  
106800       IF WORK-KDSVAR-OK                                                  
106900          MOVE WORK-TIAAMMDD-NEXT-WORKDAY TO SPAR-TIANKDAG                
107000       ELSE                                                               
107100          MOVE SPAR-TIAVIDAT              TO SPAR-TIANKDAG                
107200       END-IF                                                             
107300     END-IF                                                               
107400     .                                                                    
107500     EJECT                                                                
107600 BGAD-KONTROLLERA-ART-INFO SECTION.                                       
107700     IF SPAR-KDERS             < 20    AND                                
107800        SPAR-PRARTSTD          > ZERO                                     
107900         CONTINUE                                                         
108000      ELSE                                                                
108100         MOVE JA               TO SPAR-PARTI-FLFEL                        
108200                                  SPAR-FS-FLFEL                           
108300     END-IF                                                               
108400                                                                          
108500     IF W-IDLEVNR = '8888 ' OR                                            
108600         'BW5JA' OR '16466' OR '6492 ' OR '1003 ' OR '1304 ' OR           
108700         'BWLAA' OR 'BZFFA' OR 'BP2TH' OR 'BKMJA' OR '12054'              
108800       CONTINUE                                                           
108900     ELSE                                                                 
109000* FELFLAGGAR OM AVROP SAKNAS(TILLÄGG SAMBAND MED EVEREST)                 
109100       MOVE W-IDARTNR     TO W-IDARTNR-D9                                 
109200       MOVE SPAR-IDDC     TO W-IDDC                                       
109300       PERFORM IMS-GET-WDD902                                             
109400       IF SEGMENT-FINNS                                                   
109500         MOVE +2   TO W-KDAVROP                                           
109600         MOVE ZERO TO WS-KVAVROP                                          
109700         PERFORM IMS-GNP-WDD905                                           
109800         IF SEGMENT-FINNS                                                 
109900            IF KVAVROP < SPAR-KVAVIS                                      
110000               PERFORM UNTIL SEGMENT-SAKNAS OR                            
110100                      (WS-KVAVROP NOT < SPAR-KVAVIS)                      
110200                 ADD KVAVROP TO WS-KVAVROP                                
110300                 PERFORM IMS-GNP-WDD905                                   
110400               END-PERFORM                                                
110500               IF WS-KVAVROP < SPAR-KVAVIS                                
110600                  MOVE JA      TO SPAR-PARTI-FLFEL                        
110700                                  SPAR-FS-FLFEL                           
110800               END-IF                                                     
110900            END-IF                                                        
111000         ELSE                                                             
111100           MOVE JA             TO SPAR-PARTI-FLFEL                        
111200                                  SPAR-FS-FLFEL                           
111300         END-IF                                                           
111400       ELSE                                                               
111500         MOVE JA               TO SPAR-PARTI-FLFEL                        
111600                                  SPAR-FS-FLFEL                           
111700       END-IF                                                             
111800     END-IF                                                               
111900                                                                          
112000     IF W-FS-IDFTG             =  ZERO                                    
112100         MOVE SPAR-IDFTG       TO W-FS-IDFTG                              
112200      ELSE                                                                
112300         IF SPAR-IDFTG         =  W-FS-IDFTG                              
112400             CONTINUE                                                     
112500          ELSE                                                            
112600             MOVE JA           TO SPAR-PARTI-FLFEL                        
112700                                  SPAR-FS-FLFEL                           
112800         END-IF                                                           
112900     END-IF                                                               
113000     .                                                                    
113100     EJECT                                                                
113200 BGAD-SKAPA-INLA01  SECTION.                                              
113300                                                                          
113400     MOVE SPAR-IDDC            TO  INL-IDDC                               
113500     MOVE SPAR-IDLEVNR         TO  INL-IDLEVNR                            
113600     MOVE SPAR-IDFS            TO  INL-IDFS                               
113700     MOVE SPAR-TIAVIDAT        TO  INL-TIAVIDAT                           
113800     MOVE SPAR-FS-FLFEL        TO  INL-FLFEL                              
113900     MOVE SPACE                TO  INL-IDANALYS                           
114000                                   INL-IDKST                              
114100     MOVE ZERO                 TO  INL-IDARTNR                            
114200     MOVE SPAR-IDFTG           TO  INL-IDFTG                              
114300     MOVE ZERO                 TO  INL-IDKONTO                            
114400     MOVE SPAR-IDLBBET         TO  INL-IDLBBET                            
114500     MOVE 'R31'                TO  INL-KDINL                              
114600     MOVE SPAR-TIANKDAG        TO  INL-TIANKDAG                           
114700     MOVE ZERO                 TO  INL-TIINLMOT                           
114800                                   INL-IDSHIPM                            
114900     ADD +1                    TO  CHKP-ANT                               
115000     PERFORM IMS-ISRT-INLA01                                              
115100     IF SEGMENT-FINNS-REDAN                                               
115200         MOVE JA               TO INLA01-DUBLETT-SW                       
115300     END-IF                                                               
115400     MOVE JA                   TO INLA01-SW                               
115500     MOVE INL-W6D101           TO SPAR-INLA01-AREA                        
115600     .                                                                    
115700     EJECT                                                                
115800 BGAE-SKAPA-INLA11  SECTION.                                              
115900                                                                          
116000     PERFORM BGAEA-HAEMTA-KDLAGEMB                                        
116100     ADD +1                    TO  W-IDRADNR-INL-ACC                      
116200     MOVE W-IDRADNR-INL-ACC    TO  ART-IDRADNR-INL                        
116300                                   W-IDRADNR-INL                          
116400     MOVE SPAR-IDDC            TO  ART-IDDC                               
116500     MOVE SPAR-IDARTNR         TO  ART-IDARTNR                            
116600     MOVE SPAR-ADLAGOMR        TO  ART-ADLAGOMR                           
116700     MOVE SPAR-ADGANG          TO  ART-ADGANG                             
116800     MOVE SPAR-ADPLATS         TO  ART-ADPLATS                            
116900     MOVE SPAR-BEART           TO  ART-BEART                              
117000     MOVE SPAR-BEFT            TO  ART-BEFT                               
117100     MOVE SPAR-PARTI-FLFEL     TO  ART-FLFEL                              
117200     MOVE SPACE                TO  ART-ADTRDEST-KIT                       
117300     MOVE NEJ                  TO  ART-FLETIKETT                          
117400                                   ART-FLKVAFEL                           
117500                                   ART-FLKVAKAR                           
117600                                   ART-FLKLAR                             
117700                                   ART-FLANNULL                           
117800     MOVE SPAR-IDFKNGRP        TO  ART-IDFKNGRP                           
117900     MOVE ZERO                 TO  ART-IDLOPNRM                           
118000     MOVE SPAR-KDARTURS        TO  ART-KDARTURS                           
118100     MOVE SPAR-KDFARLIG        TO  ART-KDFARLIG                           
118200     MOVE ZERO                 TO  ART-KDINLPRIO                          
118300                                   ART-KDKVAANT                           
118400     MOVE SPAR-CL1-KDLAGEMB    TO  ART-KDLAGEMB                           
118500     MOVE SPAR-KDSORT          TO  ART-KDSORT                             
118600     MOVE SPAR-KDRT            TO  ART-KDRT                               
118700     MOVE SPAR-KVAVIS          TO  ART-KVAVIS                             
118800     MOVE ZERO                 TO  ART-KVAVIS-KIT                         
118900                                   ART-KVAVIS-PRIO                        
119000                                   ART-KVKVASEK-BER                       
119100                                   ART-KVKVASEK-VER                       
119200                                   ART-KVKVAPRIM-BER                      
119300                                   ART-KVKVAPRIM-VER                      
119400                                   ART-TIUPPDAT                           
119500     MOVE SPAR-KVMP            TO  ART-KVMP                               
119600     MOVE SPAR-PRARTSTD        TO  ART-PRARTSTD                           
119700     MOVE SPAR-VKART           TO  ART-VKART                              
119800     MOVE SPAR-VLARTNTO        TO  ART-VLARTNTO                           
119900                                                                          
120000     MOVE NEJ                  TO  ART-FLSPLPART                          
120100     MOVE SPACE                TO  ART-ADTRDEST                           
120200     MOVE SPACE                TO  ART-KDKVAINL                           
120300                                                                          
120400     ADD +1                    TO CHKP-ANT                                
120500     PERFORM IMS-ISRT-INLA11                                              
120600     .                                                                    
120700     EJECT                                                                
120800 BGAEA-HAEMTA-KDLAGEMB  SECTION.                                          
120900                                                                          
121000     MOVE W-CL1-IDARTNR-EMBQ3  TO W-CL1-ART-EMBQ3                         
121100                                                                          
121200                                                                          
121300     MOVE SPACE                 TO SPAR-CL1-KDLAGEMB                      
121400                                                                          
121500     MOVE 1                     TO EMB-IX                                 
121600     PERFORM UNTIL EMB-IX       >  TAB-EMBQ3-MAX OR                       
121700          TAB-KOD (EMB-IX)      = W-CL1-ART-EMBQ3                         
121800       ADD 1                    TO EMB-IX                                 
121900     END-PERFORM                                                          
122000                                                                          
122100     IF EMB-IX                  > TAB-EMBQ3-MAX                           
122200         CONTINUE                                                         
122300      ELSE                                                                
122400         IF TAB-KOD (EMB-IX)        =  W-CL1-ART-EMBQ3                    
122500             MOVE TAB-TEXT (EMB-IX) TO SPAR-CL1-KDLAGEMB                  
122600         END-IF                                                           
122700     END-IF                                                               
122800     .                                                                    
122900     EJECT                                                                
123000 BGB-BEHANDLA-TCO-POST  SECTION.                                          
123100                                                                          
123200     MOVE JA                   TO TCO-SW                                  
123300     MOVE NEJ                  TO TCO-FEL-SW                              
123400     MOVE TCO-7224             TO W-ANTAL-KOLLI                           
123500     MOVE TCO-6853             TO SPAR-KVINLART                           
123600                                                                          
123700     ADD  TCO-7224             TO TCO-ANTAL-KOLLI                         
123800                                                                          
123900     COMPUTE W-KVAVIS          =  W-KVAVIS +                              
124000                                 (W-ANTAL-KOLLI * SPAR-KVINLART)          
124100     IF SPAR-KVINLART NUMERIC AND                                         
124200        SPAR-KVINLART > ZERO                                              
124300       CONTINUE                                                           
124400     ELSE                                                                 
124500       MOVE JA                   TO TCO-FEL-SW                            
124600                                    W-FLFEL                               
124700     END-IF                                                               
124800     .                                                                    
124900     EJECT                                                                
125000 BGC-BEHANDLA-NCO-POST  SECTION.                                          
125100                                                                          
125200**   IF SPAR-BEFT = +40                                                   
125300* LÖSER PROBLEMET MED ATT MÅLERIETS CONTAINRAR BLIR DIV.KOLLI.            
125400**     MOVE SPACE TO NCO-7246                                             
125500**   END-IF                                                               
125600                                                                          
125700     IF TCO-OK                                                            
125800       IF  (NCO-7246(1:1) = 'G' OR 'M')                                   
125900         PERFORM BGCB-NOLLFYLL-KOLLI                                      
126000         MOVE +0                   TO FIX-MG-KVINLART                     
126100         MOVE WIN-AREA (17:990)    TO W-NCO-TAB                           
126200         MOVE +1                   TO NCO-IX                              
126300                                                                          
126400         PERFORM UNTIL NCO-IX      >  NCO-MAX                             
126500          IF NCO-IDOKOLLI-TAB(NCO-IX) (2:9) NOT =  SPACE                  
126600            PERFORM BGCA-NOLLFYLL-KOLLI                                   
126700            ADD SPAR-KVINLART TO FIX-MG-KVINLART                          
126800            ADD +1 TO NCO-ANTAL-KOLLI                                     
126900          END-IF                                                          
127000          ADD +1                 TO NCO-IX                                
127100         END-PERFORM                                                      
127200                                                                          
127300         PERFORM S10-SKAPA-INLA21                                         
127400                                                                          
127500       ELSE                                                               
127600         MOVE WIN-AREA (17:990)    TO W-NCO-TAB                           
127700         MOVE +1                   TO NCO-IX                              
127800         PERFORM UNTIL NCO-IX      >  NCO-MAX                             
127900          IF NCO-IDOKOLLI-TAB(NCO-IX) (2:9) NOT =  SPACE                  
128000            PERFORM BGCA-NOLLFYLL-KOLLI                                   
128100            PERFORM S10-SKAPA-INLA21                                      
128200            ADD +1 TO NCO-ANTAL-KOLLI                                     
128300          END-IF                                                          
128400          ADD +1                 TO NCO-IX                                
128500         END-PERFORM                                                      
128600                                                                          
128700       END-IF                                                             
128800                                                                          
128900     END-IF                                                               
129000     .                                                                    
129100     EJECT                                                                
129200 BGCA-NOLLFYLL-KOLLI        SECTION.                                      
129300                                                                          
129400     IF NCO-IDOKOLLI-TAB(NCO-IX) (2:9) NUMERIC                            
129500        CONTINUE                                                          
129600     ELSE                                                                 
129700        MOVE +10               TO NOLL-IX-IN                              
129800                                  NOLL-IX-UT                              
129900        PERFORM UNTIL NOLL-IX-IN = +1                                     
130000           IF NCO-IDOKOLLI-TAB(NCO-IX) (NOLL-IX-IN:1) NUMERIC             
130100              MOVE NCO-IDOKOLLI-TAB(NCO-IX) (NOLL-IX-IN:1) TO             
130200                   NCO-IDOKOLLI-TAB(NCO-IX) (NOLL-IX-UT:1)                
130300              SUBTRACT 1 FROM NOLL-IX-IN                                  
130400                              NOLL-IX-UT                                  
130500           ELSE                                                           
130600              SUBTRACT 1 FROM NOLL-IX-IN                                  
130700           END-IF                                                         
130800        END-PERFORM                                                       
130900        PERFORM UNTIL NOLL-IX-UT = +1                                     
131000           MOVE ZERO TO NCO-IDOKOLLI-TAB(NCO-IX) (NOLL-IX-UT:1)           
131100           SUBTRACT 1 FROM NOLL-IX-UT                                     
131200        END-PERFORM                                                       
131300     END-IF                                                               
131400     .                                                                    
131500 BGCB-NOLLFYLL-KOLLI        SECTION.                                      
131600                                                                          
131700     IF NCO-7246 (2:9) NUMERIC                                            
131800        CONTINUE                                                          
131900     ELSE                                                                 
132000        MOVE +10               TO NOLL-IX-IN                              
132100                                  NOLL-IX-UT                              
132200        PERFORM UNTIL NOLL-IX-IN = +1                                     
132300           IF NCO-7246 (NOLL-IX-IN:1) NUMERIC                             
132400              MOVE NCO-7246 (NOLL-IX-IN:1) TO                             
132500                   NCO-7246 (NOLL-IX-UT:1)                                
132600              SUBTRACT 1 FROM NOLL-IX-IN                                  
132700                              NOLL-IX-UT                                  
132800           ELSE                                                           
132900              SUBTRACT 1 FROM NOLL-IX-IN                                  
133000           END-IF                                                         
133100        END-PERFORM                                                       
133200        PERFORM UNTIL NOLL-IX-UT = +1                                     
133300           MOVE ZERO TO NCO-7246(NOLL-IX-UT:1)                            
133400           SUBTRACT 1 FROM NOLL-IX-UT                                     
133500        END-PERFORM                                                       
133600     END-IF                                                               
133700     .                                                                    
133800 BH-UPPDATERA-LEVERANSPLAN  SECTION.                                      
133900                                                                          
134000     MOVE SPAR-IDARTNR         TO LEVP-IDARTNR                            
134100     MOVE SPAR-IDLEVNR         TO LEVP-IDLEVNR                            
134200     MOVE 'T'                  TO LEVP-KDBEH                              
134300     MOVE SPAR-KVAVIS          TO LEVP-KVAVIS                             
134400     MOVE W-KVDAGAR-INLEV      TO LEVP-KVDAGAR-INLEV                      
134500     MOVE SPAR-TIAVIDAT        TO LEVP-TILEVBSK-AVS                       
134600     MOVE SPAR-TIANKDAG        TO LEVP-TILEVBSK-INL                       
134700     MOVE W-IDDC               TO LEVP-IDDC                               
134800                                                                          
134900     CALL W611LEVP USING LEVP-W611LEVP LEVP-INLB-PCB                      
135000     .                                                                    
135100     EJECT                                                                
135200 BI-UPPDATERA-INLA01        SECTION.                                      
135300                                                                          
135400     MOVE SPAR-IDLEVNR         TO W-D101KY-IDLEVNR                        
135500     MOVE SPAR-IDFS            TO W-D101KY-IDFS                           
135600     MOVE SPAR-TIAVIDAT        TO W-D101KY-TIAVIDAT                       
135700     PERFORM IMS-GHU-INLA01                                               
135800     MOVE SPAR-FS-FLFEL        TO INL-FLFEL                               
135900     PERFORM IMS-REPL-INLA01                                              
136000     ADD +1                    TO CHKP-ANT                                
136100     .                                                                    
136200     EJECT                                                                
136300 C-ERROR-PROCESSING SECTION.                                              
136400                                                                          
136500*    Trigger notification to FLS if the receiver is DC 11 which is        
136600*    identified by supplier id 1441 or BP2TW                              
136700     IF W-IDLEVNRDC = '1441' OR 'BP2TW'                                   
136800       MOVE WS-ADRESS-FLS         TO W-ADDISPABS                          
136900       PERFORM IMS-GU-WDGX0104                                            
137000       IF SEGMENT-FINNS                                                   
137100         IF 0104-FLCONN = YES OR JA                                       
137200           PERFORM CA-MOVE-ERROR-DTLS                                     
137300           PERFORM S21-SEND-OPEN                                          
137400           PERFORM S22-SEND-PUT-HEADER                                    
137500           PERFORM S23-SEND-PUT-DATA                                      
137600           PERFORM S24-SEND-CLOSE                                         
137700         END-IF                                                           
137800       END-IF                                                             
137900     END-IF                                                               
138000     .                                                                    
138100                                                                          
138200 CA-MOVE-ERROR-DTLS SECTION.                                              
138300                                                                          
138400     MOVE 0104-IDUSERKEY          TO WGC-USER-KEY                         
138500     COMPUTE WGC-USER-KEY-LENGTH =                                        
138600          FUNCTION LENGTH(FUNCTION TRIM (WGC-USER-KEY))                   
138700                                                                          
138800     MOVE SPAR-IDLEVNR-FULL       TO WGC-MFG                              
138900     COMPUTE WGC-MFG-LENGTH =                                             
139000          FUNCTION LENGTH(FUNCTION TRIM (WGC-MFG))                        
139100                                                                          
139200     MOVE SPAR-IDFS               TO WGC-DELIVERYNOTENUMBER               
139300     COMPUTE WGC-DELIVERYNOTENUMBER-LENGTH =                              
139400          FUNCTION LENGTH(FUNCTION TRIM (WGC-DELIVERYNOTENUMBER))         
139500                                                                          
139600     MOVE '20'                    TO WGC-ISSUEDATEYEAR(1:2)               
139700     MOVE SPAR-TIAVIDAT(1:2)      TO WGC-ISSUEDATEYEAR(3:2)               
139800     COMPUTE WGC-ISSUEDATEYEAR-LENGTH =                                   
139900          FUNCTION LENGTH(FUNCTION TRIM (WGC-ISSUEDATEYEAR))              
140000                                                                          
140100     IF W-IDLEVNRDC = '1441'                                              
140200       MOVE 'BP2TW'               TO WGC-SHIPTO                           
140300     ELSE                                                                 
140400       MOVE INL-IDSHIPM           TO WGC-SHIPTO                           
140500     END-IF                                                               
140600     COMPUTE WGC-SHIPTO-LENGTH =                                          
140700          FUNCTION LENGTH(FUNCTION TRIM (WGC-SHIPTO))                     
140800                                                                          
140900     MOVE 1                        TO WGC-ERRORS2-NUM                     
141000     PERFORM VARYING W-IX FROM 1 BY 1 UNTIL W-IX > WGC-ERRORS2-NUM        
141100                                                                          
141200       MOVE 'E01'              TO WGC-ERRORCODE(W-IX)                     
141300       COMPUTE WGC-ERRORCODE-LENGTH(W-IX) =                               
141400            FUNCTION LENGTH(FUNCTION TRIM(WGC-ERRORCODE(W-IX)))           
141500                                                                          
141600       MOVE 'Error in ASN processing. Update required in PULS'            
141700                               TO WGC-ERRORMESSAGE(W-IX)                  
141800       COMPUTE WGC-ERRORMESSAGE-LENGTH(W-IX) =                            
141900            FUNCTION LENGTH(FUNCTION TRIM(WGC-ERRORMESSAGE(W-IX)))        
142000                                                                          
142100     END-PERFORM                                                          
142200     .                                                                    
142300                                                                          
142400 Z-FINIT SECTION.                                                         
142500                                                                          
142600     PERFORM IMS-LAS-ATERSTART                                            
142700     MOVE ZERO                 TO CKPA-6014-KVPOST                        
142800     ACCEPT CKPA-6014-TIUPPDAT FROM DATE                                  
142900     ACCEPT CKPA-6014-TIUPPTID FROM TIME                                  
143000     IF SEGMENT-SAKNAS                                                    
143100       MOVE '1'                TO CKPA-6014-KDSEGKEY                      
143200       PERFORM IMS-ISRT-ATERSTART                                         
143300     ELSE                                                                 
143400       PERFORM IMS-REPL-ATERSTART                                         
143500     END-IF                                                               
143600                                                                          
143700     CLOSE W61101                                                         
143800           W61101UT                                                       
143900     SKIP2                                                                
144000     MOVE 'S' TO POSTSUM-OPKOD                                            
144100     CALL POSTSUM USING POSTSUM-PARM                                      
144200     .                                                                    
144300     EJECT                                                                
144400 S01-LAES-W61101  SECTION.                                                
144500     SKIP2                                                                
144600     READ W61101 INTO WIN-AREA                                            
144700     AT END                                                               
144800        SET END-OF-W61101      TO TRUE                                    
144900                                                                          
145000     NOT AT END                                                           
145100        MOVE 'W61101'          TO POSTSUM-FDNAMN                          
145200        MOVE 'W61101D1'        TO POSTSUM-DDNAMN2                         
145300        MOVE POSTTYP           TO POSTSUM-TRANSTYP                        
145400        CALL POSTSUM USING POSTSUM-PARM                                   
145500        ADD +1                 TO W-KVPOST                                
145600                                                                          
145700     END-READ                                                             
145800     .                                                                    
145900     EJECT                                                                
146000 S02-SKRIV-UTFIL SECTION.                                                 
146100     WRITE UT-POST FROM UT-AREA                                           
146200                                                                          
146300     MOVE 'W61101U1'  TO POSTSUM-FDNAMN                                   
146400     MOVE 'W61101D2' TO POSTSUM-DDNAMN2                                   
146500     CALL POSTSUM USING POSTSUM-PARM                                      
146600     .                                                                    
146700     EJECT                                                                
146800 S05-BEHANDLA-MID-POST SECTION.                                           
146900                                                                          
147000     MOVE +1 TO RED-IX-IN                                                 
147100                RED-IX-UT                                                 
147200     MOVE SPACE TO SPAR-IDFS                                              
147300                                                                          
147400     PERFORM UNTIL RED-IX-IN > 8                                          
147500       IF MID-1004(RED-IX-IN:1) = SPACE OR ZERO                           
147600         CONTINUE                                                         
147700       ELSE                                                               
147800         PERFORM UNTIL RED-IX-IN > 8                                      
147900           MOVE MID-1004(RED-IX-IN:1) TO SPAR-IDFS(RED-IX-UT:1)           
148000           ADD +1 TO RED-IX-IN                                            
148100                     RED-IX-UT                                            
148200         END-PERFORM                                                      
148300       END-IF                                                             
148400       ADD +1 TO RED-IX-IN                                                
148500     END-PERFORM                                                          
148600                                                                          
148700     MOVE MID-2007             TO SPAR-TIAVIDAT                           
148800                                  W-D101KY-TIAVIDAT                       
148900     MOVE FUNCTION CURRENT-DATE(3:6) TO SPAR-DATUM                        
149000     COMPUTE W-DA-YYMMDD = SPAR-DATUM - 10000                             
149100     IF SPAR-TIAVIDAT NUMERIC AND SPAR-TIAVIDAT > ZERO AND                
149200        SPAR-TIAVIDAT NOT > SPAR-DATUM AND                                
149300        SPAR-TIAVIDAT > W-DA-YYMMDD                                       
149400       MOVE SPAR-TIAVIDAT TO DAT-I-TIDATUM                                
149500       MOVE 'AAMMDD' TO DAT-KDDATFORM                                     
149600                                                                          
149700       CALL WDATKONV USING DAT-KDDATFORM,                                 
149800                           DAT-I-TIDATUM,                                 
149900                           DAT-O-TIDATUM,                                 
150000                           DAT-KDSVAR                                     
150100       IF DAT-KDSVAR-OK                                                   
150200         CONTINUE                                                         
150300       ELSE                                                               
150400         MOVE FUNCTION CURRENT-DATE(3:6) TO SPAR-TIAVIDAT                 
150500                                            W-D101KY-TIAVIDAT             
150600       END-IF                                                             
150700     ELSE                                                                 
150800       MOVE FUNCTION CURRENT-DATE(3:6) TO SPAR-TIAVIDAT                   
150900                                          W-D101KY-TIAVIDAT               
151000     END-IF                                                               
151100     .                                                                    
151200     EJECT                                                                
151300 S10-SKAPA-INLA21  SECTION.                                               
151400                                                                          
151500     MOVE JA                   TO KOLLI-SW                                
151600                                  INLA21-SW                               
151700     IF TCO-FINNS                                                         
151800         PERFORM S10A-KOLLA-OM-KOLLI-FINNS                                
151900     END-IF                                                               
152000                                                                          
152100     IF TCO-FINNS AND KOLLI-OK                                            
152200         IF FOERSTA-NCO OR FLERA-MKOLLI                                   
152300           IF FLERA-MKOLLI                                                
152400             MOVE +2                        TO W-IDRADNR                  
152500             PERFORM IMS-GHU-INLA21                                       
152600             IF SEGMENT-SAKNAS                                            
152700               MOVE SPACE              TO UT-TEMEMO                       
152800               MOVE 'FEL PÅ KOLLINIVÅ' TO UT-TEMEMO(2:16)                 
152900               PERFORM S02-SKRIV-UTFIL                                    
153000               PERFORM IMS-ROLLBACK                                       
153100               GOBACK                                                     
153200             END-IF                                                       
153300           ELSE                                                           
153400             MOVE +2                        TO  RAD-IDRADNR               
153500             MOVE NEJ                       TO  FOERSTA-NCO-SW            
153600           END-IF                                                         
153700          ELSE                                                            
153800             ADD  +1                        TO  RAD-IDRADNR               
153900         END-IF                                                           
154000                                                                          
154100         IF NCO-7246(1:1) = 'M' OR 'G'                                    
154200           IF FLERA-MKOLLI                                                
154300             ADD  FIX-MG-KVINLART           TO  RAD-KVINLART              
154400                                             W-INLA21-KVINLART            
154500           ELSE                                                           
154600             MOVE FIX-MG-KVINLART           TO  RAD-KVINLART              
154700             ADD FIX-MG-KVINLART            TO  W-INLA21-KVINLART         
154800             MOVE NCO-7246(2:9)             TO  RAD-IDOKOLLI              
154900           END-IF                                                         
155000         ELSE                                                             
155100           MOVE SPAR-KVINLART               TO  RAD-KVINLART              
155200           ADD  SPAR-KVINLART               TO  W-INLA21-KVINLART         
155300           MOVE NCO-IDOKOLLI-TAB(NCO-IX) (2:9)                            
155400                                            TO  RAD-IDOKOLLI              
155500         END-IF                                                           
155600         MOVE SPAR-IDLEVNR                  TO  RAD-IDLEVNR-KOLLI         
155700      ELSE                                                                
155800         MOVE +1                            TO  RAD-IDRADNR               
155900         MOVE SPACE                         TO  RAD-IDLEVNR-KOLLI         
156000         MOVE ZERO                          TO  RAD-IDOKOLLI              
156100         IF KOLLI-FEL                                                     
156200***************************************************************           
156300*** ÄNDRAT FÖR ATT FÅ ORDNING PÅ AVISERINGAR FRÅN DP5UA SOM GETT          
156400** FEL ANTAL PÅ W6D121-SEGMENT EFTERSOM KOLLIIDENTITET REDAN FINNS        
156500*          IF NCO-7246(1:1) = 'M' OR 'G'                                  
156600*            MOVE FIX-MG-KVINLART           TO  RAD-KVINLART              
156700*            ADD FIX-MG-KVINLART            TO  W-INLA21-KVINLART         
156800*          ELSE                                                           
156900*            MOVE SPAR-KVINLART             TO  RAD-KVINLART              
157000*            ADD SPAR-KVINLART              TO  W-INLA21-KVINLART         
157100*          END-IF                                                         
157200*         ELSE                                                            
157300             MOVE SPAR-KVAVIS               TO  RAD-KVINLART              
157400                                                W-INLA21-KVINLART         
157500*            ADD SPAR-KVAVIS                TO  W-INLA21-KVINLART         
157600***************************************************************           
157700         END-IF                                                           
157800     END-IF                                                               
157900                                                                          
158000     MOVE SPACE                         TO  RAD-ADINLOMR                  
158100                                            RAD-ADINLOMR-NXT              
158200     IF NCO-7246(1:1) = 'G' AND RAD-IDRADNR > +1                          
158300       MOVE JA                          TO  RAD-FLDIVKLI                  
158400     ELSE                                                                 
158500       MOVE NEJ                         TO  RAD-FLDIVKLI                  
158600     END-IF                                                               
158700     MOVE NEJ                           TO  RAD-FLKVAANT                  
158800                                            RAD-FLPRIO                    
158900                                            RAD-FLSATS                    
159000                                            RAD-FLINLFB                   
159100                                            RAD-FLINLFP                   
159200                                            RAD-FLSVSLS                   
159300     MOVE ZERO                          TO  RAD-IDANSTNR                  
159400                                            RAD-IDILIRAD                  
159500                                            RAD-IDILIST                   
159600                                            RAD-IDINLVGN                  
159700                                            RAD-KDINLPRIO                 
159800                                            RAD-TIUPPDAT                  
159900     MOVE 'AVI'                         TO  RAD-KDINLSTA                  
160000                                                                          
160100     ADD +1                    TO CHKP-ANT                                
160200     IF FLERA-MKOLLI AND KOLLI-OK                                         
160300       PERFORM IMS-REPL-INLA21                                            
160400     ELSE                                                                 
160500       PERFORM IMS-ISRT-INLA21                                            
160600     END-IF                                                               
160700     IF SEGMENT-FINNS-REDAN AND KOLLI-OK                                  
160800         IF RAD-IDRADNR           =  1                                    
160900             MOVE +1              TO W-IDRADNR                            
161000             PERFORM IMS-GHU-INLA21                                       
161100             COMPUTE RAD-KVINLART = RAD-KVINLART + SPAR-KVINLART          
161200             ADD +1               TO CHKP-ANT                             
161300             PERFORM IMS-REPL-INLA21                                      
161400         ELSE                                                             
161500             MOVE JA TO W-FLFEL                                           
161600         END-IF                                                           
161700     END-IF                                                               
161800     .                                                                    
161900     EJECT                                                                
162000 S10A-KOLLA-OM-KOLLI-FINNS  SECTION.                                      
162100                                                                          
162200     MOVE JA                  TO FOERSTA-MKOLLI-SW                        
162300                                                                          
162400     IF (NCO-7246(1:1) = 'M' OR 'G')                                      
162500      IF NCO-7246(2:9) NOT NUMERIC                                        
162600       MOVE NEJ  TO KOLLI-SW                                              
162700      ELSE                                                                
162800       MOVE SPAR-IDLEVNR        TO W-IDLEVNR-KOLLI                        
162900                                   W-IDLEVNRK                             
163000       MOVE NCO-7246(2:9)       TO W-IDOKOLLI                             
163100                                   W-IDOKOLLINR                           
163200                                                                          
163300       PERFORM IMS-GU-INLA11-C                                            
163400       IF SEGMENT-FINNS                                                   
163500         IF NCO-7246(1:1) = 'M'                                           
163600           MOVE NEJ                 TO FOERSTA-MKOLLI-SW                  
163700         END-IF                                                           
163800         MOVE ART-IDARTNR TO WS-ART-IDARTNR                               
163900         PERFORM IMS-GNP-W6D101                                           
164000                                                                          
164100         IF NCO-7246(1:1) = 'G' OR                                        
164200           (NCO-7246(1:1) = 'M' AND WS-ART-IDARTNR = SPAR-IDARTNR         
164300                                AND INL-IDFS    = SPAR-IDFS               
164400                                AND INL-TIAVIDAT = SPAR-TIAVIDAT)         
164500           PERFORM IMS-GU-INLA11-C                                        
164600           PERFORM UNTIL NOT SEGMENT-FINNS                                
164700             IF ART-IDLOPNRM > +0                                         
164800               MOVE NEJ TO KOLLI-SW                                       
164900             ELSE                                                         
165000               PERFORM IMS-GNP-INLA21-C                                   
165100               IF RAD-FLDIVKLI = NEJ AND NCO-7246(1:1) = 'G' OR           
165200                  RAD-FLDIVKLI = JA  AND NCO-7246(1:1) = 'M'              
165300                 MOVE NEJ TO KOLLI-SW                                     
165400               END-IF                                                     
165500             END-IF                                                       
165600             PERFORM IMS-GN-INLA11-C                                      
165700           END-PERFORM                                                    
165800         ELSE                                                             
165900           MOVE NEJ               TO KOLLI-SW                             
166000         END-IF                                                           
166100       END-IF                                                             
166200      END-IF                                                              
166300     ELSE                                                                 
166400       IF NCO-IDOKOLLI-TAB(NCO-IX) (2:1) NUMERIC AND                      
166500          NCO-IDOKOLLI-TAB(NCO-IX) (2:9) NOT NUMERIC                      
166600           MOVE NEJ                 TO KOLLI-SW                           
166700        ELSE                                                              
166800           MOVE SPAR-IDLEVNR        TO W-IDLEVNR-KOLLI                    
166900                                       W-IDLEVNRK                         
167000           MOVE NCO-IDOKOLLI-TAB(NCO-IX) (2:9) TO W-IDOKOLLI              
167100                                                  W-IDOKOLLINR            
167200           PERFORM IMS-GU-INLA11-C                                        
167300           IF SEGMENT-FINNS                                               
167400               MOVE NEJ               TO KOLLI-SW                         
167500           END-IF                                                         
167600       END-IF                                                             
167700     END-IF                                                               
167800     .                                                                    
167900     EJECT                                                                
168000 S11-SKAPA-INLA21-RAD1  SECTION.                                          
168100                                                                          
168200     MOVE +1                        TO  RAD-IDRADNR                       
168300     MOVE SPAR-KVAVIS               TO  RAD-KVINLART                      
168400                                                                          
168500     MOVE SPACE                     TO  RAD-ADINLOMR                      
168600                                        RAD-ADINLOMR-NXT                  
168700                                        RAD-IDLEVNR-KOLLI                 
168800     MOVE NEJ                       TO  RAD-FLDIVKLI                      
168900                                        RAD-FLKVAANT                      
169000                                        RAD-FLPRIO                        
169100                                        RAD-FLSATS                        
169200                                        RAD-FLINLFB                       
169300                                        RAD-FLINLFP                       
169400                                        RAD-FLSVSLS                       
169500     MOVE ZERO                      TO  RAD-IDANSTNR                      
169600                                        RAD-IDOKOLLI                      
169700                                        RAD-IDILIRAD                      
169800                                        RAD-IDILIST                       
169900                                        RAD-IDINLVGN                      
170000                                        RAD-KDINLPRIO                     
170100                                        RAD-TIUPPDAT                      
170200     MOVE 'AVI'                     TO  RAD-KDINLSTA                      
170300                                                                          
170400     ADD +1                    TO CHKP-ANT                                
170500     PERFORM IMS-ISRT-INLA21                                              
170600     MOVE JA                   TO INLA21-SW                               
170700     .                                                                    
170800     EJECT                                                                
170900 S21-SEND-OPEN SECTION.                                                   
171000                                                                          
171100     MOVE WS-ADRESS-FLS                   TO SEND-ADDISPABS               
171200     MOVE 'OPEN'                          TO SEND-KDFUNC                  
171300     CALL WZ01SEND USING SEND-CONTROL-AREA                                
171400                         SEND-OPEN-AREA                                   
171500     IF SEND-KDRC > ZERO                                                  
171600       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
171700       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
171800       DELIMITED BY SIZE INTO FELTEXT-STR                                 
171900       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
172000     END-IF                                                               
172100     .                                                                    
172200                                                                          
172300 S22-SEND-PUT-HEADER SECTION.                                             
172400     MOVE 0104-IDAPI             TO IDAPI                                 
172500     COMPUTE IDAPI-LEN            = FUNCTION LENGTH (                     
172600                                    FUNCTION TRIM (IDAPI))                
172700     MOVE 0104-IDPATH-API        TO IDPATH-API                            
172800     COMPUTE IDPATH-API-LEN       = FUNCTION LENGTH (                     
172900                                    FUNCTION TRIM (IDPATH-API))           
173000     MOVE 0104-IDPTYP-API        TO IDPTYP-API                            
173100     COMPUTE IDPTYP-API-LEN       = FUNCTION LENGTH (                     
173200                                    FUNCTION TRIM (IDPTYP-API))           
173300                                                                          
173400     MOVE 'PUT'                  TO SEND-KDFUNC                           
173500     MOVE LENGTH OF WAPIINFO     TO SEND-KVDLEN                           
173600     CALL WZ01SEND            USING SEND-CONTROL-AREA                     
173700                                    SEND-KVDLEN                           
173800                                    WAPIINFO                              
173900     IF SEND-KDRC > ZERO                                                  
174000       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
174100       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
174200       DELIMITED BY SIZE INTO FELTEXT-STR                                 
174300       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
174400     END-IF                                                               
174500     .                                                                    
174600                                                                          
174700 S23-SEND-PUT-DATA SECTION.                                               
174800     MOVE 'PUT'                           TO SEND-KDFUNC                  
174900     MOVE LENGTH OF WGC-ERR-DATA          TO SEND-KVDLEN                  
175000     CALL WZ01SEND USING SEND-CONTROL-AREA                                
175100                         SEND-KVDLEN                                      
175200                         WGC-ERR-DATA                                     
175300     IF SEND-KDRC > ZERO                                                  
175400       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
175500       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
175600       DELIMITED BY SIZE INTO FELTEXT-STR                                 
175700       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
175800     END-IF                                                               
175900     .                                                                    
176000                                                                          
176100 S24-SEND-CLOSE SECTION.                                                  
176200     MOVE 'CLOSE'                         TO SEND-KDFUNC                  
176300     CALL WZ01SEND USING SEND-CONTROL-AREA                                
176400     IF SEND-KDRC > ZERO                                                  
176500       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
176600       STRING 'WZ01SEND CLOSE ERROR RC=' KDRC-DISPLAY                     
176700       DELIMITED BY SIZE INTO FELTEXT-STR                                 
176800       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
176900     END-IF                                                               
177000     .                                                                    
177100 X-TAG-CHECKPOINT   SECTION.                                              
177200                                                                          
177300     MOVE ZERO                 TO CHKP-ANT                                
177400     PERFORM IMS-LAS-ATERSTART                                            
177500     IF SEGMENT-SAKNAS                                                    
177600         MOVE LOW-VALUE        TO CKPA-6014-W6GX6014                      
177700     END-IF                                                               
177800     MOVE W-KVPOST             TO CKPA-6014-KVPOST                        
177900     ACCEPT CKPA-6014-TIUPPDAT FROM DATE                                  
178000     ACCEPT CKPA-6014-TIUPPTID FROM TIME                                  
178100     IF SEGMENT-SAKNAS                                                    
178200         MOVE '1'              TO CKPA-6014-KDSEGKEY                      
178300         PERFORM IMS-ISRT-ATERSTART                                       
178400     ELSE                                                                 
178500         PERFORM IMS-REPL-ATERSTART                                       
178600     END-IF                                                               
178700     PERFORM IMS-CHECKPOINT                                               
178800     .                                                                    
178900     EJECT                                                                
179000* --- IMS SEKTIONER ---                                                   
179100     SKIP3                                                                
179200 IMS-RESTART SECTION.                                                     
179300     SKIP2                                                                
179400     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
179500     MOVE '  ' TO GODK-STATUSKODER                                        
179600     CALL CBLTDLI USING XRST MSG-PCB                                      
179700                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
179800                        CHKP-AREA-LENGTH CHKP-AREA                        
179900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
180000     PERFORM IMS-STATUSKONTROLL                                           
180100     .                                                                    
180200     EJECT                                                                
180300 IMS-REPL-ATERSTART SECTION.                                              
180400     SKIP2                                                                
180500     MOVE SPACE TO GODK-STATUSKODER                                       
180600     CALL CBLTDLI USING REPL CKPA-PCB DLI-IO-AREA                         
180700     MOVE CKPA-STATUS-CODE TO STATUS-WS                                   
180800     PERFORM IMS-STATUSKONTROLL                                           
180900     .                                                                    
181000     EJECT                                                                
181100 IMS-CHECKPOINT SECTION.                                                  
181200     SKIP2                                                                
181300     MOVE IDPGM                TO CHKP-MSG-IO-AREA                        
181400     MOVE '  XD'               TO GODK-STATUSKODER                        
181500     CALL CBLTDLI USING CHKP MSG-PCB                                      
181600                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
181700                        CHKP-AREA-LENGTH CHKP-AREA                        
181800     MOVE MSG-STATUS-CODE      TO STATUS-WS                               
181900     PERFORM IMS-STATUSKONTROLL                                           
182000     IF IMS-EJ-OK                                                         
182100       DISPLAY 'IMS-KONTROLLREGION EJ TILLGÄNGLIG'                        
182200       CALL FELLOG                                                        
182300     END-IF                                                               
182400     .                                                                    
182500     SKIP2                                                                
182600 IMS-LAS-ATERSTART SECTION.                                               
182700     SKIP2                                                                
182800     STRING 'W6CKPA01(W6GXKEY  =' W-W6GX-6013-KEY-X ')'                   
182900            DELIMITED BY SIZE INTO SSA1                                   
183000     STRING 'W6CKPA11(KDSEGKEY =' W-W6GX-6014-KEY-X ')'                   
183100            DELIMITED BY SIZE INTO SSA2                                   
183200     MOVE '  GE' TO GODK-STATUSKODER                                      
183300     CALL CBLTDLI USING GHU CKPA-PCB DLI-IO-AREA SSA1 SSA2                
183400     MOVE CKPA-STATUS-CODE TO STATUS-WS                                   
183500     PERFORM IMS-STATUSKONTROLL                                           
183600     .                                                                    
183700     EJECT                                                                
183800 IMS-ISRT-ATERSTART SECTION.                                              
183900     SKIP2                                                                
184000     STRING 'W6CKPA01(W6GXKEY  =' W-W6GX-6013-KEY-X ')'                   
184100          DELIMITED BY SIZE INTO SSA1                                     
184200     MOVE 'W6CKPA11' TO SSA2                                              
184300     MOVE '  ' TO GODK-STATUSKODER                                        
184400     CALL CBLTDLI USING ISRT CKPA-PCB DLI-IO-AREA SSA1 SSA2               
184500     MOVE CKPA-STATUS-CODE TO STATUS-WS                                   
184600     PERFORM IMS-STATUSKONTROLL                                           
184700     .                                                                    
184800     EJECT                                                                
184900 IMS-GU-WDB601-LEVNR-DC SECTION.                                          
185000     STRING 'WDB601  (IDLEVNDC =' W-IDLEVNRDC ')'                         
185100          DELIMITED BY SIZE INTO SSA1                                     
185200     MOVE '  GE'   TO GODK-STATUSKODER                                    
185300     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
185400     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
185500     PERFORM IMS-STATUSKONTROLL                                           
185600     .                                                                    
185700     EJECT                                                                
185800 IMS-GU-ARTC01 SECTION.                                                   
185900     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
186000          DELIMITED BY SIZE INTO SSA1                                     
186100     MOVE '  GE' TO GODK-STATUSKODER                                      
186200     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA SSA1                      
186300     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
186400     PERFORM IMS-STATUSKONTROLL                                           
186500     .                                                                    
186600     SKIP3                                                                
186700 IMS-GNP-ARTC11 SECTION.                                                  
186800     MOVE 'WLARTC11 ' TO SSA1                                             
186900     MOVE '  GE' TO GODK-STATUSKODER                                      
187000     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA SSA1                     
187100     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
187200     PERFORM IMS-STATUSKONTROLL                                           
187300     .                                                                    
187400     SKIP3                                                                
187500 IMS-GNP-ARTC23 SECTION.                                                  
187600     MOVE 'WLARTC11 ' TO SSA1                                             
187700     MOVE 'WLARTC23 ' TO SSA2                                             
187800     MOVE '  GE' TO GODK-STATUSKODER                                      
187900     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA SSA1 SSA2                
188000     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
188100     PERFORM IMS-STATUSKONTROLL                                           
188200     .                                                                    
188300     SKIP3                                                                
188400 IMS-GU-WDK711 SECTION.                                                   
188500     STRING 'WDK701  (IDARTNR = ' W-IDARTNR-X ')'                         
188600          DELIMITED BY SIZE INTO SSA1                                     
188700     STRING 'WDK711  (IDDC    = ' W-IDDC   ')'                            
188800          DELIMITED BY SIZE INTO SSA2                                     
188900     MOVE '  GE' TO GODK-STATUSKODER                                      
189000     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2               
189100     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
189200     PERFORM IMS-STATUSKONTROLL                                           
189300     .                                                                    
189400     EJECT                                                                
189500 IMS-GU-WDK712 SECTION.                                                   
189600     STRING 'WDK701  (IDARTNR = ' W-IDARTNR-X ')'                         
189700          DELIMITED BY SIZE INTO SSA1                                     
189800     STRING 'WDK712  (IDLAND  = ' W-IDLAND-X ')'                          
189900          DELIMITED BY SIZE INTO SSA2                                     
190000     MOVE '  ' TO GODK-STATUSKODER                                        
190100     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK712 SSA1 SSA2               
190200     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
190300     PERFORM IMS-STATUSKONTROLL                                           
190400     .                                                                    
190500     EJECT                                                                
190600 IMS-GU-BENA11 SECTION.                                                   
190700     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
190800          DELIMITED BY SIZE INTO SSA1                                     
190900     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
191000          DELIMITED BY SIZE INTO SSA2                                     
191100     MOVE '  GE' TO GODK-STATUSKODER                                      
191200     CALL CBLTDLI USING GU BENA-PCB DLI-IO-AREA SSA1 SSA2                 
191300     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
191400     PERFORM IMS-STATUSKONTROLL                                           
191500     .                                                                    
191600     SKIP3                                                                
191700 IMS-ISRT-INLA01 SECTION.                                                 
191800                                                                          
191900     MOVE 'W6INLA01 ' TO SSA1                                             
192000     MOVE '  II' TO GODK-STATUSKODER                                      
192100     CALL CBLTDLI USING ISRT INLA-PCB DLI-IO-AREA SSA1                    
192200     MOVE INLA-STATUS-CODE TO STATUS-WS                                   
192300     PERFORM IMS-STATUSKONTROLL                                           
192400     .                                                                    
192500     SKIP3                                                                
192600 IMS-GHU-INLA01 SECTION.                                                  
192700                                                                          
192800     STRING 'W6INLA01(W6D101KY =' W-W6D101KY-X ')'                        
192900          DELIMITED BY SIZE INTO SSA1                                     
193000     MOVE '  GE' TO GODK-STATUSKODER                                      
193100     CALL CBLTDLI USING GHU INLA-PCB DLI-IO-AREA SSA1                     
193200     MOVE INLA-STATUS-CODE TO STATUS-WS                                   
193300     PERFORM IMS-STATUSKONTROLL                                           
193400     .                                                                    
193500     SKIP3                                                                
193600 IMS-GU-INLA01 SECTION.                                                   
193700                                                                          
193800     STRING 'W6INLA01(W6D101KY =' W-W6D101KY-X ')'                        
193900          DELIMITED BY SIZE INTO SSA1                                     
194000     MOVE '  ' TO GODK-STATUSKODER                                        
194100     CALL CBLTDLI USING GU INLA-PCB DLI-IO-AREA SSA1                      
194200     MOVE INLA-STATUS-CODE TO STATUS-WS                                   
194300     PERFORM IMS-STATUSKONTROLL                                           
194400     .                                                                    
194500     SKIP3                                                                
194600 IMS-GNP-W6D101 SECTION.                                                  
194700                                                                          
194800     MOVE 'W6INLA01 ' TO SSA1                                             
194900     MOVE '  ' TO GODK-STATUSKODER                                        
195000     CALL CBLTDLI USING GNP INLD-PCB DLI-IO-AREA SSA1                     
195100     MOVE INLD-STATUS-CODE TO STATUS-WS                                   
195200     PERFORM IMS-STATUSKONTROLL                                           
195300     .                                                                    
195400     SKIP3                                                                
195500 IMS-REPL-INLA01 SECTION.                                                 
195600                                                                          
195700     MOVE '    ' TO GODK-STATUSKODER                                      
195800     CALL CBLTDLI USING REPL INLA-PCB DLI-IO-AREA                         
195900     MOVE INLA-STATUS-CODE TO STATUS-WS                                   
196000     PERFORM IMS-STATUSKONTROLL                                           
196100     .                                                                    
196200     EJECT                                                                
196300 IMS-GHU-INLA11 SECTION.                                                  
196400                                                                          
196500     STRING 'W6INLA01(W6D101KY =' W-W6D101KY-X ')'                        
196600          DELIMITED BY SIZE INTO SSA1                                     
196700     STRING 'W6INLA11(IDRADNRI =' W-IDRADNR-INL-X ')'                     
196800          DELIMITED BY SIZE INTO SSA2                                     
196900     MOVE '  ' TO GODK-STATUSKODER                                        
197000     CALL CBLTDLI USING GHU INLA-PCB DLI-IO-AREA SSA1 SSA2                
197100     MOVE INLA-STATUS-CODE TO STATUS-WS                                   
197200     PERFORM IMS-STATUSKONTROLL                                           
197300     .                                                                    
197400     SKIP3                                                                
197500 IMS-REPL-INLA11 SECTION.                                                 
197600                                                                          
197700     MOVE '  ' TO GODK-STATUSKODER                                        
197800     CALL CBLTDLI USING REPL INLA-PCB DLI-IO-AREA                         
197900     MOVE INLA-STATUS-CODE TO STATUS-WS                                   
198000     PERFORM IMS-STATUSKONTROLL                                           
198100     .                                                                    
198200     SKIP3                                                                
198300 IMS-ISRT-INLA11 SECTION.                                                 
198400                                                                          
198500     STRING 'W6INLA01(W6D101KY =' W-W6D101KY-X ')'                        
198600          DELIMITED BY SIZE INTO SSA1                                     
198700     MOVE 'W6INLA11 ' TO SSA2                                             
198800     MOVE '  ' TO GODK-STATUSKODER                                        
198900     CALL CBLTDLI USING ISRT INLA-PCB DLI-IO-AREA SSA1 SSA2               
199000     MOVE INLA-STATUS-CODE TO STATUS-WS                                   
199100     PERFORM IMS-STATUSKONTROLL                                           
199200     .                                                                    
199300     SKIP3                                                                
199400 IMS-GHU-INLA21 SECTION.                                                  
199500                                                                          
199600     STRING 'W6INLA01(W6D101KY =' W-W6D101KY-X ')'                        
199700          DELIMITED BY SIZE INTO SSA1                                     
199800     STRING 'W6INLA11(IDRADNRI =' W-IDRADNR-INL-X ')'                     
199900          DELIMITED BY SIZE INTO SSA2                                     
200000     STRING 'W6INLA21(IDRADNR  =' W-IDRADNR-X ')'                         
200100          DELIMITED BY SIZE INTO SSA3                                     
200200     MOVE '  GE' TO GODK-STATUSKODER                                      
200300     CALL CBLTDLI USING GHU INLA-PCB DLI-IO-AREA SSA1 SSA2 SSA3           
200400     MOVE INLA-STATUS-CODE TO STATUS-WS                                   
200500     PERFORM IMS-STATUSKONTROLL                                           
200600     .                                                                    
200700     EJECT                                                                
200800 IMS-ISRT-INLA21 SECTION.                                                 
200900                                                                          
201000     STRING 'W6INLA01(W6D101KY =' W-W6D101KY-X ')'                        
201100          DELIMITED BY SIZE INTO SSA1                                     
201200     STRING 'W6INLA11(IDRADNRI =' W-IDRADNR-INL-X ')'                     
201300          DELIMITED BY SIZE INTO SSA2                                     
201400     MOVE 'W6INLA21 ' TO SSA3                                             
201500     MOVE '  II' TO GODK-STATUSKODER                                      
201600     CALL CBLTDLI USING ISRT INLA-PCB DLI-IO-AREA SSA1 SSA2 SSA3          
201700     MOVE INLA-STATUS-CODE TO STATUS-WS                                   
201800     PERFORM IMS-STATUSKONTROLL                                           
201900     .                                                                    
202000     EJECT                                                                
202100 IMS-REPL-INLA21 SECTION.                                                 
202200                                                                          
202300     MOVE '    ' TO GODK-STATUSKODER                                      
202400     CALL CBLTDLI USING REPL INLA-PCB DLI-IO-AREA                         
202500     MOVE INLA-STATUS-CODE TO STATUS-WS                                   
202600     PERFORM IMS-STATUSKONTROLL                                           
202700     .                                                                    
202800     EJECT                                                                
202900 IMS-GU-INLA11-C SECTION.                                                 
203000     STRING 'W6INLA11(W6D1CSEQ =' W-W6D1CSEQ-X ')'                        
203100          DELIMITED BY SIZE INTO SSA1                                     
203200     MOVE '  GE' TO GODK-STATUSKODER                                      
203300     CALL CBLTDLI USING GU INLD-PCB DLI-IO-AREA SSA1                      
203400     MOVE INLD-STATUS-CODE TO STATUS-WS                                   
203500     PERFORM IMS-STATUSKONTROLL                                           
203600     .                                                                    
203700     SKIP3                                                                
203800 IMS-GN-INLA11-C SECTION.                                                 
203900     STRING 'W6INLA11(W6D1CSEQ =' W-W6D1CSEQ-X ')'                        
204000          DELIMITED BY SIZE INTO SSA1                                     
204100     MOVE '  GEGB' TO GODK-STATUSKODER                                    
204200     CALL CBLTDLI USING GN INLD-PCB DLI-IO-AREA SSA1                      
204300     MOVE INLD-STATUS-CODE TO STATUS-WS                                   
204400     PERFORM IMS-STATUSKONTROLL                                           
204500     .                                                                    
204600     SKIP3                                                                
204700 IMS-GNP-INLA21-C SECTION.                                                
204800     STRING 'W6INLA21(IDLEVNRK =' W-IDLEVNRK-C-X                          
204900                    '&IDOKOLLI =' W-IDOKOLLI-C-X ')'                      
205000          DELIMITED BY SIZE INTO SSA1                                     
205100     MOVE '  GE' TO GODK-STATUSKODER                                      
205200     CALL CBLTDLI USING GNP INLD-PCB DLI-IO-AREA SSA1                     
205300     MOVE INLD-STATUS-CODE TO STATUS-WS                                   
205400     PERFORM IMS-STATUSKONTROLL                                           
205500     .                                                                    
205600     SKIP3                                                                
205700 IMS-GU-WDF101 SECTION.                                                   
205800     STRING 'WDF101  (IDLEVNR  =' W-IDLEVNR-X ')'                         
205900          DELIMITED BY SIZE INTO SSA1                                     
206000     MOVE '  GE' TO GODK-STATUSKODER                                      
206100     CALL CBLTDLI USING GU WDF1-PCB DLI-IO-WDF101 SSA1                    
206200     MOVE WDF1-STATUS-CODE TO STATUS-WS                                   
206300     PERFORM IMS-STATUSKONTROLL                                           
206400     .                                                                    
206500     SKIP3                                                                
206600 IMS-GU-WDF116 SECTION.                                                   
206700     STRING 'WDF101  (IDLEVNR  =' W-IDLEVNR-X ')'                         
206800          DELIMITED BY SIZE INTO SSA1                                     
206900     STRING 'WDF116  (IDDC     =' W-IDDC   ')'                            
207000          DELIMITED BY SIZE INTO SSA2                                     
207100     MOVE '  GE' TO GODK-STATUSKODER                                      
207200     CALL CBLTDLI USING GU WDF1-PCB DLI-IO-WDF116 SSA1 SSA2               
207300     MOVE WDF1-STATUS-CODE TO STATUS-WS                                   
207400     PERFORM IMS-STATUSKONTROLL                                           
207500     .                                                                    
207600     EJECT                                                                
207700 IMS-GET-WDD902 SECTION.                                                  
207800     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
207900          DELIMITED BY SIZE INTO SSA1                                     
208000     STRING 'WDD902  (IDLEVNR  =' W-IDLEVNR-X ')'                         
208100          DELIMITED BY SIZE INTO SSA2                                     
208200     MOVE '  GE' TO GODK-STATUSKODER                                      
208300     CALL CBLTDLI USING GU WDD9-PCB DLI-IO-WDD9 SSA1 SSA2                 
208400     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
208500     PERFORM IMS-STATUSKONTROLL                                           
208600     .                                                                    
208700     SKIP3                                                                
208800 IMS-GNP-WDD905 SECTION.                                                  
208900     STRING 'WDD905  (KDAVROP  =' W-KDAVROP-X ')'                         
209000          DELIMITED BY SIZE INTO SSA1                                     
209100     MOVE '  GE' TO GODK-STATUSKODER                                      
209200     CALL CBLTDLI USING GNP WDD9-PCB DLI-IO-WDD9 SSA1                     
209300     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
209400     PERFORM IMS-STATUSKONTROLL                                           
209500     .                                                                    
209600     SKIP3                                                                
209700 IMS-GU-WDGX0104  SECTION.                                                
209800                                                                          
209900     STRING 'WDR501  (WDGXKEY  =' W-WDGXKEY-0103-X ')'                    
210000          DELIMITED BY SIZE INTO SSA1                                     
210100     STRING 'WDGX0104(ADDISPAB =' W-KY0104-X ')'                          
210200          DELIMITED BY SIZE INTO SSA2                                     
210300     MOVE '  '   TO GODK-STATUSKODER                                      
210400     CALL CBLTDLI USING GU ATAB-PCB DLI-IO-WDGX0104 SSA1 SSA2             
210500     MOVE ATAB-STATUS-CODE TO STATUS-WS                                   
210600     PERFORM IMS-STATUSKONTROLL                                           
210700     .                                                                    
210800 IMS-ROLLBACK    SECTION.                                                 
210900     SKIP2                                                                
211000     CALL CBLTDLI USING ROLB    MSG-PCB                                   
211100     .                                                                    
211200     SKIP2                                                                
211300 IMS-STATUSKONTROLL SECTION.                                              
211400     SKIP2                                                                
211500     SET STATUS-IX TO 1                                                   
211600     SEARCH GODK-STATUS                                                   
211700       AT END                                                             
211800         MOVE 'FEL VID DL1 ANROP' TO FELTEXT-STR                          
211900         DISPLAY FELTEXT                                                  
212000         CALL FELLOG                                                      
212100       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
212200     END-SEARCH                                                           
212300     .                                                                    
212400     EJECT                                                                
