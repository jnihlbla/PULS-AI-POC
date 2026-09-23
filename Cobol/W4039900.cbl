000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4039900.                                                
000300 AUTHOR.         BERT ANDERSSON.                                          
000400 DATE-WRITTEN.   AUGUSTI 97.                                              
000500 DATE-COMPILED.                                                           
000600                                                                          
000700                                                                          
000800*    FUNKTION.                                                            
000900*        W4039900. DIREKT-LEVERANTÖRS-PACKNING.                           
001000*        PACK-RAPPORTERING AV ETT STYCK DIREKT-LEVERANS KOLLI OCH         
001100*        OM DET ÄR SISTA (ELLER DEN ENDA) RADEN/ERNA ÄVEN                 
001200*        AVSLUTNING AV PACK-RAPPORTERINGEN.                               
001300*                                                                         
001400*        DESSUTOM RAPPORTERAS UPPGIFTER OM KOLLIT.                        
001500*                                                                         
001600*        W4039900 ÄR KONSTRUERAT MED VALDA DELAR UR PROGRAM               
001700*        W40315 (PACKRAPPORTERING), W40397 (AVSLUT PACKNING) OCH          
001800*        W40663/65 (LASTNING) SAMT NYA REGLER FÖR USA-DIR.LEV.            
001900*                                                                         
002000*        SOFTWARE RADER FRÅN PIE SYSTEMET PACKAS - XTRA MID               
002100*        ANVÄNDS MED KOLL PÅ ANSTNR=01441.                                
002200*                                                                         
002300*        OBS!! OMSTART EFTER ABEND SKER FRÅN DISPATCH BILD 0622.          
002400*        SÖK EFTER NODE "D-LEV P" PÅ BILD 0622.                           
002500*                                                                         
002600*    INDATA.                                                              
002700*        TRANSAKTION: W4T399X                                             
002800*        MID:         W4I39901                                            
002900*                     W4I39902                                            
003000*                     WMSGKOM                                             
003100*                                                                         
003200*    UTDATA.                                                              
003300*        MOD:         WMSGKOM                                             
003400*    CHANGE LOG                                                           
003500*                                                                         
003600*    DIGAMBAR/021011                                                      
003700*    STRUCTURE OF ACTION TRANSACTION 4487 IS CHANGED TO IMPROVE           
003800*    THE RESPONSE TIME OF THE SCREEN 4312.                                
003900*                                                                         
004000 DATA DIVISION.                                                           
004100                                                                          
004200 WORKING-STORAGE SECTION.                                                 
004300*    -- CHECKED BY WY2000                                                 
004400     SKIP3                                                                
004500 77    IDPGM                     PIC X(8)    VALUE 'W4039900'.            
004600 77    FELTEXT                   PIC X(24)   VALUE SPACE.                 
004700 77    WS-PGM-POSITION           PIC X(24)   VALUE SPACE.                 
004800 77    JA                        PIC X       VALUE 'J'.                   
004900 77    YES                       PIC X       VALUE 'Y'.                   
005000 77    NEJ                       PIC X       VALUE 'N'.                   
005100 77    RAETT                     PIC X       VALUE 'R'.                   
005200 77    FEL                       PIC X       VALUE 'F'.                   
005300 77    ETT                       PIC S9(9)   VALUE +1.                    
005400 77    FILLER                    PIC X(8)    VALUE 'AAAAAAAA'.            
005500 77    IDPSN-IX                  PIC S9(5)   VALUE +0   COMP-3.           
005600 77    FG-INDX                   PIC S9(5)   VALUE +0   COMP-3.           
005700 77    FG-MAX-INDX               PIC S9(5)   VALUE +10  COMP-3.           
005800 77    IN-RAD-INDX               PIC S9(5)   VALUE +0.                    
005900 77    MAX-MOD-LAENGD            PIC S9(4)   VALUE +366 COMP SYNC.        
006000 77    MIN-MOD-LAENGD            PIC S9(4)   VALUE +82  COMP SYNC.        
006100 77    INX-TOT-ANT-RADER         PIC S9(3)   VALUE ZERO  COMP-3.          
006200 77    SENASTE-IDRADNR           PIC  9(4)   VALUE ZERO.                  
006300 77    AKTUELLT-IDRADNR          PIC  9(4)   VALUE ZERO.                  
006400 77    WS-IDRADNR                PIC  9(4)   VALUE ZERO.                  
006500 77    WS-IDRADNR-SPAR           PIC  9(4)   VALUE ZERO.                  
006600 77    FILLER                    PIC X(8)    VALUE 'BBBBBBBB'.            
006700 77    WS-ANT-KLARA-RADER        PIC S9(3)   VALUE +0    COMP-3.          
006800 77    WS-KORD-KDORDKL           PIC S9(1)   VALUE +0    COMP-3.          
006900 77    WS-KORD-IDORDER           PIC S9(7)  COMP-3.                       
007000 77    WS-KORD-SUORDV            PIC S9(9)V9(2).                          
007100 77    WS-KORD-SUORDV-LOC        PIC S9(9)V9(2).                          
007200 77    WS-KORD-SUORDV-LOCPREL    PIC S9(9)V9(2).                          
007300 77    WS-KORD-KDVALISO          PIC X(3)   VALUE SPACE.                  
007400 77    WS-KDMFSFOR               PIC 9(1)   VALUE ZERO.                   
007500 77    WS-DAGENS-DATUM           PIC 9(6)   VALUE ZERO.                   
007600 77    WS-HHMMSSDD               PIC 9(8)   VALUE ZERO.                   
007700 01     WS-HHMMSSDD-RED.                                                  
007800   03   WS-HHMMSS               PIC  9(6).                                
007900   03   WS-DD                   PIC  9(2).                                
008000 77    FILLER                    PIC X(8)    VALUE 'CCCCCCCC'.            
008100 77    WS-IDANSTNR               PIC X(5)   VALUE SPACE.                  
008200 77    WS-IDDISTR                PIC 9(5)   VALUE ZERO.                   
008300 77    WS-IDKUNDNR               PIC X(6)   VALUE SPACE.                  
008400 77    WS-IDKUNDNR-NUM           PIC 9(6)   VALUE ZERO.                   
008500 77    WS-IDKOLLI-NUM            PIC 9(5)   VALUE ZERO.                   
008600 77    WS-IDPRODNR               PIC 9(7)   VALUE ZERO.                   
008700 77    WS-KOLLI-VLORDBTO         PIC 9(4)V9(3)  VALUE ZERO.               
008800 77    FILLER                    PIC X(8)    VALUE 'DDDDDDDD'.            
008900 77    WS-DIKOLLIL               PIC 9(4)   VALUE ZERO.                   
009000 77    WS-DIKOLLIB               PIC 9(3)   VALUE ZERO.                   
009100 77    WS-DIKOLLIH               PIC 9(3)   VALUE ZERO.                   
009200 77    WS-ADFLGEO                PIC X(3)   VALUE SPACE.                  
009300 77    WS-ADFLOMR                PIC 9(3)   VALUE ZERO.                   
009400 77    WS-ADRUTNIV               PIC 9(3)   VALUE ZERO.                   
009500 77    FILLER                    PIC X(8)    VALUE 'EEEEEEEE'.            
009600 77    MAX-RAD-ANTAL             PIC S9(3)  VALUE +0  COMP-3.             
009700 77    DLEV-RAD-ANTAL            PIC S9(3)  VALUE +0  COMP-3.             
009800*                                        ANTAL FÄRDIGPACKADE RADER        
009900*                                        I ETT RAD-INTERVALL.             
010000 77    WS-DARFS                  PIC 9(12)  VALUE ZERO.                   
010100 77    WS-KDORDSTA               PIC X(2)    VALUE SPACE.                 
010200 77    WS-FLAUTFAK               PIC X(01).                               
010300 77    WS-IDSKEPPN               PIC 9(7)    VALUE  0.                    
010400 77    WS-IDFAKT-GNB             PIC X(08)   VALUE SPACE.                 
010500 77    WS-IDLBBET                PIC X(12)   VALUE 'DIRECT      '.        
010600                                                                          
010700 77    WS-IDORDNR5               PIC X(05)   VALUE SPACE.                 
010800 77    WS-KDFAKTYP               PIC X(01)   VALUE SPACE.                 
010900 77    WS-MID-KVLEVART           PIC S9(7)   VALUE +0.                    
011000 77    WS-KVLEVART               PIC X(7)    VALUE SPACE.                 
011100 77    KOLLI-VALUE-ETT           PIC 9(5)    VALUE 00001.                 
011200                                                                          
011300 77  RKOD-ABEND                  PIC S9(4)  VALUE +33   COMP SYNC.        
011400 77  IX                          PIC S9(9)  VALUE ZERO  COMP SYNC.        
011500 77  IX-MAX                      PIC S9(9)  VALUE +7    COMP SYNC.        
011600 77  W-IDLANDX2                  PIC X(2)   VALUE SPACE.                  
011700                                                                          
011800 77  ERROR-TEXT                  PIC X(80)  VALUE SPACE.                  
011900 77  KDRC-DISP                   PIC 9(4)   VALUE ZERO.                   
012000 77  WS-IDCOM                    PIC S9(9)  VALUE ZERO COMP-3.            
012100                                                                          
012200 01  WS-TID-X.                                                            
012300     03 WS-TISKPTID                PIC 9(6)       VALUE ZERO.             
012400     03 WS-TISKPTID-GRP            REDEFINES WS-TISKPTID.                 
012500       05 WS-TISKPTID-HHMM         PIC 9(4).                              
012600       05 WS-TISKPTID-SS           PIC 9(2).                              
012700                                                                          
012800 01  WS-DCUSER.                                                           
012900     03 FILLER                   PIC X(5)   VALUE 'WIDDC'.                
013000     03 WS-DCUSER-IDDC           PIC X(2)   VALUE SPACE.                  
013100     03 FILLER                   PIC X(1)   VALUE SPACE.                  
013200                                                                          
013300 01    WS-TID-W.                                                          
013400   03  WS-TTMMSS                 PIC 9(6).                                
013500   03  WS-HH                     PIC 9(2).                                
013600 77    FILLER                    PIC X(8)    VALUE 'GGGGGGGG'.            
013700*                                                                         
013800 77  FARLIGT-GODS            PIC X            VALUE 'N'.                  
013900       88 FARLIGT-GODS-FINNS                  VALUE 'J'.                  
014000*                                                                         
014100       EJECT                                                              
014200 77    FILLER                    PIC X(8)    VALUE 'TRANS'.               
014300 77    WS-IDTRANS                PIC X(04).                               
014400   88  WS-GODKAND-TRANS                     VALUE '4399'                  
014500                                                  '0622'                  
014600                                                  '0693'.                 
014700*                                                                         
014800 77    WS-INDATA-TEST            PIC X(01).                               
014900   88  WS-INDATA-RATT                       VALUE 'R'.                    
015000*                                                                         
015100 01     FILLER                  PIC X(10)   VALUE 'ACC-AREA '.            
015200 01     ACC-AREA.                                                         
015300   03   ACC-AREA-GRP.                                                     
015400     05 ACC-ORAD-VKARTNTO       PIC  9(4)V9(3)    VALUE ZERO.             
015500     05 ACC-ORAD-VLARTNTO       PIC  9(6)V9(1)    VALUE ZERO.             
015600     05 ACC-ORAD-PRARTNTO       PIC  S9(9)V9(2)   VALUE ZERO.             
015700     05 ACC-ORAD-PRARTNTO-LOC   PIC  S9(9)V9(2)   VALUE ZERO.             
015800     05 ACC-ORAD-PRARTNTO-LOCPREL  PIC  S9(9)V9(2)   VALUE ZERO.          
015900     05 ACC-KOLLI-VKORDNTO      PIC  9(4)V9(3)    VALUE ZERO.             
016000     05 ACC-KOLLI-KVFLAMP       PIC  S9(2)V9(1)   VALUE ZERO.             
016100     05 ACC-KOLLI-KDFARLIG      PIC  S9           VALUE ZERO.             
016200     05 ACC-KOLLI-KVORDRAD      PIC  S9(5)        VALUE ZERO.             
016300     05 ACC-KOLLI-KVFALRAD      PIC  S9(5)        VALUE ZERO.             
016400     05 ACC-KOLLI-SUORDV        PIC  S9(9)V9(2)   VALUE ZERO.             
016500     05 ACC-KOLLI-SUORDV-LOC    PIC  S9(9)V9(2)   VALUE ZERO.             
016600     05 ACC-KOLLI-SUORDV-LOCPREL PIC  S9(9)V9(2)   VALUE ZERO.            
016700     05 ACC-KOLLI-KDVALISO      PIC  X(3)         VALUE SPACE.            
016800     SKIP2                                                                
016900*                                                                         
017000 01     FILLER                  PIC X(10)   VALUE 'WS-ORAD-'.             
017100 01     WS-ORAD.                                                          
017200   03   WS-ORAD-UPPG-AREA.                                                
017300     05 WS-ORAD-KVFLAMP         PIC  S9(2)V9(1)   VALUE ZERO.             
017400     05 WS-ORAD-KDFARLIG        PIC  S9           VALUE ZERO.             
017500     05 WS-ORAD-KVLEVART        PIC  S9(7)        VALUE ZERO.             
017600*                                                                         
017700   03   WS-ORAD-AREA.                                                     
017800     05 WS-ORAD-PRARTNTO        PIC  S9(9)V9(2)   VALUE ZERO.             
017900     05 WS-ORAD-PRARTNTO-LOC    PIC  S9(9)V9(2)   VALUE ZERO.             
018000     05 WS-ORAD-PRARTNTO-LOCPREL  PIC  S9(9)V9(2)   VALUE ZERO.           
018100     05 WS-ORAD-VKARTNTO        PIC  S9(4)V9(3)   VALUE ZERO.             
018200     05 WS-ORAD-VLARTNTO        PIC  S9(6)V9(1)   VALUE ZERO.             
018300     05 WS-ORAD-KDVALISO        PIC  X(3)         VALUE SPACE.            
018400*                                                                         
018500   03   SPAR-FARLIGT-GODS-DATA.                                           
018600     05 WS-ORAD-IDPSN           PIC  9(3)                VALUE 0.         
018700     05 WS-ORAD-VKART-FG        PIC  S9(7)        COMP-3 VALUE 0.         
018800     05 WS-ORAD-VLFG            PIC  S9(4)V9(3)   COMP-3 VALUE 0.         
018900     05 WS-ORAD-SUEQFG          PIC  S9(3)V9(4)   COMP-3 VALUE 0.         
019000     05 TOTAL-SUEQFG            PIC  S9(3)V9(4)   COMP-3 VALUE 0.         
019100     EJECT                                                                
019200*                                                                         
019300 01     DYNAMISKA-SUBPROGRAM.                                             
019400     03 CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.             
019500     03 FELLOG                  PIC X(8)    VALUE 'FELLOG  '.             
019600     03 ABEND                   PIC X(8)    VALUE 'ABEND   '.             
019700     03 WDATKONV                PIC X(8)    VALUE 'WDATKONV'.             
019800     03 W005INIT                PIC X(8)    VALUE 'W005INIT'.             
019900     03 WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.             
020000     03 W476SHNO                PIC X(8)    VALUE 'W476SHNO'.             
020100     SKIP2                                                                
020200 01    ABENDKODER.                                                        
020300     03 FILLER                  PIC X(16) VALUE 'ABENDKODER'.             
020400     03 RKOD-ABEND-UTAN-DUMP    PIC S9(4) COMP SYNC VALUE +16.            
020500     03 RKOD-ABEND-MED-DUMP     PIC S9(4) COMP SYNC VALUE +33.            
020600     03 RKOD-FELTEXT            PIC X(32) VALUE SPACE.                    
020700     SKIP2                                                                
020800*SVARSKODER TILL DISPATCHER, BILD 0622.                                   
020900 01  MESSAGE-CODES.                                                       
021000     03  ERR-ORDER-FINNS         PIC X(3)    VALUE '703'.                 
021100     03  ERR-FORMELLT-FEL        PIC X(3)    VALUE '094'.                 
021200     03  ERR-LOGISKT-FEL         PIC X(3)    VALUE '095'.                 
021300     03  ERR-SAKNAS-KREG         PIC X(3)    VALUE '063'.                 
021400     03  ERR-TRANSPORT-FEL       PIC X(3)    VALUE '087'.                 
021500     03  ERR-KUND-SPAERRAD       PIC X(3)    VALUE '213'.                 
021600     03  ERR-MOMS-REGNR-FEL      PIC X(3)    VALUE '223'.                 
021700     03  OK-BEHANDLAD            PIC X(3)    VALUE '101'.                 
021800     03  ERR-ORDERRAD-SAKN       PIC X(3)    VALUE '029'.                 
021900     03  ERR-ORDER-SAKNAS        PIC X(3)    VALUE '054'.                 
022000     03  ERR-ORDERHUVUD-SAKNAS   PIC X(3)    VALUE '417'.                 
022100     03  ERR-OVER-LEVERANS       PIC X(3)    VALUE '197'.                 
022200     03  ERR-FELAKTIGA-RADER     PIC X(3)    VALUE '751'.                 
022300     03  ERR-FEL-ANTAL           PIC X(3)    VALUE '181'.                 
022400     EJECT                                                                
022500*                                                                         
022600*    --- AREOR TILL GEMENSAMMA SUBPROGRAM                                 
022700*                                                                         
022800 01  FILLER                     PIC X(16)   VALUE 'WMSGINIT '.            
022900*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
023000*01 -COPY WMSGINIT                                                        
023100*                                                                         
023200 01  FILLER                     PIC X(16)   VALUE 'WZ01SEND '.            
023300*    --- PARAMETRAR TILL SUBPROGRAM WZ01SEND                              
023400*01 -COPY WZ01SEND                                                        
023500*                                                                         
023600 01  FILLER                      PIC X(16)   VALUE 'W476SHNO'.            
023700*    --- PARAMETRAR TILL SUBPROGRAM W476SHNO                              
023800*01  -COPY W476SHNO                                                       
023900 01  FILLER                     PIC X(16)   VALUE 'LÄNKAREOR'.            
024000*                                                                         
024100 01     FILLER                  PIC X(11)   VALUE 'HJALP-AREOR'.          
024200 01     HJALP-ODEL-TIRFS        PIC 9(11).                                
024300 01     FILLER                  REDEFINES HJALP-ODEL-TIRFS.               
024400   03   HJALP-ODEL-TIRFS-7      PIC  X(7).                                
024500   03   FILLER                  PIC  X(4).                                
024600     SKIP2                                                                
024700 01     HJALP-4472-TIRFS        PIC 9(11).                                
024800 01     FILLER                  REDEFINES HJALP-4472-TIRFS.               
024900   03   HJALP-4472-TIRFS-7      PIC  X(7).                                
025000   03   FILLER                  PIC  X(4).                                
025100     EJECT                                                                
025200 01     TEST-IDDISTR            PIC 9(5)              COMP-3.             
025300 01     FILLER REDEFINES TEST-IDDISTR.                                    
025400*  03   -COPY WWDIST03.                                                   
025500     SKIP2                                                                
025600*    ----DISTR-DEALER-PRICE-----                                          
025700 01     FILLER REDEFINES TEST-IDDISTR.                                    
025800*  03   -COPY WWDIST79.                                                   
025900     SKIP2                                                                
026000***************************************************************           
026100*                                                                         
026200*01  WDATAREA      -COPY WDATAREA.                                        
026300     EJECT                                                                
026400***************************************************************           
026500 01    NYCKLAR-TILL-DLI.                                                  
026600*                                                                         
026700*  03    -COPY WDGX01                                                     
026800*                                                                         
026900   03    W-WDE401-KUNDORDER-X.                                            
027000     05    W-401-IDDISTR         PIC S9(5)   VALUE ZERO  COMP-3.          
027100     05    W-401-IDKUNDNR        PIC S9(7)   VALUE ZERO  COMP-3.          
027200     05    W-401-IDKUNDRF.                                                
027300       07  W-401-IDORDNR         PIC  9(5)   VALUE ZERO.                  
027400       07  FILLER                PIC X(05)   VALUE SPACE.                 
027500     05    W-401-IDPRODNR        PIC S9(7)   VALUE ZERO  COMP-3.          
027600     05    W-401-IDPLKLST        PIC S9(3)   VALUE ZERO  COMP-3.          
027700*                                                                         
027800   03    W-WDE4B-KEYSEQ-X.                                                
027900     05    W-420-IDPRODNR        PIC S9(7)   VALUE ZERO  COMP-3.          
028000     05    W-420-IDPURAD         PIC S9(5)   VALUE ZERO  COMP-3.          
028100                                                                          
028200   03    W-WDE411-IDPURAD-X.                                              
028300     05    W-420-IDPURAD2        PIC S9(5)   VALUE ZERO  COMP-3.          
028400*                                                                         
028500   03    W-WDE421-IDKOLLI-X.                                              
028600     05    W-421-IDPRODNR        PIC S9(7)   VALUE ZERO  COMP-3.          
028700     05    W-421-IDKOLLI         PIC S9(5)   VALUE ZERO  COMP-3.          
028800*                                                                         
028900   03    W-WDE601-IDPRODNR-X.                                             
029000     05    W-601-IDPRODNR        PIC S9(7)   VALUE ZERO  COMP-3.          
029100*                                                                         
029200   03    W-WDE611-IDKOLLI-X.                                              
029300     05    W-610-IDKOLLI         PIC S9(5)   VALUE ZERO  COMP-3.          
029400*                                                                         
029500   03    W-WDE4E1KY-MAX-X.                                                
029600     05    W-IDPRODNR-WDE4E-MAX  PIC S9(7)   VALUE ZERO  COMP-3.          
029700     05    W-WDE4E1-MAX          PIC X(19)   VALUE HIGH-VALUE.            
029800                                                                          
029900   03    W-WDE4E1KY-MIN-X.                                                
030000     05    W-IDPRODNR-WDE4E-MIN  PIC S9(7)   VALUE ZERO  COMP-3.          
030100     05    W-WDE4E1-MIN          PIC X(19)   VALUE LOW-VALUE.             
030200                                                                          
030300     EJECT                                                                
030400     03  W-IDFAKLOP-X.                                                    
030500         05  W-IDFAKLOP           PIC S9(3)   VALUE ZERO  COMP-3.         
030600*                                                                         
030700     03  W-IDSKEPPN-X.                                                    
030800         05 W-IDSKEPPN            PIC S9(7)   COMP-3 VALUE ZERO.          
030900*                                                                         
031000   03    W-WDQ201-X.                                                      
031100     05    W-201-IDORDER         PIC S9(7) COMP-3.                        
031200*                                                                         
031300   03    W-WDQ212-IDDC-X.                                                 
031400     05    W-212-IDDC            PIC X(2).                                
031500*                                                                         
031600   03    W-WDQ301-KEY-X.                                                  
031700     05    W-WDQ301-IDORDER      PIC S9(7)   VALUE ZERO  COMP-3.          
031800     05    W-WDQ301-IDDC         PIC X(2).                                
031900     05    W-WDQ301-IDPRODNR     PIC S9(7)   VALUE ZERO  COMP-3.          
032000     05    W-WDQ301-IDPLKLST     PIC S9(3)   VALUE ZERO  COMP-3.          
032100*                                                                         
032200   03  W-Q301-KEY-MIN-X.                                                  
032300         05  W-Q301-MIN-IDORDER  PIC S9(7)   COMP-3.                      
032400         05  W-Q301-MIN-IDDC     PIC X(2).                                
032500         05  W-Q301-MIN-IDPRODNR PIC S9(7)   COMP-3.                      
032600         05  FILLER              PIC X(2)    VALUE LOW-VALUE.             
032700*                                                                         
032800   03  W-Q301-KEY-MAX-X.                                                  
032900         05  W-Q301-MAX-IDORDER  PIC S9(7)   COMP-3.                      
033000         05  W-Q301-MAX-IDDC     PIC X(2).                                
033100         05  W-Q301-MAX-IDPRODNR PIC S9(7)   COMP-3.                      
033200         05  FILLER              PIC X(2)    VALUE HIGH-VALUE.            
033300*                                                                         
033400   03  W-WDQ301KY-MIN.                                                    
033500         05  W-Q301KY-MIN-IDORDER    PIC S9(7)  COMP-3.                   
033600         05  W-Q301KY-MIN-IDDC       PIC X(2).                            
033700         05  FILLER                  PIC X(6)   VALUE LOW-VALUE.          
033800*                                                                         
033900   03  W-KDODELST                PIC X.                                   
034000*                                                                         
034100   03    W-IDARTNR-X.                                                     
034200     05    W-IDARTNR             PIC S9(9)   VALUE ZERO  COMP-3.          
034300*                                                                         
034400   03    W-KDSEGKEY-X.                                                    
034500     05    W-KDSEGKEY            PIC X(1)    VALUE '1'.                   
034600*                                                                         
034700     03  W-WDQ301KY-MAX.                                                  
034800         05  W-Q301KY-MAX-IDORDER    PIC S9(7)  COMP-3.                   
034900         05  W-Q301KY-MAX-IDDC       PIC X(2).                            
035000         05  FILLER                  PIC X(6)   VALUE HIGH-VALUE.         
035100*                                                                         
035200   03    W-4447-X.                                                        
035300     05    FILLER                PIC X(4)  VALUE '4447'.                  
035400     05    W-4447-IDDC           PIC X(2).                                
035500     05    FILLER                PIC X(24) VALUE LOW-VALUE.               
035600*                                                                         
035700   03    W-4448-X.                                                        
035800     05    W-4448-IDPRC          PIC X(4).                                
035900     05    FILLER                PIC X(1)  VALUE LOW-VALUE.               
036000*                                                                         
036100   03    W-4487-X.                                                        
036200     05    FILLER                PIC X(4)  VALUE '4487'.                  
036300     05    W-4487-IDDC           PIC X(2).                                
036400     05    FILLER                PIC X(24) VALUE LOW-VALUE.               
036500*                                                                         
036600   03    W-4488-X.                                                        
036700     05    W-4488-KDPRCGRP       PIC X(5).                                
036800*                                                                         
036900   03    W-4490-X.                                                        
037000     05    W-4490-DARFS          PIC 9(12).                               
037100     05    W-4490-IDPRODNR       PIC S9(7)  COMP-3.                       
037200     05    W-4490-IDPLKLST       PIC S9(3)  COMP-3.                       
037300     EJECT                                                                
037400                                                                          
037500   03  W-IDDC-B6-X.                                                       
037600       05 W-IDDC-B6                  PIC X(2).                            
037700                                                                          
037800 01      FILLER                   PIC X(16)   VALUE 'WDE2KEYS'.           
037900 01    W-IDSHIPM-X.                                                       
038000     03  W-IDSHIPM                PIC  9(7)   VALUE ZERO.                 
038100                                                                          
038200 01    W-WDE211KY-X.                                                      
038300     03  W-WDE211-IDDISTR    PIC S9(5)    VALUE ZERO COMP-3.              
038400     03  W-WDE211-IDKUNDNR   PIC S9(7)    VALUE ZERO COMP-3.              
038500     EJECT                                                                
038600 01      FILLER                   PIC X(16)   VALUE 'WDE1KEYS'.           
038700 01    W-WDE111KY-X.                                                      
038800     03  W-WDE111-IDDISTR    PIC S9(5)    VALUE ZERO COMP-3.              
038900     03  W-WDE111-IDKUNDNR   PIC S9(7)    VALUE ZERO COMP-3.              
039000     EJECT                                                                
039100*                                                                         
039200 01    FILLER                 PIC X(16) VALUE 'FG-TABELL'.                
039300                                                                          
039400 01    FG-TABELL.                                                         
039500   03    TAB-POST OCCURS 10.                                              
039600     05  TAB-IDPSN            PIC 9(3)              VALUE ZERO.           
039700     05  TAB-VKART-FG         PIC S9(7)      COMP-3 VALUE ZERO.           
039800     05  TAB-VLFG             PIC S9(4)V9(3) COMP-3 VALUE ZERO.           
039900     EJECT                                                                
040000******************************************************************        
040100*                                                                *        
040200*                AREOR FÖR MFS OCH SKÄRMHANTERING                *        
040300*                                                                *        
040400******************************************************************        
040500*                                                                         
040600*    --- AREOR FÖR MSG-IO                                                 
040700 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
040800 01     MID-AREA                PIC X(1000).                              
040900 01     FILLER REDEFINES MID-AREA.                                        
041000*  03   -COPY W4I39901.                                                   
041100     EJECT                                                                
041200 01     FILLER REDEFINES MID-AREA.                                        
041300*  03   -COPY W4I39902.                                                   
041400     EJECT                                                                
041500 01  FILLER                      PIC X(16)  VALUE 'MSG-AREA '.            
041600     SKIP3                                                                
041700*01  -COPY WMSGAREA.                                                      
041800*    EJECT                                                                
041900*01  -COPY W40636I1   -PRE MOD4636-                                       
042000     EJECT                                                                
042100 01  FILLER                      PIC X(16)   VALUE 'KOM-IO-AREA'.         
042200 01  KOM-IO-AREA.                                                         
042300*03  -COPY WMSGKOM                                                        
042400     EJECT                                                                
042500*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
042600*                                                                         
042700 01    IMS-WS.                                                            
042800   03    FILLER                  PIC X(16)   VALUE 'IMS-WS     '.         
042900     SKIP3                                                                
043000*                        **** STATUS-KOD FRÅN IMS                         
043100   03    STATUS-WS               PIC XX.                                  
043200     88    SEGMENT-FINNS                     VALUE '  '.                  
043300     88    SEGMENT-SLUT                      VALUE 'GB'.                  
043400     88    SEGMENT-SAKNAS                    VALUE 'GE'.                  
043500     88    SEGMENT-FINNS-REDAN               VALUE 'II'.                  
043600     SKIP3                                                                
043700   03    GODK-STATUSKODER.                                                
043800     05    GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.              
043900     SKIP3                                                                
044000 01    SSA1                      PIC X(96).                               
044100 01    SSA2                      PIC X(160).                              
044200 01    SSA3                      PIC X(96).                               
044300 01    SSA4                      PIC X(96).                               
044400     EJECT                                                                
044500*                            IMS FUNKTIONSKODER                           
044600*01    -COPY W0003                                                        
044700     EJECT                                                                
044800 01    FILLER    PIC X(25)  VALUE 'DLI INPUT-OUTPUT AREA1'.               
044900 01  FILLER                      PIC X(16)  VALUE 'WDE401-AR'.            
045000 01  -COPY WDE401                                                         
045100     EJECT                                                                
045200 01  FILLER                      PIC X(16)  VALUE 'WDE411-AR'.            
045300 01  -COPY WDE411                                                         
045400     EJECT                                                                
045500 01  FILLER                      PIC X(16)  VALUE 'WDE421-AR'.            
045600 01  -COPY WDE421                                                         
045700     EJECT                                                                
045800 01  FILLER                      PIC X(16)  VALUE 'WDE601-AR'.            
045900 01  -COPY WDE601                                                         
046000     EJECT                                                                
046100 01  FILLER                      PIC X(16)  VALUE 'WDE611-AR'.            
046200 01  -COPY WDE611                                                         
046300     EJECT                                                                
046400 01  FILLER                      PIC X(16)  VALUE '4490-AREA'.            
046500 01  -COPY WDGX4490                                                       
046600     EJECT                                                                
046700 01  FILLER                      PIC X(16)  VALUE '4512-AREA'.            
046800 01  -COPY WDGX4512                                                       
046900     EJECT                                                                
047000 01  FILLER                      PIC X(16)  VALUE 'WDQ3-AREA'.            
047100 01  -COPY WDQ301                                                         
047200     EJECT                                                                
047300 01  FILLER                      PIC X(16)  VALUE 'WDK9-AREA'.            
047400 01  -COPY WDK901                                                         
047500     EJECT                                                                
047600 01  FILLER                      PIC X(16)  VALUE 'WDK6-AREA'.            
047700 01  -COPY WDK611                                                         
047800     EJECT                                                                
047900 01  FILLER                      PIC X(16)  VALUE 'WDK7-AREA'.            
048000 01  -COPY WDK711                                                         
048100     EJECT                                                                
048200 01  FILLER                      PIC X(16)  VALUE 'WDQ2-AREA'.            
048300 01  -COPY WDQ201                                                         
048400     EJECT                                                                
048500 01  FILLER                      PIC X(16)  VALUE 'WDQ212-AREA'.          
048600 01  -COPY WDQ212                                                         
048700     EJECT                                                                
048800 01  FILLER                      PIC X(16)  VALUE '4448-AREA'.            
048900 01  -COPY WDGX4448                                                       
049000     EJECT                                                                
049100 01  FILLER                      PIC X(16)  VALUE '4474-AREA'.            
049200 01  -COPY WDGX4474                                                       
049300     EJECT                                                                
049400 01  FILLER               PIC X(16)   VALUE 'DLI-IO-WDE201'.              
049500 01  DLI-IO-WDE201.                                                       
049600*      03  -COPY WDE201                                                   
049700     EJECT                                                                
049800 01  FILLER               PIC X(16)   VALUE 'DLI-IO-WDE211'.              
049900 01  DLI-IO-WDE211.                                                       
050000*      03  -COPY WDE211                                                   
050100     EJECT                                                                
050200 01  FILLER               PIC X(16)   VALUE 'DLI-IO-WDE221'.              
050300 01  DLI-IO-WDE221.                                                       
050400*      03  -COPY WDE221                                                   
050500 01  FILLER               PIC X(16)   VALUE 'DLI-IO-WDE101'.              
050600 01  DLI-IO-WDE101.                                                       
050700*      03  -COPY WDE101                                                   
050800     EJECT                                                                
050900 01  FILLER               PIC X(16)   VALUE 'DLI-IO-WDE111'.              
051000 01  DLI-IO-WDE111.                                                       
051100*      03  -COPY WDE111                                                   
051200     EJECT                                                                
051300 01  FILLER               PIC X(16)   VALUE 'DLI-IO-WDE121'.              
051400 01  DLI-IO-WDE121.                                                       
051500*      03  -COPY WDE121                                                   
051600     EJECT                                                                
051700 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
051800 01   DLI-IO-AREA-B601.                                                   
051900*     03  -COPY WDB601                                                    
052000                                                                          
052100 LINKAGE SECTION.                                                         
052200*                                                                         
052300*01    -COPY W0009     -PRE MSG-                                          
052400     SKIP2                                                                
052500*01    -COPY W0009     -PRE AD36-                                         
052600     EJECT                                                                
052700*01    -COPY W0009     -PRE DISP-                                         
052800     EJECT                                                                
052900*01    -COPY W0008     -PRE USEA-                                         
053000     05  FILLER                  PIC X.                                   
053100     SKIP2                                                                
053200*01    -COPY W0008     -PRE WDE4-                                         
053300     05  FILLER                  PIC X.                                   
053400     SKIP2                                                                
053500*01    -COPY W0008     -PRE WDE42-                                        
053600     05  FILLER                  PIC X.                                   
053700     EJECT                                                                
053800*01    -COPY W0008     -PRE ORQA-                                         
053900     05  FILLER                  PIC X.                                   
054000     EJECT                                                                
054100*01    -COPY W0008     -PRE ORQA2-                                        
054200     05  FILLER                  PIC X.                                   
054300     EJECT                                                                
054400*01    -COPY W0008     -PRE ORQI-                                         
054500     05  FILLER                  PIC X.                                   
054600     EJECT                                                                
054700*01    -COPY W0008     -PRE WDE6-                                         
054800     05  FILLER                  PIC X.                                   
054900     EJECT                                                                
055000*01    -COPY W0008     -PRE XXKH-                                         
055100     05  FILLER                  PIC X.                                   
055200     EJECT                                                                
055300*01    -COPY W0008     -PRE ARTC-                                         
055400     05  FILLER                  PIC X.                                   
055500     EJECT                                                                
055600*01    -COPY W0008     -PRE ARTS-                                         
055700     05  FILLER                  PIC X.                                   
055800     EJECT                                                                
055900*01    -COPY W0008     -PRE 4487-                                         
056000     05  FILLER                  PIC X.                                   
056100     EJECT                                                                
056200*01    -COPY W0008     -PRE WDB2-                                         
056300     05  FILLER                  PIC X.                                   
056400*01    -COPY W0008     -PRE WDE2-                                         
056500     05  FILLER                  PIC X.                                   
056600*01    -COPY W0008     -PRE WDE1-                                         
056700     05  FILLER                  PIC X.                                   
056800*01    -COPY W0008     -PRE 4517-                                         
056900     05  FILLER                  PIC X.                                   
057000     EJECT                                                                
057100*01    -COPY W0008     -PRE WDB6-                                         
057200     05  FILLER                  PIC X.                                   
057300     EJECT                                                                
057400                                                                          
057500  PROCEDURE DIVISION USING MSG-PCB AD36-PCB DISP-PCB  USEA-PCB            
057600                           WDE4-PCB WDE42-PCB                             
057700                           ORQA-PCB ORQA2-PCB ORQI-PCB WDE6-PCB           
057800                           XXKH-PCB ARTC-PCB  ARTS-PCB 4487-PCB           
057900                           WDB2-PCB                                       
058000                           WDE2-PCB WDE1-PCB  4517-PCB WDB6-PCB.          
058100                                                                          
058200     PERFORM IMS-GU-MSG-AREA                                              
058300     IF SEGMENT-FINNS                                                     
058400       PERFORM IMS-GN-KOM-AREA                                            
058500       PERFORM A-INIT                                                     
058600       PERFORM B-KONTROLL-INDATA                                          
058700       IF WS-INDATA-RATT AND WS-GODKAND-TRANS                             
058800                                                                          
058900          PERFORM C-LAGG-UPP-KOLLI-SEG                                    
059000          PERFORM D-BEHANDLA-RADER                                        
059100                                                                          
059200          PERFORM F-UPPDATERA-KOLLIREG                                    
059300                                                                          
059400          IF VORD-KVORDRAD = VORD-KVORDRAD-PACK                           
059500            PERFORM J-UPDATE-E401-Q301-HTYP4487                           
059600            PERFORM K-UPDATE-E601-KOLLIREG                                
059700          END-IF                                                          
059800          PERFORM H-UPDATE-LAST-OCH-FAKT-REG                              
059900       END-IF                                                             
060000     END-IF                                                               
060100     PERFORM Z-DISPATCH-AVSLUT                                            
060200                                                                          
060300     MOVE ZERO TO RETURN-CODE                                             
060400     GOBACK                                                               
060500     .                                                                    
060600     EJECT                                                                
060700 A-INIT             SECTION.                                              
060800     MOVE 'STA A-SEC'                     TO WS-PGM-POSITION              
060900     IF MSG-DUBBLA-TRANSKODER                                             
061000       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO   MID-W4I39901               
061100                                               MID2-W4I39902              
061200       MOVE MSG-IDTRANS-2                 TO   WS-IDTRANS                 
061300       MOVE MSG-KDMFSFOR-2                TO   WS-KDMFSFOR                
061400     ELSE                                                                 
061500       MOVE MSG-INDATA-MINUS-1-TRANSKOD   TO   MID-W4I39901               
061600                                               MID2-W4I39902              
061700       MOVE MSG-IDTRANS-1                 TO   WS-IDTRANS                 
061800       MOVE MSG-KDMFSFOR-1                TO   WS-KDMFSFOR                
061900     END-IF                                                               
062000*                                                                         
062100     PERFORM S21-INIT-WS-FIELDS                                           
062200     PERFORM AA-FLYTTA-INDATA-MID                                         
062300                                                                          
062400     ACCEPT WS-DAGENS-DATUM               FROM DATE                       
062500     ACCEPT WS-HHMMSSDD                   FROM TIME                       
062600     ACCEPT WS-TID-W                      FROM TIME                       
062700                                                                          
062800     PERFORM S08-HAMTA-MASKINDATUM                                        
062900                                                                          
063000     IF NOT MID-IDDC = W-IDDC-B6                                          
063100        MOVE MID-IDDC TO W-IDDC-B6                                        
063200        PERFORM IMS-GU-WDB601                                             
063300     END-IF                                                               
063400     IF DCS-NDC-NA OR DCS-CHINA                                           
063500       MOVE ALL '+'           TO MSGI-WMSGINIT                            
063600       MOVE '011'             TO MSGI-KDCALL                              
063700       MOVE WS-DCUSER         TO MSGI-IDUSER                              
063800       MOVE WS-DAGENS-DATUM   TO MSGI-TILOKDAT                            
063900       MOVE WS-HHMMSSDD(1:4)  TO MSGI-TILOKTID                            
064000       CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                         
064100       MOVE MSGI-TILOKDAT     TO WS-DAGENS-DATUM                          
064200                                 DAT-I-TIDATUM                            
064300       MOVE MSGI-TILOKTID     TO WS-HHMMSSDD (1:4)                        
064400                                 WS-TTMMSS   (1:4)                        
064500                                 WS-TISKPTID-HHMM                         
064600                                                                          
064700       MOVE 'AAMMDD'          TO DAT-KDDATFORM                            
064800       CALL WDATKONV USING DAT-KDDATFORM                                  
064900                           DAT-I-TIDATUM                                  
065000                           DAT-O-TIDATUM                                  
065100                           DAT-KDSVAR                                     
065200     END-IF                                                               
065300                                                                          
065400     MOVE '1'                          TO W-KDSEGKEY                      
065500                                                                          
065600     MOVE SPACE                        TO MSG-KOM-IDMFSMED                
065700     MOVE 'SLUTA-SEC'                     TO WS-PGM-POSITION              
065800     .                                                                    
065900     SKIP2                                                                
066000 AA-FLYTTA-INDATA-MID    SECTION.                                         
066100                                                                          
066200     MOVE MID-IDANSTNR                 TO WS-IDANSTNR                     
066300     MOVE MID-IDDC                     TO WS-DCUSER-IDDC                  
066400     MOVE MID-IDPRODNR                 TO WS-IDPRODNR                     
066500                                          W-401-IDPRODNR                  
066600     MOVE MID-IDDISTR                  TO W-401-IDDISTR                   
066700                                          WS-IDDISTR                      
066800                                          TEST-IDDISTR                    
066900     MOVE MID-IDKUNDNR                 TO WS-IDKUNDNR-NUM                 
067000                                          W-401-IDKUNDNR                  
067100                                          WS-IDKUNDNR                     
067200     MOVE MID-IDORDNR                  TO WS-IDORDNR5                     
067300                                          W-401-IDORDNR                   
067400     MOVE MID-IDFAKT-GNB               TO WS-IDFAKT-GNB                   
067500     .                                                                    
067600     SKIP2                                                                
067700 B-KONTROLL-INDATA      SECTION.                                          
067800       MOVE 'STA B-SEC'                     TO WS-PGM-POSITION            
067900     MOVE RAETT                        TO WS-INDATA-TEST                  
068000     MOVE +1 TO IN-RAD-INDX                                               
068100     MOVE +0 TO SENASTE-IDRADNR                                           
068200     IF WS-IDANSTNR = '01441'                                             
068300       MOVE +1                         TO MAX-RAD-ANTAL                   
068400       MOVE MID2-IDRADNR (IN-RAD-INDX) TO WS-IDRADNR                      
068500     ELSE                                                                 
068600       MOVE +90                        TO MAX-RAD-ANTAL                   
068700       MOVE MID-IDRADNR (IN-RAD-INDX)  TO WS-IDRADNR                      
068800     END-IF                                                               
068900                                                                          
069000     PERFORM UNTIL IN-RAD-INDX > MAX-RAD-ANTAL                            
069100         PERFORM BA-KONTROLL-AV-RAD                                       
069200         ADD +1                TO IN-RAD-INDX                             
069300         IF WS-IDANSTNR = '01441'                                         
069400           IF MID2-IDRADNR (IN-RAD-INDX) = ALL '+'                        
069500             MOVE +99 TO IN-RAD-INDX                                      
069600           ELSE                                                           
069700             MOVE MID2-IDRADNR (IN-RAD-INDX) TO WS-IDRADNR                
069800           END-IF                                                         
069900         ELSE                                                             
070000           IF MID-IDRADNR (IN-RAD-INDX) = ALL '+'                         
070100             COMPUTE DLEV-RAD-ANTAL = IN-RAD-INDX - 1                     
070200             MOVE +99 TO IN-RAD-INDX                                      
070300           ELSE                                                           
070400             MOVE MID-IDRADNR (IN-RAD-INDX) TO WS-IDRADNR                 
070500           END-IF                                                         
070600         END-IF                                                           
070700     END-PERFORM                                                          
070800     IF WS-IDANSTNR NOT = '01441'                                         
070900       MOVE DLEV-RAD-ANTAL TO MAX-RAD-ANTAL                               
071000     END-IF                                                               
071100       MOVE 'SLUT B-SEC'                    TO WS-PGM-POSITION            
071200     .                                                                    
071300     SKIP2                                                                
071400 BA-KONTROLL-AV-RAD       SECTION.                                        
071500       MOVE 'STA BA-SEC '      TO WS-PGM-POSITION                         
071600     MOVE WS-IDDISTR           TO W-401-IDDISTR                           
071700     MOVE WS-IDKUNDNR          TO W-401-IDKUNDNR                          
071800     MOVE WS-IDORDNR5          TO W-401-IDORDNR                           
071900     MOVE WS-IDPRODNR          TO W-401-IDPRODNR                          
072000     MOVE +1                   TO W-401-IDPLKLST                          
072100                                                                          
072200                                                                          
072300     PERFORM IMS-GU-WDE401                                                
072400                                                                          
072500     PERFORM UNTIL SEGMENT-FINNS                                          
072600          OR W-401-IDPLKLST > 15                                          
072700       ADD +1                  TO W-401-IDPLKLST                          
072800       PERFORM IMS-GU-WDE401                                              
072900     END-PERFORM                                                          
073000                                                                          
073100     IF SEGMENT-FINNS                                                     
073200       MOVE KORD-IDPLKLST        TO W-401-IDPLKLST                        
073300       MOVE KORD-IDORDER         TO W-201-IDORDER                         
073400       MOVE KORD-KDORDKL         TO WS-KORD-KDORDKL                       
073500       MOVE KORD-IDORDER         TO WS-KORD-IDORDER                       
073600       MOVE KORD-SUORDV          TO WS-KORD-SUORDV                        
073700       MOVE KORD-SUORDV-LOC      TO WS-KORD-SUORDV-LOC                    
073800       MOVE KORD-SUORDV-LOCPREL  TO WS-KORD-SUORDV-LOCPREL                
073900       MOVE KORD-KDVALISO        TO WS-KORD-KDVALISO                      
074000                                                                          
074100       PERFORM IMS-GU-ORQI01                                              
074200       IF SEGMENT-FINNS                                                   
074300         MOVE WS-IDRADNR         TO W-420-IDPURAD2                        
074400         PERFORM IMS-GNP-WDE411                                           
074500         IF SEGMENT-FINNS                                                 
074600                                                                          
074700            PERFORM BAA-KONTROLLERA-RADEN                                 
074800         ELSE                                                             
074900*        RAD FR.MID-AREA SAKNAS I WDE411'                                 
075000           MOVE ERR-ORDERRAD-SAKN TO MSG-KOM-IDMFSMED                     
075100           MOVE '4'              TO MSG-KOM-KDSVAR                        
075200           MOVE FEL              TO WS-INDATA-TEST                        
075300         END-IF                                                           
075400       ELSE                                                               
075500*        ORDERHUVUD RENSAD? '                                             
075600         MOVE ERR-ORDERHUVUD-SAKNAS   TO MSG-KOM-IDMFSMED                 
075700         MOVE '4'                     TO MSG-KOM-KDSVAR                   
075800         MOVE FEL                     TO WS-INDATA-TEST                   
075900       END-IF                                                             
076000     ELSE                                                                 
076100*   ORDER FR.MID-AREA SAKNAS I WDE401'                                    
076200       MOVE ERR-ORDER-SAKNAS     TO MSG-KOM-IDMFSMED                      
076300       MOVE '4'                  TO MSG-KOM-KDSVAR                        
076400       MOVE FEL                  TO WS-INDATA-TEST                        
076500     END-IF                                                               
076600       MOVE 'SLUT BA-SEC'                    TO WS-PGM-POSITION           
076700     .                                                                    
076800     EJECT                                                                
076900 BAA-KONTROLLERA-RADEN SECTION.                                           
077000                                                                          
077100     IF WS-IDANSTNR = '01441'                                             
077200       MOVE MID2-KVLEVART(IN-RAD-INDX) TO WS-MID-KVLEVART                 
077300     ELSE                                                                 
077400       MOVE MID-KVLEVART(IN-RAD-INDX) TO WS-MID-KVLEVART                  
077500     END-IF                                                               
077600                                                                          
077700     IF (WS-MID-KVLEVART + ORAD-KVLEVART) > ORAD-KVBEART                  
077800     OR (WS-MID-KVLEVART + ORAD-KVLEVART) > ORAD-KVAVBART                 
077900*   ÖVER LEVERANS!'                                                       
078000       MOVE ERR-OVER-LEVERANS    TO MSG-KOM-IDMFSMED                      
078100       MOVE '4'                  TO MSG-KOM-KDSVAR                        
078200       MOVE FEL                  TO WS-INDATA-TEST                        
078300     END-IF                                                               
078400                                                                          
078500     IF WS-MID-KVLEVART = ZERO                                            
078600*   ANTAL = 0     '                                                       
078700       MOVE ERR-FEL-ANTAL        TO MSG-KOM-IDMFSMED                      
078800       MOVE '4'                  TO MSG-KOM-KDSVAR                        
078900       MOVE FEL                  TO WS-INDATA-TEST                        
079000     END-IF                                                               
079100                                                                          
079200     MOVE WS-IDRADNR             TO AKTUELLT-IDRADNR                      
079300     IF AKTUELLT-IDRADNR > SENASTE-IDRADNR                                
079400       MOVE AKTUELLT-IDRADNR     TO SENASTE-IDRADNR                       
079500     ELSE                                                                 
079600       MOVE ERR-FELAKTIGA-RADER  TO MSG-KOM-IDMFSMED                      
079700       MOVE '4'                  TO MSG-KOM-KDSVAR                        
079800       MOVE FEL                  TO WS-INDATA-TEST                        
079900     END-IF                                                               
080000     .                                                                    
080100     EJECT                                                                
080200 C-LAGG-UPP-KOLLI-SEG   SECTION.                                          
080300       MOVE 'STA C-SEC  '                   TO WS-PGM-POSITION            
080400     MOVE WS-IDPRODNR            TO W-601-IDPRODNR                        
080500     MOVE WS-IDKOLLI-NUM         TO W-421-IDKOLLI                         
080600                                    W-610-IDKOLLI                         
080700     PERFORM IMS-GHU-WDE601                                               
080800                                                                          
080900     PERFORM S09-INIT-KOLLI-VALUE                                         
081000                                                                          
081100     IF WS-INDATA-RATT AND WS-GODKAND-TRANS                               
081200       PERFORM IMS-ISRT-KOLLI                                             
081300                                                                          
081400       IF SEGMENT-FINNS-REDAN                                             
081500         PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                            
081600             ADD +1              TO WS-IDKOLLI-NUM                        
081700                                      KOLLI-IDKOLLI                       
081800                                      W-421-IDKOLLI                       
081900                                      W-610-IDKOLLI                       
082000             PERFORM IMS-ISRT-KOLLI                                       
082100         END-PERFORM                                                      
082200       END-IF                                                             
082300     END-IF                                                               
082400     MOVE 'SLUT C-SEC  '                  TO WS-PGM-POSITION              
082500     .                                                                    
082600     EJECT                                                                
082700 D-BEHANDLA-RADER     SECTION.                                            
082800       MOVE 'STA D-SEC '                    TO WS-PGM-POSITION            
082900     PERFORM IMS-GHU-WDE401                                               
083000                                                                          
083100     MOVE +1 TO IN-RAD-INDX                                               
083200     IF WS-IDANSTNR = '01441'                                             
083300       MOVE MID2-IDRADNR (IN-RAD-INDX) TO WS-IDRADNR                      
083400       MOVE MID2-KVLEVART(IN-RAD-INDX) TO WS-KVLEVART                     
083500     ELSE                                                                 
083600       MOVE MID-IDRADNR (IN-RAD-INDX)  TO WS-IDRADNR                      
083700       MOVE MID-KVLEVART(IN-RAD-INDX)  TO WS-KVLEVART                     
083800     END-IF                                                               
083900     MOVE WS-IDRADNR               TO W-420-IDPURAD2                      
084000                                      WS-IDRADNR-SPAR                     
084100     PERFORM IMS-GHNP-WDE411                                              
084200                                                                          
084300     PERFORM UNTIL IN-RAD-INDX > MAX-RAD-ANTAL                            
084400                                                                          
084500        IF WS-KVLEVART > '0000000'                                        
084600          PERFORM DAB-UPPDATERA-RAD                                       
084700          PERFORM DAC-LAGG-UPP-KOLLI-KOPPL                                
084800          PERFORM DAD-NOLLSTALL-SPAR-ORAD                                 
084900        END-IF                                                            
085000        MOVE WS-IDRADNR             TO WS-IDRADNR-SPAR                    
085100        ADD +1                      TO IN-RAD-INDX                        
085200        IF IN-RAD-INDX > MAX-RAD-ANTAL                                    
085300          CONTINUE                                                        
085400        ELSE                                                              
085500          IF WS-IDANSTNR = '01441'                                        
085600            MOVE MID2-IDRADNR (IN-RAD-INDX) TO WS-IDRADNR                 
085700            MOVE MID2-KVLEVART(IN-RAD-INDX) TO WS-KVLEVART                
085800          ELSE                                                            
085900            MOVE MID-IDRADNR (IN-RAD-INDX) TO WS-IDRADNR                  
086000            MOVE MID-KVLEVART(IN-RAD-INDX) TO WS-KVLEVART                 
086100          END-IF                                                          
086200*IMS LÄSN.ÄR BEROENDE OM NÄSTA RAD ÄR MINDRE EL. STÖRRE ÄN FÖREG.         
086300          MOVE WS-IDRADNR TO W-420-IDPURAD2                               
086400          IF WS-IDRADNR < WS-IDRADNR-SPAR                                 
086500            PERFORM IMS-GHNP-WDE411-F                                     
086600          ELSE                                                            
086700            PERFORM IMS-GHNP-WDE411                                       
086800          END-IF                                                          
086900        END-IF                                                            
087000     END-PERFORM                                                          
087100     MOVE 'SLUT D-SEC '             TO WS-PGM-POSITION                    
087200     .                                                                    
087300     EJECT                                                                
087400 DAB-UPPDATERA-RAD         SECTION.                                       
087500       MOVE 'STA DAB-SEC '          TO WS-PGM-POSITION                    
087600     MOVE ORAD-VKARTNTO             TO WS-ORAD-VKARTNTO                   
087700     MOVE ORAD-VLARTNTO             TO WS-ORAD-VLARTNTO                   
087800     MOVE ORAD-KVFLAMP              TO WS-ORAD-KVFLAMP                    
087900     MOVE ORAD-KDFARLIG             TO WS-ORAD-KDFARLIG                   
088000     MOVE ORAD-PRARTNTO             TO WS-ORAD-PRARTNTO                   
088100     MOVE ORAD-PRARTNTO-LOC         TO WS-ORAD-PRARTNTO-LOC               
088200     MOVE ORAD-PRARTNTO-LOCPREL     TO WS-ORAD-PRARTNTO-LOCPREL           
088300     MOVE ORAD-KDVALISO             TO WS-ORAD-KDVALISO                   
088400*                                                                         
088500     MOVE ORAD-IDPSN                TO WS-ORAD-IDPSN                      
088600     MOVE ORAD-VKART-FG             TO WS-ORAD-VKART-FG                   
088700     MOVE ORAD-VLFG                 TO WS-ORAD-VLFG                       
088800     MOVE ORAD-SUEQFG               TO WS-ORAD-SUEQFG                     
088900*                                                                         
089000     IF WS-IDANSTNR = '01441'                                             
089100       MOVE MID2-KVLEVART(IN-RAD-INDX) TO WS-MID-KVLEVART                 
089200       ADD WS-MID-KVLEVART          TO ORAD-KVLEVART                      
089300                                         WS-ORAD-KVLEVART                 
089400     ELSE                                                                 
089500       MOVE MID-KVLEVART(IN-RAD-INDX) TO WS-MID-KVLEVART                  
089600       ADD WS-MID-KVLEVART          TO ORAD-KVLEVART                      
089700                                         WS-ORAD-KVLEVART                 
089800     END-IF                                                               
089900     PERFORM S10-UPPD-SPAR-KOLLI                                          
090000                                                                          
090100     IF ORAD-KVLEVART = ORAD-KVAVBART                                     
090200        MOVE +4                     TO ORAD-KDRADSTA                      
090300        ADD 1                       TO WS-ANT-KLARA-RADER                 
090400     END-IF                                                               
090500                                                                          
090600     IF WS-IDANSTNR = '01441'                                             
090700       IF MID2-IDKLIENT (IN-RAD-INDX) > SPACE                             
090800         MOVE MID2-IDKLIENT(IN-RAD-INDX) TO ORAD-IDKLIENT                 
090900       END-IF                                                             
091000       IF MID2-IDARBREF (IN-RAD-INDX) > SPACE                             
091100         MOVE MID2-IDARBREF(IN-RAD-INDX) TO ORAD-IDARBREF                 
091200       END-IF                                                             
091300       IF MID2-IDBIL    (IN-RAD-INDX) > SPACE                             
091400         MOVE MID2-IDBIL(IN-RAD-INDX)  TO ORAD-IDBIL                      
091500       END-IF                                                             
091600       IF MID2-IDVIN    (IN-RAD-INDX) > SPACE                             
091700         MOVE MID2-IDVIN(IN-RAD-INDX)  TO ORAD-IDVIN                      
091800       END-IF                                                             
091900     END-IF                                                               
092000                                                                          
092100     PERFORM IMS-REPL-BEHANDLAD-RAD                                       
092200       MOVE 'SLUT DAB-SEC '         TO WS-PGM-POSITION                    
092300     .                                                                    
092400     EJECT                                                                
092500 DAC-LAGG-UPP-KOLLI-KOPPL  SECTION.                                       
092600       MOVE 'STA DAC-SEC '          TO WS-PGM-POSITION                    
092700     MOVE WS-IDPRODNR               TO KKOLLI-IDPRODNR                    
092800     MOVE WS-IDKOLLI-NUM            TO KKOLLI-IDKOLLI                     
092900     MOVE WS-ORAD-KVLEVART          TO KKOLLI-KVLEVART                    
093000     PERFORM IMS-ISRT-KOLLI-KOPPL                                         
093100     IF SEGMENT-FINNS-REDAN                                               
093200         MOVE WS-IDPRODNR           TO W-421-IDPRODNR                     
093300         MOVE WS-IDKOLLI-NUM        TO W-421-IDKOLLI                      
093400         PERFORM IMS-GHNP-KOLLI-KOPPL                                     
093500         ADD WS-ORAD-KVLEVART       TO KKOLLI-KVLEVART                    
093600         PERFORM IMS-REPL-KOLLI-KOPPL                                     
093700     ELSE                                                                 
093800         ADD +1                     TO ACC-KOLLI-KVORDRAD                 
093900                                                                          
094000         IF WS-ORAD-KDFARLIG = +4                                         
094100         OR WS-ORAD-KDFARLIG = +7                                         
094200             ADD +1                 TO ACC-KOLLI-KVFALRAD                 
094300         END-IF                                                           
094400     END-IF                                                               
094500*                                                                         
094600     IF WS-ORAD-IDPSN > ZERO                                              
094700       PERFORM DACA-SPARA-FG-DATA                                         
094800     END-IF                                                               
094900       MOVE 'SLUT DAC-SEC '         TO WS-PGM-POSITION                    
095000     .                                                                    
095100     EJECT                                                                
095200 DACA-SPARA-FG-DATA SECTION.                                              
095300       MOVE 'STA DACA-SEC  '        TO WS-PGM-POSITION                    
095400     MOVE +1 TO FG-INDX                                                   
095500     PERFORM UNTIL FG-INDX > FG-MAX-INDX                                  
095600                                                                          
095700       IF TAB-IDPSN(FG-INDX) = ZERO                                       
095800         MOVE WS-ORAD-IDPSN         TO TAB-IDPSN(FG-INDX)                 
095900         PERFORM S17-BERAEKNA-FG-FAELT                                    
096000                                                                          
096100       ELSE                                                               
096200         IF WS-ORAD-IDPSN = TAB-IDPSN(FG-INDX)                            
096300           PERFORM S17-BERAEKNA-FG-FAELT                                  
096400         END-IF                                                           
096500       END-IF                                                             
096600                                                                          
096700       ADD +1 TO FG-INDX                                                  
096800     END-PERFORM                                                          
096900                                                                          
097000     COMPUTE TOTAL-SUEQFG = TOTAL-SUEQFG      +                           
097100                            (WS-ORAD-SUEQFG *                             
097200                             KKOLLI-KVLEVART)                             
097300     END-COMPUTE                                                          
097400       MOVE 'SLUT DACA-SEC  '       TO WS-PGM-POSITION                    
097500     .                                                                    
097600     EJECT                                                                
097700 DAD-NOLLSTALL-SPAR-ORAD      SECTION.                                    
097800                                                                          
097900     MOVE ZEROES              TO WS-ORAD-KVFLAMP                          
098000                                 WS-ORAD-KDFARLIG                         
098100                                 WS-ORAD-KVLEVART                         
098200                                 WS-ORAD-IDPSN                            
098300                                 WS-ORAD-VKART-FG                         
098400                                 WS-ORAD-VLFG                             
098500                                 WS-ORAD-SUEQFG                           
098600     .                                                                    
098700     EJECT                                                                
098800 F-UPPDATERA-KOLLIREG      SECTION.                                       
098900       MOVE 'STA F-SEC  '                   TO WS-PGM-POSITION            
099000     PERFORM IMS-GHU-WDE601                                               
099100     MOVE VORD-FLAUTFAK       TO  WS-FLAUTFAK                             
099200     MOVE VORD-DARFS          TO  WS-DARFS                                
099300     ADD +1                   TO  VORD-KVKOLLI                            
099400                                  VORD-KVKOLPAC                           
099500                                  KOLLI-VALUE-ETT                         
099600     MOVE WS-DAGENS-DATUM     TO  VORD-TIPACKN-SK                         
099700                                                                          
099800     IF VORD-KDORDSTA        =   1                                        
099900         MOVE 2               TO  VORD-KDORDSTA                           
100000     END-IF                                                               
100100                                                                          
100200     COMPUTE VORD-KVORDRAD-PACK                                           
100300           = VORD-KVORDRAD-PACK + WS-ANT-KLARA-RADER                      
100400     END-COMPUTE                                                          
100500                                                                          
100600     COMPUTE VORD-VKORDBTO    ROUNDED                                     
100700           = VORD-VKORDBTO + ACC-KOLLI-VKORDNTO * KOLLI-VALUE-ETT         
100800     END-COMPUTE                                                          
100900                                                                          
101000     COMPUTE VORD-VLORDBTO    ROUNDED                                     
101100           = VORD-VLORDBTO + WS-KOLLI-VLORDBTO * KOLLI-VALUE-ETT          
101200     END-COMPUTE                                                          
101300                                                                          
101400     COMPUTE VORD-VLORDNTO    ROUNDED                                     
101500           = VORD-VLORDNTO + ACC-ORAD-VLARTNTO * KOLLI-VALUE-ETT          
101600     END-COMPUTE                                                          
101700                                                                          
101800     COMPUTE VORD-VKORDNTO    ROUNDED                                     
101900           = VORD-VKORDNTO + ACC-KOLLI-VKORDNTO * KOLLI-VALUE-ETT         
102000     END-COMPUTE                                                          
102100                                                                          
102200     IF VORD-VKORDNTO > VORD-VKORDBTO                                     
102300       MOVE VORD-VKORDBTO     TO VORD-VKORDNTO                            
102400     END-IF                                                               
102500                                                                          
102600     COMPUTE VORD-SUORDV-PACK-LOC ROUNDED                                 
102700                   = VORD-SUORDV-PACK-LOC + ACC-KOLLI-SUORDV-LOC          
102800                                               * KOLLI-VALUE-ETT          
102900     COMPUTE VORD-SUORDV-PACK-LOCPREL ROUNDED                             
103000           = VORD-SUORDV-PACK-LOCPREL + ACC-KOLLI-SUORDV-LOCPREL          
103100                                               * KOLLI-VALUE-ETT          
103200                                                                          
103300     COMPUTE VORD-SUORDV-PACK ROUNDED                                     
103400                           = VORD-SUORDV-PACK + ACC-KOLLI-SUORDV          
103500                                               * KOLLI-VALUE-ETT          
103600     MOVE ACC-KOLLI-KDVALISO      TO VORD-KDVALISO                        
103700     PERFORM IMS-REPL-WDE601                                              
103800                                                                          
103900     PERFORM FA-BEHANDLA-KOLLI                                            
104000       MOVE 'SLUT F-SEC '                   TO WS-PGM-POSITION            
104100     .                                                                    
104200     EJECT                                                                
104300 FA-BEHANDLA-KOLLI      SECTION.                                          
104400       MOVE 'STA FA-SEC   '                  TO WS-PGM-POSITION           
104500     MOVE WS-IDKOLLI-NUM          TO  W-610-IDKOLLI                       
104600     PERFORM IMS-GHU-WDE611                                               
104700*                                                                         
104800     PERFORM S11-UPPD-KOLLI-FRAN-ARB                                      
104900*                                                                         
105000     PERFORM S13-UPPDAT-KDORDSTA                                          
105100*                                                                         
105200     PERFORM S16-UPPD-FARLIGT-GODS-DATA                                   
105300*                                                                         
105400     PERFORM IMS-REPL-KOLLI                                               
105500       MOVE 'SLUT FA-SEC '                   TO WS-PGM-POSITION           
105600     .                                                                    
105700     EJECT                                                                
105800 J-UPDATE-E401-Q301-HTYP4487     SECTION.                                 
105900       MOVE 'STA J-SEC'                     TO WS-PGM-POSITION            
106000     PERFORM IMS-GHU-KUNDORDER                                            
106100                                                                          
106200     MOVE ZERO                   TO KORD-KDPAKOLL                         
106300                                                                          
106400     COMPUTE KORD-KVORDRAD-PACK = KORD-KVORDRAD    +                      
106500                                  KORD-KVORDRAD-LEVPL                     
106600     END-COMPUTE                                                          
106700                                                                          
106800     PERFORM IMS-REPL-WDE401                                              
106900                                                                          
107000     MOVE KORD-IDDISTR               TO TEST-IDDISTR                      
107100                                                                          
107200     PERFORM JB-UPDATE-ORQA                                               
107300     PERFORM JD-UPDATE-ORQI                                               
107400       MOVE 'SLUT J-SEC '                   TO WS-PGM-POSITION            
107500     .                                                                    
107600     EJECT                                                                
107700 JB-UPDATE-ORQA                          SECTION.                         
107800                                                                          
107900     MOVE KORD-IDORDER                   TO W-WDQ301-IDORDER              
108000     MOVE KORD-IDDC                      TO W-WDQ301-IDDC                 
108100     MOVE KORD-IDPRODNR                  TO W-WDQ301-IDPRODNR             
108200     MOVE KORD-IDPLKLST                  TO W-WDQ301-IDPLKLST             
108300     PERFORM IMS-GHU-ORQA01                                               
108400                                                                          
108500     MOVE KORD-KVORDRAD-PACK             TO ODEL-KVPACKRAD-OD             
108600     MOVE 'P'                            TO ODEL-KDODELSTA                
108700     MOVE DAT-TIAAMMDD                   TO ODEL-TIPACKN                  
108800     MOVE WS-TTMMSS                      TO ODEL-TIPACTID                 
108900*                                                                         
109000     MOVE ODEL-IDDC                      TO W-4447-IDDC                   
109100                                            W-4487-IDDC                   
109200     MOVE ODEL-IDPRC                     TO W-4448-IDPRC                  
109300     MOVE ODEL-DARFS                     TO W-4490-DARFS                  
109400     MOVE ODEL-IDPRODNR                  TO W-4490-IDPRODNR               
109500     MOVE ODEL-IDPLKLST                  TO W-4490-IDPLKLST               
109600     PERFORM IMS-REPL-ORQA01                                              
109700                                                                          
109800     PERFORM S06-BORTTAG-PRODTAB                                          
109900     EJECT                                                                
110000     .                                                                    
110100 JD-UPDATE-ORQI                          SECTION.                         
110200       MOVE 'STA JD-SEC '                    TO WS-PGM-POSITION           
110300     MOVE WS-KORD-IDORDER         TO W-Q301KY-MIN-IDORDER                 
110400                                     W-Q301KY-MAX-IDORDER                 
110500     MOVE MID-IDDC                TO W-Q301KY-MIN-IDDC                    
110600                                     W-Q301KY-MAX-IDDC                    
110700                                                                          
110800     MOVE 'R'                     TO W-KDODELST                           
110900     PERFORM IMS-GU-ORQA-STATUS                                           
111000     IF SEGMENT-FINNS                                                     
111100        MOVE 'R*'                 TO WS-KDORDSTA                          
111200     ELSE                                                                 
111300                                                                          
111400        MOVE 'U'                  TO W-KDODELST                           
111500        PERFORM IMS-GU-ORQA-STATUS                                        
111600        IF SEGMENT-FINNS                                                  
111700           MOVE 'U*'              TO WS-KDORDSTA                          
111800        ELSE                                                              
111900           PERFORM JDA-KOLLA-KVKOLLI                                      
112000           IF WS-KDORDSTA         =  'P*'                                 
112100               CONTINUE                                                   
112200           ELSE                                                           
112300               MOVE 'P '          TO WS-KDORDSTA                          
112400           END-IF                                                         
112500        END-IF                                                            
112600     END-IF                                                               
112700                                                                          
112800     MOVE    WS-KORD-IDORDER      TO W-201-IDORDER                        
112900     MOVE    MID-IDDC             TO W-212-IDDC                           
113000     PERFORM IMS-GHU-ORQI12                                               
113100     MOVE WS-KDORDSTA             TO ARB-KDORDSTA                         
113200     PERFORM IMS-REPL-ORQI12                                              
113300     MOVE 'SLUT JD-SEC '          TO WS-PGM-POSITION                      
113400     .                                                                    
113500     EJECT                                                                
113600 JDA-KOLLA-KVKOLLI SECTION.                                               
113700     MOVE 'STA JDA-SEC '            TO WS-PGM-POSITION                    
113800     MOVE WS-IDPRODNR               TO W-601-IDPRODNR                     
113900     PERFORM IMS-GHU-WDE601                                               
114000                                                                          
114100     IF VORD-KVKOLLI-LAST > ZERO OR                                       
114200        VORD-KVKOLLI-FAKT > ZERO                                          
114300         MOVE 'P*'                  TO WS-KDORDSTA                        
114400       IF VORD-KVKOLLI-LAST = VORD-KVKOLLI-FAKT                           
114500         MOVE 'P '                  TO WS-KDORDSTA                        
114600       END-IF                                                             
114700     END-IF                                                               
114800     MOVE 'SLUT JDA-SEC '           TO WS-PGM-POSITION                    
114900     .                                                                    
115000     EJECT                                                                
115100 K-UPDATE-E601-KOLLIREG    SECTION.                                       
115200       MOVE 'STA K-SEC  '                   TO WS-PGM-POSITION            
115300     MOVE WS-IDPRODNR                TO  W-601-IDPRODNR                   
115400     PERFORM IMS-GHU-WDE601                                               
115500                                                                          
115600     IF SEGMENT-FINNS                                                     
115700                                                                          
115800       IF VORD-KVKOLPAC = 0                                               
115900           MOVE DAT-TIAAMMDD         TO VORD-TIPACKN-SK                   
116000       END-IF                                                             
116100                                                                          
116200       MOVE +3                       TO  VORD-KDORDSTA                    
116300     END-IF                                                               
116400                                                                          
116500     PERFORM IMS-REPL-WDE601                                              
116600     MOVE 'SLUT K-SEC '                     TO WS-PGM-POSITION            
116700     .                                                                    
116800     SKIP2                                                                
116900 H-UPDATE-LAST-OCH-FAKT-REG    SECTION.                                   
117000*SOM INFO: SEKTION HA- ÄR HÄMTAD FRÅN PGM 4663 OCH ÖVRIGA                 
117100*SEKTIONER I H-  ÄR HÄMTADE FRÅN PGM 4665.                                
117200                                                                          
117300     PERFORM HA-UPPDATERA-WDE6                                            
117400                                                                          
117500     MOVE WS-IDDISTR          TO DIST79-IDDISTR                           
117600     PERFORM S27-SHIPM-NUMBER                                             
117700     PERFORM S26-DC-LAND                                                  
117800     PERFORM HE-SKAPA-WDE2                                                
117900     PERFORM HF-SKAPA-WDE1                                                
118000     PERFORM S23-OPEN-WZ01                                                
118100     PERFORM S24-SEND-WZ01                                                
118200     PERFORM S25-CLOSE-WZ01                                               
118300     .                                                                    
118400     EJECT                                                                
118500 HA-UPPDATERA-WDE6    SECTION.                                            
118600       MOVE 'STA HA-SEC '                    TO WS-PGM-POSITION           
118700     MOVE WS-IDPRODNR         TO W-601-IDPRODNR                           
118800     MOVE WS-IDKOLLI-NUM      TO W-610-IDKOLLI                            
118900                                                                          
119000     PERFORM IMS-GHU-WDE611                                               
119100     MOVE NEJ                     TO KOLLI-FLUTLAST                       
119200     MOVE WS-IDLBBET              TO KOLLI-IDLBBET                        
119300     MOVE WS-DARFS                TO KOLLI-DARFS                          
119400     MOVE +6                      TO KOLLI-KDKOLSTA                       
119500     MOVE ZERO                    TO KOLLI-IDKOLLI-SAMP                   
119600                                                                          
119700*- - - - - - KOLLA OM KOLLIT INNEHÅLLER FARLIGT GODS                      
119800     MOVE +1         TO IDPSN-IX                                          
119900     PERFORM UNTIL IDPSN-IX > +9                                          
120000        IF KOLLI-IDPSN(IDPSN-IX) > +0                                     
120100           MOVE JA TO FARLIGT-GODS                                        
120200        END-IF                                                            
120300        ADD +1       TO IDPSN-IX                                          
120400     END-PERFORM                                                          
120500                                                                          
120600*-- TILASTID SÄTTS TILL 9:OR PGA. ATT 4698-BOLLA-PGM BEHÖVER              
120700*-- VETA VILKA KOLLIN I EN ORDER SOM REDAN BLEV UTLASTADE.                
120800*-- DESSA 9:OR ERSÄTTS SEDAN I BMP-FAKT MED RIKTIG TID.                   
120900*    MOVE +9999999              TO KOLLI-TILASTID                         
121000     PERFORM IMS-REPL-WDE611                                              
121100                                                                          
121200     PERFORM IMS-GHU-WDE601                                               
121300     MOVE VORD-KDFAKTYP           TO WS-KDFAKTYP                          
121400     ADD  +1                      TO VORD-KVKOLLI-FL                      
121500     ADD  KOLLI-SUORDV-KOLLI      TO VORD-SUORDV-FL                       
121600     ADD  KOLLI-SUORDV-LOC        TO VORD-SUORDV-FL-LOC                   
121700     ADD  KOLLI-SUORDV-LOCPREL    TO VORD-SUORDV-FL-LOCPREL               
121800     ADD  KOLLI-VKORDBTO-KOLLI    TO VORD-VKORDBTO-FL                     
121900     ADD  KOLLI-VLORDBTO-KOLLI    TO VORD-VLORDBTO-FL                     
122000     MOVE KOLLI-KDVALISO          TO VORD-KDVALISO                        
122100     PERFORM IMS-REPL-WDE601                                              
122200       MOVE 'SLUT HA-SEC '                   TO WS-PGM-POSITION           
122300     .                                                                    
122400     EJECT                                                                
122500 HE-SKAPA-WDE2 SECTION.                                                   
122600                                                                          
122700     INITIALIZE BILL-WDE201                                               
122800     MOVE SHNO-IDSHIPM                    TO BILL-IDSHIPM                 
122900                                             W-IDSHIPM                    
123000     MOVE MID-IDDC                        TO BILL-IDDC                    
123100     MOVE W-IDLANDX2                      TO BILL-IDLANDX3-SEND           
123200     MOVE ZERO                            TO BILL-IDLEVNR                 
123300     MOVE WS-DAGENS-DATUM                 TO BILL-TISKEPPN                
123400     MOVE FUNCTION CURRENT-DATE (13:2)    TO WS-TISKPTID-SS               
123500     MOVE WS-TISKPTID                     TO BILL-TISKPTID                
123600     EVALUATE TRUE                                                        
123700       WHEN VORD-KDFAKTYP = 'R' OR 'G'                                    
123800         MOVE 'INV'                       TO BILL-KDFINDOC                
123900       WHEN VORD-KDFAKTYP = 'K' OR 'N'                                    
124000         MOVE 'INT'                       TO BILL-KDFINDOC                
124100     END-EVALUATE                                                         
124200     MOVE VORD-IDDC-EXP                   TO BILL-IDDC-EXP                
124300     PERFORM IMS-ISRT-WDE201                                              
124400                                                                          
124500     INITIALIZE BGMT-WDE211                                               
124600     MOVE MID-IDDISTR                  TO BGMT-IDDISTR                    
124700                                          W-WDE211-IDDISTR                
124800     MOVE MID-IDKUNDNR                 TO BGMT-IDKUNDNR                   
124900                                          W-WDE211-IDKUNDNR               
125000     MOVE SPACE                        TO BGMT-IDPARTNR                   
125100     MOVE 'N'                          TO BGMT-FLCOD                      
125200     MOVE ZERO                         TO BGMT-PRFRAKT                    
125300     MOVE ZERO                         TO BGMT-PRFOERS                    
125400     MOVE ZERO                         TO BGMT-REFOERS                    
125500     MOVE ZERO                         TO BGMT-REOVKOFF                   
125600     MOVE ZERO                         TO BGMT-KDLEVVIL                   
125700     MOVE ZERO                         TO BGMT-PRAVDRAG                   
125800     MOVE ZERO                         TO BGMT-PREMBHNT                   
125900     MOVE ZERO                         TO BGMT-PRLEGKST                   
126000     MOVE ZERO                         TO BGMT-REAVDRAG                   
126100     MOVE ZERO                         TO BGMT-REEMBHNT                   
126200     MOVE ZERO                         TO BGMT-RELEGKST                   
126300     MOVE SPACE                        TO BGMT-FLSEPINV                   
126400     PERFORM IMS-ISRT-WDE211                                              
126500                                                                          
126600                                                                          
126700     INITIALIZE BGMT-WDE211                                               
126800     MOVE MID-IDPRODNR                 TO BKOLLI-IDPRODNR                 
126900     MOVE WS-IDKOLLI-NUM               TO BKOLLI-IDKOLLI                  
127000     MOVE ZERO                         TO BKOLLI-IDGMTREF                 
127100     MOVE SPACE                        TO BKOLLI-FLOVRLEV                 
127200     MOVE SPACE                        TO BKOLLI-KDFAKTYP                 
127300     MOVE ZERO                         TO BKOLLI-KDORDKL                  
127400     MOVE SPACE                        TO BKOLLI-KDPRSTA                  
127500     MOVE ZERO                         TO BKOLLI-VKORDBTO-KOLLI           
127510     MOVE SPACE                        TO BKOLLI-FLCROSS                  
127600     PERFORM IMS-ISRT-WDE221                                              
127700     .                                                                    
127800     EJECT                                                                
127900                                                                          
128000 HF-SKAPA-WDE1 SECTION.                                                   
128100                                                                          
128200     INITIALIZE SHIP-WDE101                                               
128300     MOVE SHNO-IDSHIPM                 TO SHIP-IDSHIPM                    
128400                                          W-IDSHIPM                       
128500     MOVE +999              TO SHIP-IDTRPTNR                              
128600     MOVE MID-IDDC          TO SHIP-IDDC                                  
128700     MOVE W-IDLANDX2        TO SHIP-IDLANDX3-SEND                         
128800     MOVE MSGI-TILOKDAT     TO SHIP-TISKEPPN                              
128900     MOVE WS-TISKPTID       TO SHIP-TISKPTID                              
129000     MOVE 'N'               TO SHIP-FLSKRIV-NU                            
129100     MOVE 'N'               TO SHIP-KDKLAR                                
129200     IF WS-IDANSTNR = '01441'                                             
129300       MOVE 'SOFTW'           TO SHIP-IDLBBET                             
129400     ELSE                                                                 
129500       MOVE 'DIRLEV'          TO SHIP-IDLBBET                             
129600     END-IF                                                               
129700     EVALUATE TRUE                                                        
129800       WHEN VORD-KDFAKTYP = 'R' OR 'G'                                    
129900         MOVE 'INV'         TO SHIP-KDFINDOC                              
130000       WHEN VORD-KDFAKTYP = 'K' OR 'N'                                    
130100         MOVE 'INT'         TO SHIP-KDFINDOC                              
130200     END-EVALUATE                                                         
130300     MOVE VORD-IDDC-EXP     TO SHIP-IDDC-EXP                              
130400     MOVE '0'               TO SHIP-KDFAKSTA-EXP                          
130500     MOVE ZERO              TO SHIP-SUNTO-TOT                             
130600                               SHIP-PRKURS-BET                            
130700     MOVE SPACE             TO SHIP-IDSYSTEM                              
130800                               SHIP-KDVALISO-BET                          
130810                                                                          
130900     MOVE NEJ               TO SHIP-FLFARLIG                              
130910     MOVE SPACES            TO SHIP-KDVALISO-EXP                          
130920     MOVE ZEROES            TO SHIP-SUORDV-EXP                            
130930                               SHIP-SUORDV-FAKT                           
130940                               SHIP-VKORDBTO-FAKT                         
130950                               SHIP-VLORDBTO-FAKT                         
131000     PERFORM IMS-ISRT-WDE101                                              
131100                                                                          
131200     INITIALIZE SGMT-WDE111                                               
131300     MOVE MID-IDDISTR       TO SGMT-IDDISTR                               
131400                               W-WDE111-IDDISTR                           
131500     MOVE MID-IDKUNDNR      TO SGMT-IDKUNDNR                              
131600                               W-WDE111-IDKUNDNR                          
131700     MOVE 'N'               TO SGMT-FLCOD                                 
131800     MOVE MID-IDDC          TO SGMT-IDDC                                  
131900     MOVE -1                TO SGMT-KDLEVVIL                              
132000     MOVE 9                 TO SGMT-KDORDKL-MAX                           
132100     COMPUTE SGMT-TISKEPPN-9KOMPL = 9999999 -                             
132200                                    SHIP-TISKEPPN                         
132300     MOVE  SPACE            TO  SGMT-IDPARTNR                             
132400     MOVE  ZERO             TO  SGMT-KDFORSKN                             
132500                                SGMT-KDFKBIL                              
132600     MOVE  SPACE            TO  SGMT-KDVALISO                             
132700     MOVE  ZERO             TO  SGMT-PRKURS                               
132800     COMPUTE SGMT-TISKEPPN-9KOMPL = 9999999 -                             
132900                                    SHIP-TISKEPPN                         
133000     PERFORM IMS-ISRT-WDE111                                              
133100                                                                          
133200     INITIALIZE  SKOLLI-WDE121                                            
133300     MOVE MID-IDPRODNR      TO SKOLLI-IDPRODNR                            
133400     MOVE WS-IDKOLLI-NUM    TO SKOLLI-IDKOLLI                             
133500     MOVE -1                TO SKOLLI-KDORDKL                             
133600     MOVE ZERO              TO SKOLLI-DIKOLLIB                            
133700                               SKOLLI-DIKOLLIH                            
133800                               SKOLLI-DIKOLLIL                            
133900     MOVE SPACE             TO SKOLLI-FLDIRLEV                            
134000     MOVE ZERO              TO SKOLLI-IDORDER                             
134100                               SKOLLI-KDEMBTYP                            
134200                               SKOLLI-KDFRAKT                             
134300                               SKOLLI-KDFARLIG-KOLLI                      
134400     MOVE SPACE             TO SKOLLI-KDKOLLI                             
134500     MOVE ZERO              TO SKOLLI-KVFLAMP-KOLLI                       
134600                               SKOLLI-SUORDV-LOC                          
134700                               SKOLLI-SUORDV-LOCPREL                      
134800     MOVE SPACE             TO SKOLLI-KDVALISO                            
134900                               SKOLLI-KDVALISO-EXP                        
135000     MOVE ZERO              TO SKOLLI-SUORDV                              
135100                               SKOLLI-SUORDV-EXP                          
135200                               SKOLLI-TIPACKN                             
135300                               SKOLLI-VKORDBTO-KOLLI                      
135400                               SKOLLI-VKORDNTO-KOLLI                      
135500                               SKOLLI-VLORDBTO-KOLLI                      
135600                               SKOLLI-IDDISTR                             
135700                               SKOLLI-IDKUNDNR                            
135800     MOVE SPACE             TO SKOLLI-IDKUNDRF                            
135900                               SKOLLI-KDFAKTYP                            
136000     MOVE ZERO              TO SKOLLI-TIORDREG                            
136100                               SKOLLI-IDFAKT                              
136200                               SKOLLI-IDFAKT-EXP                          
136300                               SKOLLI-IDKOLLI-SAMP                        
136310     MOVE SPACE             TO SKOLLI-FLCROSS                             
136400     PERFORM IMS-ISRT-WDE121                                              
136500                                                                          
136600     .                                                                    
136700     EJECT                                                                
136800 S06-BORTTAG-PRODTAB            SECTION.                                  
136900       MOVE 'STA S06-SEC   '                  TO WS-PGM-POSITION          
137000     PERFORM IMS-GU-XXKH11                                                
137100     MOVE 4448-KDPRCGRP          TO W-4488-KDPRCGRP                       
137200                                                                          
137300     PERFORM IMS-GHU-WDGX4490                                             
137400     IF SEGMENT-FINNS                                                     
137500        PERFORM IMS-DLET-WDGX4490                                         
137600     END-IF                                                               
137700       MOVE 'SLUT S06-SEC   '                 TO WS-PGM-POSITION          
137800     .                                                                    
137900     SKIP2                                                                
138000 S08-HAMTA-MASKINDATUM     SECTION.                                       
138100     SKIP3                                                                
138200     MOVE 'IDAG'                 TO   DAT-KDDATFORM                       
138300     CALL WDATKONV USING DAT-KDDATFORM                                    
138400                         DAT-I-TIDATUM                                    
138500                         DAT-O-TIDATUM                                    
138600                         DAT-KDSVAR                                       
138700     EJECT                                                                
138800     .                                                                    
138900 S09-INIT-KOLLI-VALUE      SECTION.                                       
139000                                                                          
139100     MOVE 1                 TO  KOLLI-KDKOLSTA                            
139200     MOVE WS-DAGENS-DATUM   TO  KOLLI-TIPACKN                             
139300     MOVE WS-HHMMSSDD       TO  WS-HHMMSSDD-RED                           
139400     MOVE WS-HHMMSS         TO  KOLLI-TIPACTID                            
139500     MOVE WS-DARFS          TO  KOLLI-DARFS                               
139600     MOVE WS-IDKOLLI-NUM    TO  KOLLI-IDKOLLI                             
139700     MOVE WS-IDANSTNR       TO  KOLLI-IDPLOCK                             
139800     MOVE WS-IDDISTR        TO  KOLLI-IDDISTR                             
139900     MOVE WS-IDKUNDNR-NUM   TO  KOLLI-IDKUNDNR                            
140000     MOVE WS-ADFLOMR        TO  KOLLI-ADFLOMR                             
140100     MOVE WS-ADRUTNIV       TO  KOLLI-ADRUTNIV                            
140200* WS-DIKOLLIX HAR VÄRDET NOLL.                                            
140300     MOVE WS-DIKOLLIL       TO  KOLLI-DIKOLLIL                            
140400     MOVE WS-DIKOLLIB       TO  KOLLI-DIKOLLIB                            
140500     MOVE WS-DIKOLLIH       TO  KOLLI-DIKOLLIH                            
140600     MOVE ACC-KOLLI-VKORDNTO TO KOLLI-VKORDBTO-KOLLI                      
140700     MOVE MID-IDDC          TO  KOLLI-IDDC                                
140800     MOVE WS-ADFLGEO        TO  KOLLI-ADFLGEO                             
140900     MOVE WS-IDFAKT-GNB     TO  KOLLI-IDSUPREF                            
141000     MOVE WS-KORD-KDORDKL   TO  KOLLI-KDORDKL                             
141100     MOVE WS-FLAUTFAK       TO  KOLLI-FLAUTFAK                            
141200     MOVE NEJ               TO  KOLLI-FLBANDST                            
141300                                KOLLI-FLFRSUTS                            
141400     MOVE ZERO              TO  KOLLI-IDKOLLI-FLER                        
141500                                KOLLI-IDKOLLI-SAMP                        
141600                                KOLLI-IDFAKT                              
141700                                KOLLI-IDFAKT-EXP                          
141800                                KOLLI-KDKOLLI                             
141900                                KOLLI-KDEMBTYP                            
142000                                KOLLI-IDTRPTNR                            
142100                                KOLLI-ADVMODUL                            
142200                                KOLLI-ADHMODUL                            
142300                                KOLLI-IDFAKLOP                            
142400                                KOLLI-DIDMODUL                            
142500                                KOLLI-DIHMODUL                            
142600                                KOLLI-KVFALRAD                            
142700                                KOLLI-KDORDKL                             
142800                                KOLLI-TIFAKT                              
142900                                KOLLI-TIFAKT-EXP                          
143000                                KOLLI-TIFAKTID                            
143100                                KOLLI-TIFAKTID-EXP                        
143200                                KOLLI-TILASTN                             
143300                                KOLLI-TILASTID                            
143400                                KOLLI-SUORDV-KOLLI                        
143500                                KOLLI-SUORDV-KLI-EXP                      
143600                                KOLLI-SUORDV-LOC                          
143700                                KOLLI-SUORDV-LOCPREL                      
143800                                KOLLI-KDARTURS-KOLLI                      
143900                                KOLLI-KVORDRAD                            
144000                                KOLLI-TIAAVVD-PATR                        
144100                                KOLLI-KDFARLIG-KOLLI                      
144200                                KOLLI-KVFLAMP-KOLLI                       
144300                                KOLLI-IDLASTN                             
144400                                KOLLI-SUEQFG                              
144500                                KOLLI-VLORDBTO-KOLLI                      
144600* OBS - HÄR INITIERAS NYA DDGS-FÄLT RENT GENERELLT                        
144700* OBS - KONTROLLERA OM DETTA ÄR KORREKT!!!                                
144800                                KOLLI-DASUPREF                            
144900                                KOLLI-TISUPTID                            
145000                                KOLLI-KDVIA                               
145100                                KOLLI-IDLEVNR                             
145200                                KOLLI-IDTULLNR                            
145300                                KOLLI-RETULKS                             
145400                                KOLLI-IDSHIPM                             
145500     MOVE SPACE              TO KOLLI-KDVALISO                            
145600     MOVE SPACE              TO KOLLI-KDVALISO-EXP                        
145700*                                                                         
145800     MOVE +1 TO FG-INDX                                                   
145900     PERFORM UNTIL FG-INDX > FG-MAX-INDX                                  
146000       MOVE ZERO             TO KOLLI-IDPSN(FG-INDX)                      
146100                                KOLLI-VKART-FG(FG-INDX)                   
146200                                KOLLI-VLFG(FG-INDX)                       
146300       ADD +1 TO FG-INDX                                                  
146400     END-PERFORM                                                          
146500*                                                                         
146600     MOVE SPACE             TO  KOLLI-FLUTLAST                            
146700                                KOLLI-FLTULLG                             
146800                                KOLLI-IDLBBET                             
146900                                KOLLI-IDTULFTG                            
147000                                KOLLI-KDSTASKLI                           
147100                                KOLLI-FILLERX2                            
147200     COMPUTE KOLLI-VLORDBTO-KOLLI ROUNDED =                               
147300     KOLLI-DIKOLLIL * KOLLI-DIKOLLIH * KOLLI-DIKOLLIB / 1000000           
147400*--------------------------------------- KOLLI BREDD, HÖJD OCH            
147500*--------------------------------------- LÄNGD ANGIVNA I CM MEDAN         
147600*--------------------------------------- BRUTTOVOLYM I KUBIK M.           
147700     MOVE KOLLI-VLORDBTO-KOLLI TO WS-KOLLI-VLORDBTO                       
147800     .                                                                    
147900     EJECT                                                                
148000 S10-UPPD-SPAR-KOLLI    SECTION.                                          
148100       MOVE 'STA S10-SEC '                    TO WS-PGM-POSITION          
148200     COMPUTE ACC-KOLLI-VKORDNTO ROUNDED = ACC-KOLLI-VKORDNTO +            
148300                    WS-ORAD-VKARTNTO * WS-ORAD-KVLEVART                   
148400     END-COMPUTE                                                          
148500*                                                                         
148600     COMPUTE ACC-ORAD-VLARTNTO = ACC-ORAD-VLARTNTO +                      
148700                          WS-ORAD-VLARTNTO * WS-ORAD-KVLEVART             
148800     END-COMPUTE                                                          
148900*                                                                         
149000                                                                          
149100     MOVE WS-IDDISTR     TO TEST-IDDISTR                                  
149200     IF DIST79-DEALER-PRICE                                               
149300       IF  ORAD-PRARTNTO-LOCPREL > 0                                      
149400         COMPUTE ACC-KOLLI-SUORDV-LOCPREL =                               
149500                   ACC-KOLLI-SUORDV-LOCPREL +                             
149600                   WS-ORAD-PRARTNTO-LOCPREL * WS-ORAD-KVLEVART            
149700         END-COMPUTE                                                      
149800       ELSE                                                               
149900         COMPUTE ACC-KOLLI-SUORDV-LOC =                                   
150000                   ACC-KOLLI-SUORDV-LOC +                                 
150100                   WS-ORAD-PRARTNTO-LOC * WS-ORAD-KVLEVART                
150200          END-COMPUTE                                                     
150300       END-IF                                                             
150400     ELSE                                                                 
150500       IF DIST79-ECOM-PRICE                                               
150600         COMPUTE ACC-KOLLI-SUORDV-LOC =                                   
150700                   ACC-KOLLI-SUORDV-LOC +                                 
150800                   WS-ORAD-PRARTNTO-LOC * WS-ORAD-KVLEVART                
150900       ELSE                                                               
151000         COMPUTE ACC-KOLLI-SUORDV = ACC-KOLLI-SUORDV +                    
151100                      WS-ORAD-PRARTNTO * WS-ORAD-KVLEVART                 
151200         END-COMPUTE                                                      
151300       END-IF                                                             
151400     END-IF                                                               
151500     MOVE WS-ORAD-KDVALISO   TO ACC-KOLLI-KDVALISO                        
151600*                                                                         
151700     IF   WS-ORAD-KVFLAMP     >  ZERO                                     
151800     AND (WS-ORAD-KVFLAMP     <  ACC-KOLLI-KVFLAMP                        
151900     OR   ACC-KOLLI-KVFLAMP   =  ZERO)                                    
152000       MOVE WS-ORAD-KVFLAMP    TO ACC-KOLLI-KVFLAMP                       
152100     END-IF                                                               
152200                                                                          
152300     IF  WS-ORAD-KDFARLIG    =  +2 OR +3 OR +4 OR +7                      
152400     AND WS-ORAD-KDFARLIG    >  ACC-KOLLI-KDFARLIG                        
152500       MOVE WS-ORAD-KDFARLIG TO ACC-KOLLI-KDFARLIG                        
152600     END-IF                                                               
152700*                                                                         
152800       MOVE 'SLUT S10-SEC '                   TO WS-PGM-POSITION          
152900     .                                                                    
153000     EJECT                                                                
153100 S11-UPPD-KOLLI-FRAN-ARB   SECTION.                                       
153200       MOVE 'STA S11-SEC '                    TO WS-PGM-POSITION          
153300     MOVE ACC-KOLLI-KVFALRAD      TO KOLLI-KVFALRAD                       
153400     MOVE ACC-KOLLI-KVORDRAD      TO KOLLI-KVORDRAD                       
153500     MOVE ACC-KOLLI-VKORDNTO      TO KOLLI-VKORDNTO-KOLLI                 
153600                                     KOLLI-VKORDBTO-KOLLI                 
153700*                                                                         
153800     IF KOLLI-VKORDNTO-KOLLI >= KOLLI-VKORDBTO-KOLLI                      
153900       ADD 0.1                      TO KOLLI-VKORDBTO-KOLLI               
154000     END-IF                                                               
154100*                                                                         
154200     MOVE ACC-KOLLI-SUORDV        TO KOLLI-SUORDV-KOLLI                   
154300     MOVE ACC-KOLLI-SUORDV-LOC    TO KOLLI-SUORDV-LOC                     
154400     MOVE ACC-KOLLI-SUORDV-LOCPREL TO KOLLI-SUORDV-LOCPREL                
154500     MOVE ACC-KOLLI-KDVALISO      TO KOLLI-KDVALISO                       
154600*                                                                         
154700     IF      ACC-KOLLI-KVFLAMP   >  ZERO                                  
154800        AND (ACC-KOLLI-KVFLAMP   <  KOLLI-KVFLAMP-KOLLI                   
154900        OR   KOLLI-KVFLAMP-KOLLI =  ZERO)                                 
155000       MOVE ACC-KOLLI-KVFLAMP    TO KOLLI-KVFLAMP-KOLLI                   
155100     END-IF                                                               
155200*                                                                         
155300     IF ACC-KOLLI-KDFARLIG       >  KOLLI-KDFARLIG-KOLLI                  
155400         MOVE ACC-KOLLI-KDFARLIG TO KOLLI-KDFARLIG-KOLLI                  
155500     END-IF                                                               
155600                                                                          
155700     IF KOLLI-KDFARLIG-KOLLI = +4                                         
155800     OR KOLLI-KDFARLIG-KOLLI = +7                                         
155900       MOVE +950                    TO KOLLI-ADFLOMR                      
156000     END-IF                                                               
156100       MOVE 'SLUT S11-SEC '                   TO WS-PGM-POSITION          
156200     .                                                                    
156300     EJECT                                                                
156400 S13-UPPDAT-KDORDSTA SECTION.                                             
156500       MOVE 'STA S13-SEC  '                   TO WS-PGM-POSITION          
156600       MOVE  WS-KORD-IDORDER      TO W-201-IDORDER                        
156700       MOVE    MID-IDDC           TO W-212-IDDC                           
156800       PERFORM IMS-GHU-ORQI12                                             
156900       IF ARB-KDORDSTA = 'U '                                             
157000         MOVE 'U*'                TO ARB-KDORDSTA                         
157100         PERFORM IMS-REPL-ORQI12                                          
157200       END-IF                                                             
157300       MOVE 'SLUT S13-SEC '                   TO WS-PGM-POSITION          
157400     .                                                                    
157500     SKIP2                                                                
157600 S16-UPPD-FARLIGT-GODS-DATA SECTION.                                      
157700       MOVE 'STA S16-SEC '                    TO WS-PGM-POSITION          
157800                                                                          
157900     MOVE +1 TO FG-INDX                                                   
158000     PERFORM UNTIL FG-INDX > FG-MAX-INDX                                  
158100                                                                          
158200       IF TAB-IDPSN(FG-INDX) > ZERO                                       
158300         MOVE TAB-IDPSN(FG-INDX)    TO KOLLI-IDPSN(FG-INDX)               
158400         COMPUTE KOLLI-VKART-FG(FG-INDX) =                                
158500                                       TAB-VKART-FG(FG-INDX) /            
158600                                       KOLLI-VALUE-ETT                    
158700         END-COMPUTE                                                      
158800         COMPUTE KOLLI-VLFG(FG-INDX) = TAB-VLFG(FG-INDX) /                
158900                                       KOLLI-VALUE-ETT                    
159000         END-COMPUTE                                                      
159100       ELSE                                                               
159200         MOVE ZERO                  TO KOLLI-IDPSN(FG-INDX)               
159300                                       KOLLI-VKART-FG(FG-INDX)            
159400                                       KOLLI-VLFG(FG-INDX)                
159500       END-IF                                                             
159600                                                                          
159700       ADD +1 TO FG-INDX                                                  
159800     END-PERFORM                                                          
159900                                                                          
160000     IF TAB-IDPSN(1) > ZERO                                               
160100       COMPUTE KOLLI-SUEQFG = TOTAL-SUEQFG / KOLLI-VALUE-ETT              
160200     ELSE                                                                 
160300       MOVE ZERO TO KOLLI-SUEQFG                                          
160400     END-IF                                                               
160500       MOVE 'SLUT S16-SEC '                   TO WS-PGM-POSITION          
160600     .                                                                    
160700     EJECT                                                                
160800 S17-BERAEKNA-FG-FAELT SECTION.                                           
160900     MOVE 'STA S17-SEC '                    TO WS-PGM-POSITION            
161000     COMPUTE TAB-VLFG(FG-INDX) = TAB-VLFG(FG-INDX) +                      
161100                                 (WS-ORAD-VLFG          *                 
161200                                  KKOLLI-KVLEVART)                        
161300     END-COMPUTE                                                          
161400     IF WS-ORAD-IDPSN = 10 OR 11                                          
161500       COMPUTE TAB-VKART-FG(FG-INDX) = TAB-VKART-FG(FG-INDX) +            
161600                                       (WS-ORAD-VKART-FG     *            
161700                                        KKOLLI-KVLEVART)                  
161800       END-COMPUTE                                                        
161900     ELSE                                                                 
162000       MOVE ZERO TO TAB-VKART-FG(FG-INDX)                                 
162100     END-IF                                                               
162200                                                                          
162300     MOVE 10 TO FG-INDX                                                   
162400     MOVE 'SLUT S17-SEC '                   TO WS-PGM-POSITION            
162500     .                                                                    
162600     EJECT                                                                
162700 S21-INIT-WS-FIELDS    SECTION.                                           
162800     MOVE 'STA S21-SEC  '                   TO WS-PGM-POSITION            
162900                                                                          
163000     MOVE ZERO                   TO INX-TOT-ANT-RADER                     
163100                                    ACC-KOLLI-VKORDNTO                    
163200                                    ACC-ORAD-VLARTNTO                     
163300                                    ACC-ORAD-VKARTNTO                     
163400                                    ACC-ORAD-PRARTNTO                     
163500                                    ACC-ORAD-PRARTNTO-LOC                 
163600                                    ACC-ORAD-PRARTNTO-LOCPREL             
163700                                    WS-ORAD-KVFLAMP                       
163800                                    WS-ORAD-KDFARLIG                      
163900                                    WS-ORAD-PRARTNTO                      
164000                                    WS-ORAD-PRARTNTO-LOC                  
164100                                    WS-ORAD-PRARTNTO-LOCPREL              
164200                                    WS-ORAD-VKARTNTO                      
164300                                    WS-ORAD-KVLEVART                      
164400                                    WS-ORAD-IDPSN                         
164500                                    WS-ORAD-VKART-FG                      
164600                                    WS-ORAD-VLFG                          
164700                                    WS-ORAD-SUEQFG                        
164800                                    TOTAL-SUEQFG                          
164900                                    WS-ANT-KLARA-RADER                    
165000                                    WS-KORD-KDORDKL                       
165100                                    WS-KORD-IDORDER                       
165200                                    WS-KOLLI-VLORDBTO                     
165300                                    WS-DIKOLLIL                           
165400                                    WS-DIKOLLIB                           
165500                                    WS-DIKOLLIH                           
165600                                    WS-ADFLOMR                            
165700                                    WS-ADRUTNIV                           
165800                                    WS-DARFS                              
165900                                    WS-KDORDSTA                           
166000                                    WS-TTMMSS                             
166100                                    WS-HH                                 
166200                                    WS-HHMMSS                             
166300                                    WS-DD                                 
166400                                    ACC-KOLLI-VKORDNTO                    
166500                                    ACC-KOLLI-KVFLAMP                     
166600                                    ACC-KOLLI-KDFARLIG                    
166700                                    ACC-KOLLI-KVORDRAD                    
166800                                    ACC-KOLLI-KVFALRAD                    
166900                                    ACC-KOLLI-SUORDV                      
167000                                    ACC-KOLLI-SUORDV-LOC                  
167100                                    ACC-KOLLI-SUORDV-LOCPREL              
167200                                    KOLLI-VALUE-ETT                       
167300                                    WS-DIKOLLIL                           
167400                                    WS-DIKOLLIB                           
167500                                    WS-DIKOLLIH                           
167600                                                                          
167700     MOVE SPACE                  TO WS-ADFLGEO                            
167800                                    WS-FLAUTFAK                           
167900                                    ACC-KOLLI-KDVALISO                    
168000                                    WS-ORAD-KDVALISO                      
168100                                    WS-INDATA-TEST                        
168200     IF  MID-IDANSTNR = '01441'                                           
168300         MOVE +800               TO WS-IDKOLLI-NUM                        
168400     ELSE                                                                 
168500         MOVE +1                 TO WS-IDKOLLI-NUM                        
168600     END-IF                                                               
168700     MOVE 'SLUT S21-SEC '                   TO WS-PGM-POSITION            
168800     .                                                                    
168900     EJECT                                                                
169000 S23-OPEN-WZ01 SECTION.                                                   
169100                                                                          
169200     MOVE 'OPEN'                     TO SEND-KDFUNC                       
169300     MOVE 'CARPARTS.PULS.ADDIT   '   TO SEND-ADDISPABS                    
169400     MOVE SPACE                      TO SEND-ADDISPABS-RETURN             
169500                                                                          
169600     CALL WZ01SEND   USING      SEND-CONTROL-AREA                         
169700                                SEND-OPEN-AREA                            
169800     IF SEND-KDRC > 0                                                     
169900       MOVE SEND-KDRC           TO KDRC-DISP                              
170000       STRING 'WZ01SEND-OPEN RC-ERR = ' KDRC-DISP                         
170100            DELIMITED BY SIZE INTO ERROR-TEXT                             
170200       CALL FELLOG                                                        
170300     ELSE                                                                 
170400       MOVE SEND-IDCOM               TO WS-IDCOM                          
170500     END-IF                                                               
170600     .                                                                    
170700     EJECT                                                                
170800 S24-SEND-WZ01 SECTION.                                                   
170900     MOVE W-IDSHIPM          TO MOD4636-MID-IDSHIPM                       
171000     MOVE 'N'                TO MOD4636-MID-KDTRPINF                      
171100     MOVE ZERO               TO MOD4636-MID-IDDISTR                       
171200                                MOD4636-MID-IDKUNDNR                      
171300                                MOD4636-MID-IDPRODNR                      
171400                                MOD4636-MID-IDKOLLI                       
171500                                MOD4636-MID-SUORDV-DIST                   
171600                                MOD4636-MID-SUORDV-DIST-LOC               
171700                                MOD4636-MID-SUORDV-DIST-PREL              
171800                                MOD4636-MID-SUORDV-NOLL                   
171900                                MOD4636-MID-SUORDV-NOLL-LOC               
172000                                MOD4636-MID-SUORDV-NOLL-PREL              
172100                                MOD4636-MID-KDORDKL                       
172200     MOVE 'PUT'                      TO SEND-KDFUNC                       
172300     COMPUTE SEND-KVDLEN = LENGTH OF MOD4636-MID-W40636I1                 
172400                                                                          
172500     CALL WZ01SEND   USING      SEND-CONTROL-AREA                         
172600                                SEND-KVDLEN                               
172700                                MOD4636-MID-W40636I1                      
172800     IF SEND-KDRC > 0                                                     
172900       MOVE SEND-KDRC           TO KDRC-DISP                              
173000       STRING 'WZ01SEND-PUT RC-ERR = ' KDRC-DISP                          
173100            DELIMITED BY SIZE INTO ERROR-TEXT                             
173200       CALL FELLOG                                                        
173300**  DVKTST  START                                                         
173400*    ELSE                                                                 
173500*       ADD  1  TO DVK-INDX                                               
173600*       STRING 'ADDIT*' MOD4636-MID-IDSHIPM '*'                           
173700*               DELIMITED BY SIZE INTO DVK-TEXT (DVK-INDX)                
173800**  DVKTST  END                                                           
173900     END-IF                                                               
174000     .                                                                    
174100     EJECT                                                                
174200 S25-CLOSE-WZ01  SECTION.                                                 
174300                                                                          
174400     MOVE 'CLOSE'               TO SEND-KDFUNC                            
174500                                                                          
174600     CALL WZ01SEND   USING      SEND-CONTROL-AREA                         
174700     IF SEND-KDRC > 0                                                     
174800       MOVE SEND-KDRC           TO KDRC-DISP                              
174900       STRING 'WZ01SEND-CLOSE RC-ERR = ' KDRC-DISP                        
175000            DELIMITED BY SIZE INTO ERROR-TEXT                             
175100       CALL FELLOG                                                        
175200     END-IF                                                               
175300     .                                                                    
175400     EJECT                                                                
175500 S26-DC-LAND  SECTION.                                                    
175600                                                                          
175700     MOVE MID-IDDC TO W-IDDC-B6                                           
175800     PERFORM IMS-GU-WDB601                                                
175900     MOVE DCS-IDLANDX2 TO W-IDLANDX2                                      
176000     .                                                                    
176100     EJECT                                                                
176200                                                                          
176300 S27-SHIPM-NUMBER SECTION.                                                
176400                                                                          
176500     CALL W476SHNO USING SHNO-W476SHNO 4517-PCB                           
176600                                                                          
176700     MOVE SHNO-IDSHIPM         TO W-IDSHIPM                               
176800     .                                                                    
176900     EJECT                                                                
177000                                                                          
177100 Z-DISPATCH-AVSLUT     SECTION.                                           
177200                                                                          
177300*    SKRIV FEL/KLAR MEDDELANDE TILL MPP DISPATCHERN                       
177400     IF MSG-KOM-IDMFSMED = SPACE                                          
177500        MOVE OK-BEHANDLAD      TO MSG-KOM-IDMFSMED                        
177600     END-IF                                                               
177700     PERFORM IMS-INSERT-DISP-MSG                                          
177800     .                                                                    
177900     EJECT                                                                
178000*IMS SEKTIONER                                                            
178100*                                                                         
178200*                 IIIIIIIIIII MMMMMMMMMMM SSSSSSSSSSS                     
178300*                 III     III MM MMMMM MM SSSS   SSSS                     
178400*                 IIIII IIIII MM  MMM  MM SSS SSS SSS                     
178500*                 IIIII IIIII MM M M M MM SSS  SSSSSS                     
178600*                 IIIII IIIII MM MM MM MM SSSSSS  SSS                     
178700*                 IIIII IIIII MM MMMMM MM SSS SSS SSS                     
178800*                 III     III MM MMMMM MM SSSS   SSSS                     
178900*                 IIIIIIIIIII MMMMMMMMMMM SSSSSSSSSSS                     
179000*                                                                         
179100*                                                                         
179200 IMS-GU-MSG-AREA SECTION.                                                 
179300                                                                          
179400     MOVE '  QC' TO GODK-STATUSKODER                                      
179500     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
179600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
179700     PERFORM IMS-STATUSKONTROLL                                           
179800     .                                                                    
179900     SKIP3                                                                
180000 IMS-GN-KOM-AREA SECTION.                                                 
180100                                                                          
180200     MOVE '  '   TO GODK-STATUSKODER                                      
180300     CALL CBLTDLI USING GN MSG-PCB KOM-IO-AREA                            
180400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
180500     PERFORM IMS-STATUSKONTROLL                                           
180600     .                                                                    
180700     SKIP2                                                                
180800 IMS-INSERT-DISP-MSG SECTION.                                             
180900                                                                          
181000     MOVE SPACE TO GODK-STATUSKODER                                       
181100     CALL CBLTDLI USING ISRT DISP-PCB KOM-IO-AREA                         
181200     MOVE DISP-STATUS-CODE TO STATUS-WS                                   
181300     PERFORM IMS-STATUSKONTROLL                                           
181400     .                                                                    
181500     SKIP2                                                                
181600 IMS-GU-WDE401 SECTION.                                                   
181700     STRING 'WDE401  (WDE401KY =' W-WDE401-KUNDORDER-X ')'                
181800            DELIMITED BY SIZE INTO SSA1                                   
181900     MOVE '  GE' TO GODK-STATUSKODER                                      
182000     CALL CBLTDLI USING GU     WDE4-PCB KORD-WDE401 SSA1                  
182100     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
182200     PERFORM IMS-STATUSKONTROLL                                           
182300     SKIP3                                                                
182400     .                                                                    
182500 IMS-GNP-WDE411 SECTION.                                                  
182600     STRING 'WDE411  (IDPURAD  =' W-WDE411-IDPURAD-X ')'                  
182700            DELIMITED BY SIZE INTO SSA1                                   
182800     MOVE '  GE' TO GODK-STATUSKODER                                      
182900     CALL CBLTDLI USING GNP    WDE4-PCB ORAD-WDE411 SSA1                  
183000     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
183100     PERFORM IMS-STATUSKONTROLL                                           
183200     SKIP3                                                                
183300     .                                                                    
183400 IMS-REPL-WDE401    SECTION.                                              
183500     MOVE '  '   TO GODK-STATUSKODER                                      
183600     CALL CBLTDLI USING REPL WDE4-PCB KORD-WDE401                         
183700     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
183800     PERFORM IMS-STATUSKONTROLL                                           
183900     SKIP2                                                                
184000     .                                                                    
184100 IMS-GHU-WDE401              SECTION.                                     
184200     STRING 'WDE401  (WDE401KY =' W-WDE401-KUNDORDER-X ')'                
184300          DELIMITED BY SIZE INTO SSA1                                     
184400     MOVE '    ' TO GODK-STATUSKODER                                      
184500     CALL CBLTDLI USING GHU    WDE42-PCB KORD-WDE401 SSA1                 
184600     MOVE WDE42-STATUS-CODE TO STATUS-WS                                  
184700     PERFORM IMS-STATUSKONTROLL                                           
184800     SKIP3                                                                
184900     .                                                                    
185000 IMS-GHNP-WDE411            SECTION.                                      
185100     STRING 'WDE411  (IDPURAD  =' W-WDE411-IDPURAD-X ')'                  
185200          DELIMITED BY SIZE INTO SSA1                                     
185300     MOVE '    ' TO GODK-STATUSKODER                                      
185400     CALL CBLTDLI USING GHNP   WDE42-PCB ORAD-WDE411 SSA1                 
185500     MOVE WDE42-STATUS-CODE TO STATUS-WS                                  
185600     PERFORM IMS-STATUSKONTROLL                                           
185700     .                                                                    
185800 IMS-GHNP-WDE411-F          SECTION.                                      
185900     STRING 'WDE411  *F(IDPURAD  =' W-WDE411-IDPURAD-X ')'                
186000          DELIMITED BY SIZE INTO SSA1                                     
186100     MOVE '    ' TO GODK-STATUSKODER                                      
186200     CALL CBLTDLI USING GHNP   WDE42-PCB ORAD-WDE411 SSA1                 
186300     MOVE WDE42-STATUS-CODE TO STATUS-WS                                  
186400     PERFORM IMS-STATUSKONTROLL                                           
186500     .                                                                    
186600 IMS-REPL-BEHANDLAD-RAD SECTION.                                          
186700     MOVE '    ' TO GODK-STATUSKODER                                      
186800     CALL CBLTDLI USING REPL WDE42-PCB ORAD-WDE411                        
186900     MOVE WDE42-STATUS-CODE TO STATUS-WS                                  
187000     PERFORM IMS-STATUSKONTROLL                                           
187100     SKIP3                                                                
187200     .                                                                    
187300 IMS-GHNP-KOLLI-KOPPL    SECTION.                                         
187400     STRING 'WDE421  *F(WDE401KY =' W-WDE421-IDKOLLI-X ')'                
187500            DELIMITED BY SIZE INTO SSA1                                   
187600     MOVE '  ' TO GODK-STATUSKODER                                        
187700     CALL CBLTDLI USING GHNP WDE42-PCB KKOLLI-WDE421 SSA1                 
187800     MOVE WDE42-STATUS-CODE TO STATUS-WS                                  
187900     PERFORM IMS-STATUSKONTROLL                                           
188000     SKIP3                                                                
188100     .                                                                    
188200 IMS-REPL-KOLLI-KOPPL  SECTION.                                           
188300     MOVE '  '   TO GODK-STATUSKODER                                      
188400     CALL CBLTDLI USING REPL WDE42-PCB KKOLLI-WDE421                      
188500     MOVE WDE42-STATUS-CODE TO STATUS-WS                                  
188600     PERFORM IMS-STATUSKONTROLL                                           
188700     SKIP2                                                                
188800     .                                                                    
188900 IMS-ISRT-KOLLI-KOPPL  SECTION.                                           
189000     MOVE   'WDE421   '       TO   SSA1                                   
189100     MOVE '  II' TO GODK-STATUSKODER                                      
189200     CALL CBLTDLI USING ISRT WDE42-PCB KKOLLI-WDE421 SSA1                 
189300     MOVE WDE42-STATUS-CODE TO STATUS-WS                                  
189400     PERFORM IMS-STATUSKONTROLL                                           
189500     .                                                                    
189600     EJECT                                                                
189700 IMS-GHU-WDE601           SECTION.                                        
189800     STRING 'WDE601  (IDPRODNR =' W-WDE601-IDPRODNR-X ')'                 
189900            DELIMITED BY SIZE INTO SSA1                                   
190000     MOVE '    ' TO GODK-STATUSKODER                                      
190100     CALL CBLTDLI USING GHU WDE6-PCB VORD-WDE601 SSA1                     
190200     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
190300     PERFORM IMS-STATUSKONTROLL                                           
190400     .                                                                    
190500     EJECT                                                                
190600 IMS-REPL-WDE601 SECTION.                                                 
190700     MOVE '    ' TO GODK-STATUSKODER                                      
190800     CALL CBLTDLI USING REPL WDE6-PCB VORD-WDE601                         
190900     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
191000     PERFORM IMS-STATUSKONTROLL                                           
191100     SKIP3                                                                
191200     .                                                                    
191300 IMS-REPL-WDE611 SECTION.                                                 
191400     MOVE '    ' TO GODK-STATUSKODER                                      
191500     CALL CBLTDLI USING REPL WDE6-PCB KOLLI-WDE611                        
191600     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
191700     PERFORM IMS-STATUSKONTROLL                                           
191800     SKIP3                                                                
191900     .                                                                    
192000 IMS-GHU-WDE611 SECTION.                                                  
192100     STRING 'WDE601  (IDPRODNR =' W-WDE601-IDPRODNR-X ')'                 
192200            DELIMITED BY SIZE INTO SSA1                                   
192300     STRING 'WDE611  (IDKOLLI  =' W-WDE611-IDKOLLI-X ')'                  
192400            DELIMITED BY SIZE INTO SSA2                                   
192500     MOVE '    ' TO GODK-STATUSKODER                                      
192600     CALL CBLTDLI USING GHU    WDE6-PCB KOLLI-WDE611 SSA1 SSA2            
192700     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
192800     PERFORM IMS-STATUSKONTROLL                                           
192900     SKIP3                                                                
193000     .                                                                    
193100 IMS-REPL-KOLLI    SECTION.                                               
193200     MOVE '    ' TO GODK-STATUSKODER                                      
193300     CALL CBLTDLI USING REPL WDE6-PCB KOLLI-WDE611                        
193400     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
193500     PERFORM IMS-STATUSKONTROLL                                           
193600     SKIP3                                                                
193700     .                                                                    
193800 IMS-ISRT-KOLLI   SECTION.                                                
193900     STRING 'WDE601  (IDPRODNR =' W-WDE601-IDPRODNR-X ')'                 
194000            DELIMITED BY SIZE INTO SSA1                                   
194100     MOVE   'WDE611   '       TO   SSA2                                   
194200     MOVE '  II' TO GODK-STATUSKODER                                      
194300     CALL CBLTDLI USING ISRT WDE6-PCB KOLLI-WDE611 SSA1 SSA2              
194400     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
194500     PERFORM IMS-STATUSKONTROLL                                           
194600     .                                                                    
194700     EJECT                                                                
194800 IMS-GHU-KUNDORDER SECTION.                                               
194900     STRING 'WDE401  (WDE401KY =' W-WDE401-KUNDORDER-X ')'                
195000            DELIMITED BY SIZE INTO SSA1                                   
195100     MOVE '    ' TO GODK-STATUSKODER                                      
195200     CALL CBLTDLI USING GHU    WDE4-PCB KORD-WDE401 SSA1                  
195300     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
195400     PERFORM IMS-STATUSKONTROLL                                           
195500     SKIP2                                                                
195600     .                                                                    
195700 IMS-GU-ORQA-STATUS    SECTION.                                           
195800                                                                          
195900     STRING 'WLORQA01(WDQ301KY>=' W-WDQ301KY-MIN                          
196000                    '&WDQ301KY<=' W-WDQ301KY-MAX                          
196100                    '&KDODELST =' W-KDODELST     ')'                      
196200          DELIMITED BY SIZE INTO SSA1                                     
196300     MOVE '  GE' TO GODK-STATUSKODER                                      
196400     CALL CBLTDLI USING GU  ORQA-PCB ODEL-WDQ301 SSA1                     
196500     MOVE ORQA-STATUS-CODE TO STATUS-WS                                   
196600     PERFORM IMS-STATUSKONTROLL                                           
196700     .                                                                    
196800 IMS-REPL-ORQA01        SECTION.                                          
196900     MOVE '    ' TO GODK-STATUSKODER                                      
197000     CALL CBLTDLI USING REPL ORQA-PCB ODEL-WDQ301                         
197100     MOVE ORQA-STATUS-CODE TO STATUS-WS                                   
197200     PERFORM IMS-STATUSKONTROLL                                           
197300     .                                                                    
197400     EJECT                                                                
197500 IMS-GHU-ORQA01   SECTION.                                                
197600     STRING 'WLORQA01(WDQ301KY =' W-WDQ301-KEY-X ')'                      
197700            DELIMITED BY SIZE INTO SSA1                                   
197800     MOVE '  ' TO GODK-STATUSKODER                                        
197900     CALL CBLTDLI USING GHU    ORQA-PCB ODEL-WDQ301 SSA1                  
198000     MOVE ORQA-STATUS-CODE TO STATUS-WS                                   
198100     PERFORM IMS-STATUSKONTROLL                                           
198200     .                                                                    
198300 IMS-GU-ORQI01    SECTION.                                                
198400     STRING 'WLORQI01(IDORDER  =' W-WDQ201-X ')'                          
198500            DELIMITED BY SIZE INTO SSA1                                   
198600     MOVE '  GE' TO GODK-STATUSKODER                                      
198700     CALL CBLTDLI USING GU   ORQI-PCB OHUV-WDQ201 SSA1                    
198800     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
198900     PERFORM IMS-STATUSKONTROLL                                           
199000     .                                                                    
199100 IMS-GHU-ORQI12    SECTION.                                               
199200     STRING 'WLORQI01(IDORDER  =' W-WDQ201-X ')'                          
199300            DELIMITED BY SIZE INTO SSA1                                   
199400     STRING 'WLORQI12(IDDC     =' W-WDQ212-IDDC-X ')'                     
199500            DELIMITED BY SIZE INTO SSA2                                   
199600     MOVE '    ' TO GODK-STATUSKODER                                      
199700     CALL CBLTDLI USING GHU   ORQI-PCB ARB-WDQ212 SSA1 SSA2               
199800     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
199900     PERFORM IMS-STATUSKONTROLL                                           
200000     .                                                                    
200100 IMS-REPL-ORQI12      SECTION.                                            
200200     MOVE '  ' TO GODK-STATUSKODER                                        
200300     CALL CBLTDLI USING REPL ORQI-PCB ARB-WDQ212                          
200400     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
200500     PERFORM IMS-STATUSKONTROLL                                           
200600     .                                                                    
200700     EJECT                                                                
200800 IMS-GU-XXKH11 SECTION.                                                   
200900                                                                          
201000     STRING 'WLXXKH01(WDGXKEY  =' W-4447-X    ')'                         
201100            DELIMITED BY SIZE INTO SSA1                                   
201200     STRING 'WLXXKH11(WDGXKEY  =' W-4448-X    ')'                         
201300            DELIMITED BY SIZE INTO SSA2                                   
201400     MOVE '  '   TO GODK-STATUSKODER                                      
201500     CALL CBLTDLI USING GU XXKH-PCB 4448-WDGX4448-CTX SSA1 SSA2           
201600     MOVE XXKH-STATUS-CODE TO STATUS-WS                                   
201700     PERFORM IMS-STATUSKONTROLL                                           
201800     SKIP2                                                                
201900     .                                                                    
202000     EJECT                                                                
202100 IMS-GHU-WDGX4490   SECTION.                                              
202200     STRING 'WDR401  (WDGXKEY  =' W-4487-X ')'                            
202300         DELIMITED BY SIZE INTO SSA1                                      
202400     STRING 'WDGX4488(KDPRCGRP =' W-4488-X ')'                            
202500         DELIMITED BY SIZE INTO SSA2                                      
202600     STRING 'WDGX4490(KY4490   =' W-4490-X ')'                            
202700         DELIMITED BY SIZE INTO SSA3                                      
202800     MOVE '  GE' TO GODK-STATUSKODER                                      
202900     CALL CBLTDLI USING GHU 4487-PCB 4490-WDGX4490 SSA1 SSA2 SSA3         
203000     MOVE 4487-STATUS-CODE TO STATUS-WS                                   
203100     PERFORM IMS-STATUSKONTROLL                                           
203200     .                                                                    
203300     SKIP2                                                                
203400 IMS-DLET-WDGX4490 SECTION.                                               
203500     MOVE '  ' TO GODK-STATUSKODER                                        
203600     CALL CBLTDLI USING DLET 4487-PCB 4490-WDGX4490                       
203700     MOVE 4487-STATUS-CODE TO STATUS-WS                                   
203800     PERFORM IMS-STATUSKONTROLL                                           
203900     .                                                                    
204000     SKIP2                                                                
204100 IMS-ISRT-WDE201 SECTION.                                                 
204200                                                                          
204300     MOVE 'WDE201  ' TO SSA1                                              
204400     MOVE '  II' TO GODK-STATUSKODER                                      
204500     CALL CBLTDLI USING ISRT WDE2-PCB DLI-IO-WDE201 SSA1                  
204600     MOVE WDE2-STATUS-CODE TO STATUS-WS                                   
204700     PERFORM IMS-STATUSKONTROLL                                           
204800     .                                                                    
204900     EJECT                                                                
205000                                                                          
205100 IMS-ISRT-WDE211 SECTION.                                                 
205200                                                                          
205300     STRING 'WDE201  (IDSHIPM  =' W-IDSHIPM-X ')'                         
205400          DELIMITED BY SIZE INTO SSA1                                     
205500     MOVE 'WDE211  ' TO SSA2                                              
205600     MOVE '  II' TO GODK-STATUSKODER                                      
205700     CALL CBLTDLI USING ISRT WDE2-PCB DLI-IO-WDE211 SSA1 SSA2             
205800     MOVE WDE2-STATUS-CODE TO STATUS-WS                                   
205900     PERFORM IMS-STATUSKONTROLL                                           
206000     .                                                                    
206100     EJECT                                                                
206200                                                                          
206300 IMS-ISRT-WDE221 SECTION.                                                 
206400                                                                          
206500     STRING 'WDE201  (IDSHIPM  =' W-IDSHIPM-X ')'                         
206600          DELIMITED BY SIZE INTO SSA1                                     
206700     STRING 'WDE211  (WDE211KY =' W-WDE211KY-X ')'                        
206800          DELIMITED BY SIZE INTO SSA2                                     
206900     MOVE 'WDE221  ' TO SSA3                                              
207000     MOVE '  II' TO GODK-STATUSKODER                                      
207100     CALL CBLTDLI USING ISRT WDE2-PCB DLI-IO-WDE221 SSA1 SSA2 SSA3        
207200     MOVE WDE2-STATUS-CODE TO STATUS-WS                                   
207300     PERFORM IMS-STATUSKONTROLL                                           
207400     .                                                                    
207500     EJECT                                                                
207600 IMS-ISRT-WDE101 SECTION.                                                 
207700                                                                          
207800     MOVE 'WDE101  ' TO SSA1                                              
207900     MOVE '  II' TO GODK-STATUSKODER                                      
208000     CALL CBLTDLI USING ISRT WDE1-PCB DLI-IO-WDE101 SSA1                  
208100     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
208200     PERFORM IMS-STATUSKONTROLL                                           
208300     .                                                                    
208400     EJECT                                                                
208500                                                                          
208600 IMS-ISRT-WDE111 SECTION.                                                 
208700                                                                          
208800     STRING 'WDE101  (IDSHIPM  =' W-IDSHIPM-X ')'                         
208900          DELIMITED BY SIZE INTO SSA1                                     
209000     MOVE 'WDE111  ' TO SSA2                                              
209100     MOVE '  II' TO GODK-STATUSKODER                                      
209200     CALL CBLTDLI USING ISRT WDE1-PCB DLI-IO-WDE111 SSA1 SSA2             
209300     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
209400     PERFORM IMS-STATUSKONTROLL                                           
209500     .                                                                    
209600     EJECT                                                                
209700                                                                          
209800 IMS-ISRT-WDE121 SECTION.                                                 
209900                                                                          
210000     STRING 'WDE101  (IDSHIPM  =' W-IDSHIPM-X ')'                         
210100          DELIMITED BY SIZE INTO SSA1                                     
210200     STRING 'WDE111  (WDE111KY =' W-WDE111KY-X ')'                        
210300          DELIMITED BY SIZE INTO SSA2                                     
210400     MOVE 'WDE121  ' TO SSA3                                              
210500     MOVE '  II' TO GODK-STATUSKODER                                      
210600     CALL CBLTDLI USING ISRT WDE1-PCB DLI-IO-WDE121 SSA1 SSA2 SSA3        
210700     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
210800     PERFORM IMS-STATUSKONTROLL                                           
210900     .                                                                    
211000     EJECT                                                                
211100 IMS-GU-WDB601    SECTION.                                                
211200     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
211300          DELIMITED BY SIZE INTO SSA1                                     
211400     MOVE '    ' TO GODK-STATUSKODER                                      
211500     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
211600     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
211700     PERFORM IMS-STATUSKONTROLL                                           
211800     .                                                                    
211900 IMS-STATUSKONTROLL SECTION.                                              
212000     SET STATUS-IX TO 1                                                   
212100     SEARCH GODK-STATUS AT END CALL FELLOG                                
212200       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
212300     END-SEARCH                                                           
212400     CONTINUE                                                             
212500     .                                                                    
