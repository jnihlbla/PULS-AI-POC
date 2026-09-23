000100 ID DIVISION.                                                             
000201                                                                          
000301 PROGRAM-ID.     W4039000.                                                
000401 AUTHOR.         BO SVENSSON.                                             
000501 DATE-WRITTEN.   MARS 99.                                                 
000601 DATE-COMPILED.                                                           
000701                                                                          
000801*    FUNKTION.                                                            
000901*        W4039000. DIREKT-LEVERANTÖRS-PACKNING, DDGS.                     
001001*        PACK-RAPPORTERING AV ETT STYCK DIREKT-LEVERANS KOLLI OCH         
001101*        OM DET ÄR SISTA (ELLER DEN ENDA) RADEN/ERNA ÄVEN                 
001201*        AVSLUTNING AV PACK-RAPPORTERINGEN.                               
001301*                                                                         
001401*        DESSUTOM RAPPORTERAS UPPGIFTER OM KOLLIT.                        
001501*                                                                         
001601*        W4039000 ÄR KONSTRUERAT MED VALDA DELAR UR PROGRAM               
001701*        W40315 (PACKRAPPORTERING), W40397 (AVSLUT PACKNING) OCH          
001801*        W40663/65 (LASTNING) SAMT NYA REGLER FÖR USA-DIR.LEV.            
001901*                                                                         
002001*        LARM MAIL OM RADEN SAKNAS ELLER REDAN ÄR PACKAD                  
002101*        PÅ WDF4 VID UPPDATERING                                          
002201*                                                                         
002301*    INDATA.                                                              
002401*        TRANSAKTION: W4T390X                                             
002501*        MID:         W4I39001                                            
002601*                     WMSGKOM                                             
002701*                                                                         
002801*    UTDATA.                                                              
002901*        MOD:         WMSGKOM                                             
003001                                                                          
003101*                                                                         
003201 DATA DIVISION.                                                           
003301                                                                          
003401 WORKING-STORAGE SECTION.                                                 
003501*    -- CHECKED BY WY2000                                                 
003601                                                                          
003701 77    IDPGM                     PIC X(8)    VALUE 'W4039000'.            
003801                                                                          
003901 01    FELTEXT.                                                           
004001       03  FILLER                PIC X(8)    VALUE 'FELTEXT'.             
004101       03  FELTEXT-STR           PIC X(72)   VALUE SPACE.                 
004201 77    CURRENT-SECTION           PIC X(16)   VALUE SPACE.                 
004301 77    CURRENT-IMS-SECTION       PIC X(16)   VALUE SPACE.                 
004401 77    CURRENT-DAP-SECTION       PIC X(16)   VALUE SPACE.                 
004501 77    JA                        PIC X       VALUE 'J'.                   
004601 77    YES                       PIC X       VALUE 'Y'.                   
004701 77    NEJ                       PIC X       VALUE 'N'.                   
004801 77    KDRC-DISPLAY              PIC Z(5).                                
004901                                                                          
005001 77    WS-IDDC                   PIC X(2)    VALUE SPACE.                 
005101                                                                          
005201 77    RAETT                     PIC X       VALUE 'R'.                   
005301 77    FEL                       PIC X       VALUE 'F'.                   
005401 77    ETT                       PIC S9(9)   VALUE +1.                    
005501 77    FILLER                    PIC X(8)    VALUE 'AAAAAAAA'.            
005601 77    IDPSN-IX                  PIC S9(5)   VALUE +0   COMP-3.           
005701 77    TMS-IX                    PIC S9(9)   VALUE +0   COMP SYNC.        
005801 77    FG-INDX                   PIC S9(5)   VALUE +0   COMP-3.           
005901 77    FG-MAX-INDX               PIC S9(5)   VALUE +10  COMP-3.           
006001 77    IN-RAD-INDX               PIC S9(5)   VALUE +0.                    
006101 77    MAX-MOD-LAENGD            PIC S9(4)   VALUE +366 COMP SYNC.        
006201 77    MIN-MOD-LAENGD            PIC S9(4)   VALUE +82  COMP SYNC.        
006301 77    INX-TOT-ANT-RADER         PIC S9(3)   VALUE ZERO  COMP-3.          
006401 77    SENASTE-IDRADNR           PIC  9(4)   VALUE ZERO.                  
006501 77    AKTUELLT-IDRADNR          PIC  9(4)   VALUE ZERO.                  
006601 77    FILLER                    PIC X(8)    VALUE 'BBBBBBBB'.            
006701 77    WS-ANT-KLARA-RADER        PIC S9(3)   VALUE +0    COMP-3.          
006801 77    WS-VORD-KVKOLLI-FAKT      PIC S9(5)   VALUE +0    COMP-3.          
006901 77    WS-VORD-KVKOLLI-LAST      PIC S9(5)   VALUE +0    COMP-3.          
007001 77    WS-VORD-IDLEVNR           PIC  X(5)   VALUE SPACE.                 
007101 77    WS-VORD-KDVIA             PIC X(2).                                
007201 77    WS-VORD-FLAUTFAK          PIC X(1).                                
007301 77    WS-VORD-KDFRAKT           PIC 9(3).                                
007401 77    WS-KORD-KDORDKL           PIC S9(1)   VALUE +0    COMP-3.          
007501 77    WS-KORD-IDORDER           PIC S9(7)  COMP-3.                       
007601 77    WS-KDMFSFOR               PIC 9(1)   VALUE ZERO.                   
007701 77    WS-DAGENS-DATUM           PIC 9(6)   VALUE ZERO.                   
007801 77    WS-HHMMSSDD               PIC 9(8)   VALUE ZERO.                   
007901 01     WS-HHMMSSDD-RED.                                                  
008001   03   WS-HHMMSS               PIC  9(6).                                
008101   03   WS-DD                   PIC  9(2).                                
008201 77    FILLER                    PIC X(8)    VALUE 'CCCCCCCC'.            
008301 77    WS-IDANSTNR               PIC X(5)   VALUE SPACE.                  
008401 77    WS-IDDISTR                PIC 9(5)   VALUE ZERO.                   
008501 77    WS-IDKUNDNR               PIC X(6)   VALUE SPACE.                  
008601 77    WS-IDKUNDNR-NUM           PIC 9(6)   VALUE ZERO.                   
008701 77    WS-IDKOLLI-NUM            PIC 9(5)   VALUE ZERO.                   
008801 77    WS-IDPRODNR               PIC 9(7)   VALUE ZERO.                   
008901 77    WS-KOLLI-VLORDBTO         PIC 9(4)V9(3)  VALUE ZERO.               
009001 77    WS-KOLLI-VKORDBTO         PIC 9(6)V9(1)  VALUE ZERO.               
009101 77    FILLER                    PIC X(8)    VALUE 'DDDDDDDD'.            
009201 77    WS-KDEMBTYP               PIC 9(1)   VALUE ZERO.                   
009301 77    WS-DIKOLLIL               PIC 9(4)   VALUE ZERO.                   
009401 77    WS-DIKOLLIB               PIC 9(3)   VALUE ZERO.                   
009501 77    WS-DIKOLLIH               PIC 9(3)   VALUE ZERO.                   
009601 77    FILLER                    PIC X(8)    VALUE 'EEEEEEEE'.            
009701 77    MAX-RAD-ANTAL             PIC S9(3)  VALUE +74  COMP-3.            
009801*                                        ANTAL FÄRDIGPACKADE RADEW        
009901*                                        I ETT RAD-INTERVALL.             
010001 77    WS-DARFS                  PIC 9(12)   VALUE ZERO.                  
010101 77    WS-KDORDSTA               PIC X(2)    VALUE SPACE.                 
010201 77    WS-IDSKEPPN               PIC 9(7)    VALUE  0.                    
010301 77    WS-IDSUPREF               PIC X(10)   VALUE SPACE.                 
010401 77    WS-DASUPREF               PIC X(8)    VALUE SPACE.                 
010501 77    WS-TISUPTID               PIC 9(5)    VALUE ZERO.                  
010601 77    WS-IDLBBET                PIC X(12)   VALUE 'DIRECT      '.        
010701                                                                          
010801 77    WS-IDORDNR5               PIC X(05)   VALUE SPACE.                 
010901 77    WS-KDFAKTYP               PIC X(01)   VALUE SPACE.                 
011001 77    WS-MID-KVLEVART           PIC S9(7)   VALUE +0.                    
011101 77    WS-PRFRAKT                PIC S9(7)V9(2) VALUE ZERO COMP-3.        
011201 77    WS-IDKUNDNR-WDE2          PIC 9(7)       VALUE ZERO COMP-3.        
011301 77    KDRC-DISP                 PIC 9(4)       VALUE ZERO.               
011401 77    WS-IDCOM                  PIC S9(9)      VALUE ZERO COMP-3.        
011501 77    WS-TIUTSKR-WDF4           PIC  9(8)      VALUE ZERO.               
011601 77    WS-TIUTSTID-WDF4          PIC S9(7)      VALUE ZERO COMP-3.        
011701 77  W-IDLANDX2                  PIC X(2)   VALUE SPACE.                  
011801 77  ERROR-TEXT                  PIC X(80)  VALUE SPACE.                  
011901 77  WS-TIORDREG-NUM6            PIC 9(06)  VALUE ZERO.                   
012001 77  WS-ODEL-DATRPAVD            PIC 9(8)   VALUE ZERO.                   
012101 77  WS-ODEL-TIHHMM              PIC 9(04)  VALUE ZERO.                   
012201 77  WS-ODEL-DARFS               PIC 9(12)  VALUE ZERO.                   
012301                                                                          
012401 77  RKOD-ABEND                  PIC S9(4)  VALUE +33   COMP SYNC.        
012501 77  IX                          PIC S9(9)  VALUE ZERO  COMP SYNC.        
012601                                                                          
012701 77  WS-FLYG-TRP                 PIC S9(3)  VALUE +99.                    
012801 77  WS-FLYG-FRAKT               PIC S9(3)  VALUE +17.                    
012901 77  WS-AIR                      PIC X(3)   VALUE 'AAA'.                  
013001 77  WS-LHS                      PIC X(3)   VALUE 'LHS'.                  
013101                                                                          
013201*                                                                         
013301 01  WS-ADFLGEO-HIT.                                                      
013401     03 WS-IDDEPOT-HIT           PIC X(2).                                
013501     03 WS-IDROUTE-HIT           PIC X(1).                                
013601                                                                          
013701 01  WS-DCUSER.                                                           
013801     03 FILLER                   PIC X(5)   VALUE 'WIDDC'.                
013901     03 WS-DCUSER-IDDC           PIC X(2)   VALUE SPACE.                  
014001     03 FILLER                   PIC X(1)   VALUE SPACE.                  
014101                                                                          
014201 01    WS-TID-W.                                                          
014301   03  WS-TTMMSS                 PIC 9(6).                                
014401   03  WS-HH                     PIC 9(2).                                
014501 77    FILLER                    PIC X(8)    VALUE 'GGGGGGGG'.            
014601*                                                                         
014701 77  FARLIGT-GODS            PIC X            VALUE 'N'.                  
014801       88 FARLIGT-GODS-FINNS                  VALUE 'J'.                  
014901*                                                                         
015001 77  WS-OPEN-WDM2-FINNS      PIC X            VALUE 'N'.                  
015101       88 OPEN-WDM2-FINNS                     VALUE 'J'.                  
015201                                                                          
015301 77  SW-MAIL-SKICKAT              PIC X(1)    VALUE 'N'.                  
015401     88  MAIL-SKICKAT                         VALUE 'J'.                  
015501     88  FIRST-MAIL-RAD                       VALUE 'N'.                  
015601                                                                          
015701*                                                                         
015801 77    FILLER                    PIC X(8)    VALUE 'TRANS'.               
015901 77    WS-IDTRANS                PIC X(04).                               
016001   88  WS-GODKAND-TRANS                     VALUE '4390'                  
016101                                                  '0622'                  
016201                                                  '0693'.                 
016301*                                                                         
016401 77    WS-INDATA-TEST            PIC X(01).                               
016501   88  WS-INDATA-RATT                       VALUE 'R'.                    
016601*                                                                         
016701 01     FILLER                  PIC X(10)   VALUE 'SPAR-KOLLI'.           
016801 01     SPAR-KOLLI-AREA.                                                  
016901   03   SPAR-KOLLI-SUORDV       PIC  9(9)V9(2)    VALUE ZERO.             
017001   03   SPAR-KOLLI-SUORDV-LOC   PIC  9(9)V9(2)    VALUE ZERO.             
017101   03   SPAR-KOLLI-SUORDV-LOCPREL PIC  9(9)V9(2)    VALUE ZERO.           
017201   03   SPAR-KOLLI-VKORDBTO     PIC  9(6)V9(1)    VALUE ZERO.             
017301   03   SPAR-KOLLI-VLORDBTO     PIC  9(4)V9(3)    VALUE ZERO.             
017401   03   SPAR-KOLLI-KVLEVART     PIC  9(7)         VALUE ZERO.             
017501   03   SPAR-KOLLI-KDVALISO     PIC  X(3)         VALUE SPACE.            
017601*                                                                         
017701 01     FILLER                  PIC X(10)   VALUE 'ACC-AREA '.            
017801 01     ACC-AREA.                                                         
017901   03   ACC-AREA-GRP.                                                     
018001     05 ACC-ORAD-VKARTNTO       PIC  9(4)V9(3)    VALUE ZERO.             
018101     05 ACC-ORAD-VLARTNTO       PIC  9(6)V9(1)    VALUE ZERO.             
018201     05 ACC-KOLLI-VKORDNTO      PIC  9(4)V9(3)    VALUE ZERO.             
018301     05 ACC-KOLLI-KVFLAMP       PIC  S9(2)V9(1)   VALUE ZERO.             
018401     05 ACC-KOLLI-KDFARLIG      PIC  S9           VALUE ZERO.             
018501     05 ACC-KOLLI-KVORDRAD      PIC  S9(5)        VALUE ZERO.             
018601     05 ACC-KOLLI-KVFALRAD      PIC  S9(5)        VALUE ZERO.             
018701     05 ACC-KOLLI-SUORDV        PIC  S9(9)V9(2)   VALUE ZERO.             
018801     05 ACC-KOLLI-SUORDV-LOC    PIC  S9(9)V9(2)   VALUE ZERO.             
018901     05 ACC-KOLLI-SUORDV-LOCPREL PIC  S9(9)V9(2)   VALUE ZERO.            
019001     05 ACC-KOLLI-KDVALISO      PIC  X(3)         VALUE SPACE.            
019101                                                                          
019201*                                                                         
019301 01     FILLER                  PIC X(10)   VALUE 'WS-ORAD-'.             
019401 01     WS-ORAD.                                                          
019501   03   WS-ORAD-UPPG-AREA.                                                
019601     05 WS-ORAD-KVFLAMP         PIC  S9(2)V9(1)   VALUE ZERO.             
019701     05 WS-ORAD-KDFARLIG        PIC  S9           VALUE ZERO.             
019801     05 WS-ORAD-KVLEVART        PIC  S9(7)        VALUE ZERO.             
019901*                                                                         
020001   03   WS-ORAD-AREA.                                                     
020101     05 WS-ORAD-PRARTNTO        PIC  S9(9)V9(2)   VALUE ZERO.             
020201     05 WS-ORAD-PRARTNTO-LOC    PIC  S9(9)V9(2)   VALUE ZERO.             
020301     05 WS-ORAD-PRARTNTO-LOCPREL PIC  S9(9)V9(2)   VALUE ZERO.            
020401     05 WS-ORAD-VKARTNTO        PIC  S9(4)V9(3)   VALUE ZERO.             
020501     05 WS-ORAD-VLARTNTO        PIC  S9(6)V9(1)   VALUE ZERO.             
020601     05 WS-ORAD-KDVALISO        PIC  X(3)         VALUE SPACE.            
020701*                                                                         
020801   03   SPAR-FARLIGT-GODS-DATA.                                           
020901     05 WS-ORAD-IDPSN           PIC  9(3)                VALUE 0.         
021001     05 WS-ORAD-VKART-FG        PIC  S9(7)        COMP-3 VALUE 0.         
021101     05 WS-ORAD-VLFG            PIC  S9(4)V9(3)   COMP-3 VALUE 0.         
021201     05 WS-ORAD-SUEQFG          PIC  S9(3)V9(4)   COMP-3 VALUE 0.         
021301     05 TOTAL-SUEQFG            PIC  S9(3)V9(4)   COMP-3 VALUE 0.         
021401*                                                                         
021501 01  FILLER                      PIC X(16)  VALUE '4322-arb-AREA'.        
021601* ARBETSAREA XXJK-WDGX4322                                                
021701 01  -COPY WDGX4322    -PRE XXJK-                                         
021801*                                                                         
021901                                                                          
022001 01     DYNAMISKA-SUBPROGRAM.                                             
022101     03 CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.             
022201     03 FELLOG                  PIC X(8)    VALUE 'FELLOG  '.             
022301     03 ABEND                   PIC X(8)    VALUE 'ABEND   '.             
022401     03 WDATKONV                PIC X(8)    VALUE 'WDATKONV'.             
022501     03 W005INIT                PIC X(8)    VALUE 'W005INIT'.             
022601     03 WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.             
022701     03 WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.             
022801     03 W476SHNO                PIC X(8)    VALUE 'W476SHNO'.             
022901     03 W403TMS1                PIC X(8)    VALUE 'W403TMS1'.             
023001                                                                          
023101 01    ABENDKODER.                                                        
023201     03 FILLER                  PIC X(16) VALUE 'ABENDKODER'.             
023301     03 RKOD-ABEND-UTAN-DUMP    PIC S9(4) COMP SYNC VALUE +16.            
023401     03 RKOD-ABEND-MED-DUMP     PIC S9(4) COMP SYNC VALUE +33.            
023501     03 RKOD-FELTEXT            PIC X(32) VALUE SPACE.                    
023601                                                                          
023701*SVARSKODER TILL DISPATCHER, BILD 0622.                                   
023801 01  MESSAGE-CODES.                                                       
023901     03  ERR-ORDER-FINNS         PIC X(3)    VALUE '703'.                 
024001     03  ERR-FORMELLT-FEL        PIC X(3)    VALUE '094'.                 
024101     03  ERR-LOGISKT-FEL         PIC X(3)    VALUE '095'.                 
024201     03  ERR-SAKNAS-KREG         PIC X(3)    VALUE '063'.                 
024301     03  ERR-TRANSPORT-FEL       PIC X(3)    VALUE '087'.                 
024401     03  ERR-KUND-SPAERRAD       PIC X(3)    VALUE '213'.                 
024501     03  ERR-MOMS-REGNR-FEL      PIC X(3)    VALUE '223'.                 
024601     03  OK-BEHANDLAD            PIC X(3)    VALUE '101'.                 
024701     03  ERR-ORDERRAD-SAKN       PIC X(3)    VALUE '029'.                 
024801     03  ERR-ORDER-SAKNAS        PIC X(3)    VALUE '054'.                 
024901     03  ERR-ORDERHUVUD-SAKNAS   PIC X(3)    VALUE '417'.                 
025001     03  ERR-OVER-LEVERANS       PIC X(3)    VALUE '197'.                 
025101     03  ERR-FELAKTIGA-RADER     PIC X(3)    VALUE '751'.                 
025201     03  ERR-KOLLI-REDAN-RAPP    PIC X(3)    VALUE '721'.                 
025301     03  ERR-KOLLI-FAKTURERAT    PIC X(3)    VALUE '737'.                 
025401     03  ERR-VIKT-SAKNAS         PIC X(3)    VALUE '792'.                 
025501                                                                          
025601*                                                                         
025701*    --- AREOR TILL GEMENSAMMA SUBPROGRAM                                 
025801                                                                          
025901 01  FILLER                     PIC X(10)   VALUE 'WDECAREA'.             
026001*01 -COPY WDECAREA                                                        
026101*                                                                         
026201 01  FILLER                     PIC X(16)   VALUE 'WMSGINIT '.            
026301*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
026401*01 -COPY WMSGINIT                                                        
026501*                                                                         
026601 01  FILLER                     PIC X(16)   VALUE 'WZ01SEND '.            
026701*    --- PARAMETRAR TILL SUBPROGRAM WZ01SEND                              
026801*                                                                         
026901 01  HDR-AREA.                                                            
027001*    03  -COPY WZ01REQU                                                   
027101*    03  -COPY WZ04HDR                                                    
027201                                                                          
027301 01  FILLER                      PIC X(16)   VALUE 'SEND-AREA'.           
027401 01  SEND-AREA.                                                           
027501*    03  -COPY WZ01SEND                                                   
027601*                                                                         
027701 01  FILLER                      PIC X(16)   VALUE 'W476SHNO'.            
027801*    --- PARAMETRAR TILL SUBPROGRAM W476SHNO                              
027901*01  -COPY W476SHNO                                                       
028001 01  FILLER                     PIC X(16)   VALUE 'LÄNKAREOR'.            
028101*                                                                         
028201*TMS PACKNING INFO                                                        
028301 01  FILLER                      PIC X(8)   VALUE  'W403TMS1'.            
028401*    -COPY W403TMS1                                                       
028501     EJECT                                                                
028601*                                                                         
028701 01     FILLER                  PIC X(11)   VALUE 'HJALP-AREOR'.          
028801 01     HJALP-ODEL-TIRFS        PIC 9(11).                                
028901 01     FILLER                  REDEFINES HJALP-ODEL-TIRFS.               
029001   03   HJALP-ODEL-TIRFS-7      PIC  X(7).                                
029101   03   FILLER                  PIC  X(4).                                
029201                                                                          
029301 01     HJALP-4472-TIRFS        PIC 9(11).                                
029401 01     FILLER                  REDEFINES HJALP-4472-TIRFS.               
029501   03   HJALP-4472-TIRFS-7      PIC  X(7).                                
029601   03   FILLER                  PIC  X(4).                                
029701                                                                          
029801                                                                          
029901 01     TEST-IDDISTR            PIC 9(5)              COMP-3.             
030001 01     FILLER REDEFINES TEST-IDDISTR.                                    
030101*  03   -COPY WWDIST03.                                                   
030201                                                                          
030301 01     FILLER REDEFINES TEST-IDDISTR.                                    
030401*  03   -COPY WWDIST07.                                                   
030501                                                                          
030601 01     FILLER REDEFINES TEST-IDDISTR.                                    
030701*  03   -COPY WWDIST13.                                                   
030801                                                                          
030901 01     FILLER REDEFINES TEST-IDDISTR.                                    
031001*  03   -COPY WWDIST21.                                                   
031101                                                                          
031201 01     FILLER REDEFINES TEST-IDDISTR.                                    
031301*  03   -COPY WWDIST35.                                                   
031401                                                                          
031501 01     FILLER REDEFINES TEST-IDDISTR.                                    
031601*  03   -COPY WWDIST47.                                                   
031701                                                                          
031801 01  FILLER    REDEFINES TEST-IDDISTR.                                    
031901*  03   -COPY WWDIST79.                                                   
032001                                                                          
032101 01  FILLER    REDEFINES TEST-IDDISTR.                                    
032201*  03   -COPY WWDIST83.                                                   
032301                                                                          
032401 01  FILLER    REDEFINES TEST-IDDISTR.                                    
032501*  03   -COPY WWDIST85.                                                   
032601                                                                          
032701 01  FILLER    REDEFINES TEST-IDDISTR.                                    
032801*  03   -COPY WWDIST87.                                                   
032901*    ----DISTR-DEALER-PRICE----                                           
033001                                                                          
033101*    ---------AREA FÖR SOP-RUTINERS START---                              
033201 01  FILLER                      PIC X(16)   VALUE 'WMSGSOP '.            
033301 01  PROG-TO-PROG-SW.                                                     
033401*    03  -COPY WMSGSOP                                                    
033501*                                                                         
033601 01  WS-BC-PARAMETRAR.                                                    
033701     03  WS-BC.                                                           
033801         05  BC-URV-IDSHIPM      PIC 9(7)    VALUE ZERO.                  
033901                                                                          
034001***************************************************************           
034101*                                                                         
034201*01  WDATAREA      -COPY WDATAREA.                                        
034301                                                                          
034401***************************************************************           
034501 01    NYCKLAR-TILL-DLI.                                                  
034601*                                                                         
034701*  03    -COPY WDGX01                                                     
034801*                                                                         
034901   03    W-WDE4A1-KUNDORDER-X.                                            
035001     05    W-4A1-IDDISTR         PIC S9(5)   VALUE ZERO  COMP-3.          
035101     05    W-4A1-IDKUNDNR        PIC S9(7)   VALUE ZERO  COMP-3.          
035201     05    W-4A1-IDKUNDRF.                                                
035301       07  W-4A1-IDORDNR         PIC  9(5)   VALUE ZERO.                  
035401       07  FILLER                PIC X(05)   VALUE SPACE.                 
035501*                                                                         
035601   03    W-WDE401-KUNDORDER-X.                                            
035701     05    W-401-IDDISTR         PIC S9(5)   VALUE ZERO  COMP-3.          
035801     05    W-401-IDKUNDNR        PIC S9(7)   VALUE ZERO  COMP-3.          
035901     05    W-401-IDKUNDRF.                                                
036001       07  W-401-IDORDNR         PIC  9(5)   VALUE ZERO.                  
036101       07  FILLER                PIC X(05)   VALUE SPACE.                 
036201     05    W-401-IDPRODNR        PIC S9(7)   VALUE ZERO  COMP-3.          
036301     05    W-401-IDPLKLST        PIC S9(3)   VALUE ZERO  COMP-3.          
036401*                                                                         
036501   03    W-WDE4B-KEYSEQ-X.                                                
036601     05    W-411-IDPRODNR        PIC S9(7)   VALUE ZERO  COMP-3.          
036701     05    W-411-IDPURAD         PIC S9(5)   VALUE ZERO  COMP-3.          
036801*                                                                         
036901   03    W-WDE4ASEQ-X.                                                    
037001    05     W-E4ASEQ-IDGMTREF         PIC X(17)  VALUE SPACE.              
037101                                                                          
037201   03    W-WDE411-IDPURAD-X.                                              
037301     05    W-411-IDPURAD2        PIC S9(5)   VALUE ZERO  COMP-3.          
037401*                                                                         
037501   03    W-WDE421-IDKOLLI-X.                                              
037601     05    W-421-IDPRODNR        PIC S9(7)   VALUE ZERO  COMP-3.          
037701     05    W-421-IDKOLLI         PIC S9(5)   VALUE ZERO  COMP-3.          
037801*                                                                         
037901   03    W-WDE601-IDPRODNR-X.                                             
038001     05    W-601-IDPRODNR        PIC S9(7)   VALUE ZERO  COMP-3.          
038101*                                                                         
038201   03    W-WDE611-IDKOLLI-X.                                              
038301     05    W-611-IDKOLLI         PIC S9(5)   VALUE ZERO  COMP-3.          
038401*                                                                         
038501     03  W-IDFAKLOP-X.                                                    
038601         05  W-IDFAKLOP           PIC S9(3)   VALUE ZERO  COMP-3.         
038701*                                                                         
038801     03  W-IDSKEPPN-X.                                                    
038901         05 W-IDSKEPPN            PIC S9(7)   COMP-3 VALUE ZERO.          
039001*                                                                         
039101   03    W-WDQ201-X.                                                      
039201     05    W-201-IDORDER         PIC S9(7) COMP-3.                        
039301*                                                                         
039401   03    W-WDQ211-KEY-X.                                                  
039501     05    W-WDQ211-IDDC         PIC X(2).                                
039601     05    W-WDQ211-IDLEVNR      PIC  X(5)   VALUE SPACE.                 
039701*                                                                         
039801   03    W-WDQ301-KEY-X.                                                  
039901     05    W-WDQ301-IDORDER      PIC S9(7)   VALUE ZERO  COMP-3.          
040001     05    W-WDQ301-IDDC         PIC X(2).                                
040101     05    W-WDQ301-IDPRODNR     PIC S9(7)   VALUE ZERO  COMP-3.          
040201     05    W-WDQ301-IDPLKLST     PIC S9(3)   VALUE ZERO  COMP-3.          
040301*                                                                         
040401   03  W-Q301-KEY-MIN-X.                                                  
040501         05  W-Q301-MIN-IDORDER  PIC S9(7)   COMP-3.                      
040601         05  W-Q301-MIN-IDDC     PIC X(2).                                
040701         05  W-Q301-MIN-IDPRODNR PIC S9(7)   COMP-3.                      
040801         05  FILLER              PIC X(2)    VALUE LOW-VALUE.             
040901*                                                                         
041001   03  W-Q301-KEY-MAX-X.                                                  
041101         05  W-Q301-MAX-IDORDER  PIC S9(7)   COMP-3.                      
041201         05  W-Q301-MAX-IDDC     PIC X(2).                                
041301         05  W-Q301-MAX-IDPRODNR PIC S9(7)   COMP-3.                      
041401         05  FILLER              PIC X(2)    VALUE HIGH-VALUE.            
041501*                                                                         
041601   03  W-WDQ301KY-MIN.                                                    
041701         05  W-Q301KY-MIN-IDORDER    PIC S9(7)  COMP-3.                   
041801         05  W-Q301KY-MIN-IDDC       PIC X(2).                            
041901         05  FILLER                  PIC X(6)   VALUE LOW-VALUE.          
042001*                                                                         
042101   03  W-WDE4F1KY-MIN-X.                                                  
042201         05  W-IDPRODNR-MIN          PIC S9(7)  COMP-3.                   
042301         05  W-IDKOLLI-MIN           PIC S9(5)  COMP-3.                   
042401         05  FILLER                  PIC X(22)  VALUE LOW-VALUE.          
042501*                                                                         
042601   03  W-WDE4F1KY-MAX-X.                                                  
042701         05  W-IDPRODNR-MAX          PIC S9(7)  COMP-3.                   
042801         05  W-IDKOLLI-MAX           PIC S9(5)  COMP-3.                   
042901         05  FILLER                  PIC X(22)  VALUE HIGH-VALUE.         
043001*                                                                         
043101   03  W-IDGMT-X.                                                         
043201       05  W-IDDISTR-WDB2      PIC S9(5)   VALUE ZERO COMP-3.             
043301       05  W-IDKUNDNR-WDB2     PIC S9(7)   VALUE ZERO COMP-3.             
043401                                                                          
043501   03  W-KDODELST                PIC X.                                   
043601*                                                                         
043701   03    W-IDARTNR-X.                                                     
043801     05    W-IDARTNR             PIC S9(9)   VALUE ZERO  COMP-3.          
043901*                                                                         
044001   03    W-KDSEGKEY-X.                                                    
044101     05    W-KDSEGKEY            PIC X(1)    VALUE '1'.                   
044201*                                                                         
044301     03  W-WDQ301KY-MAX.                                                  
044401         05  W-Q301KY-MAX-IDORDER    PIC S9(7)  COMP-3.                   
044501         05  W-Q301KY-MAX-IDDC       PIC X(2).                            
044601         05  FILLER                  PIC X(6)   VALUE HIGH-VALUE.         
044701*                                                                         
044801   03    W-4447-X.                                                        
044901     05    FILLER                PIC X(4)  VALUE '4447'.                  
045001     05    W-4447-IDDC           PIC X(2).                                
045101     05    FILLER                PIC X(24) VALUE LOW-VALUE.               
045201*                                                                         
045301   03    W-4448-X.                                                        
045401     05    W-4448-IDPRC          PIC X(4).                                
045501     05    FILLER                PIC X(1)  VALUE LOW-VALUE.               
045601*                                                                         
045701     03  W-4744                   PIC X(4)    VALUE '4744'.               
045801     03  W-LOW-VALUE              PIC X(26)   VALUE LOW-VALUE.            
045901*                                                                         
046001   03    W-4321-IDHTYP-X.                                                 
046101         05  W-4321-IDHTYP         PIC X(4)  VALUE '4321'.                
046201         05  W-4321-NYCKEL-VALFRI  PIC X(26) VALUE LOW-VALUE.             
046301                                                                          
046401   03    W-IDSHIPM-X.                                                     
046501         05  W-IDSHIPM           PIC  9(7)   VALUE ZERO.                  
046601                                                                          
046701   03    W-WDE211KY-X.                                                    
046801         05  W-WDE211-IDDISTR    PIC S9(5)    VALUE ZERO COMP-3.          
046901         05  W-WDE211-IDKUNDNR   PIC S9(7)    VALUE ZERO COMP-3.          
047001                                                                          
047101   03    W-WDE111KY-X.                                                    
047201         05  W-WDE111-IDDISTR    PIC S9(5)    VALUE ZERO COMP-3.          
047301         05  W-WDE111-IDKUNDNR   PIC S9(7)    VALUE ZERO COMP-3.          
047401                                                                          
047501   03  W-IDDC-B6-X.                                                       
047601       05 W-IDDC-B6              PIC X(2).                                
047701                                                                          
047801   03  W-IDLEVNR-F4-X.                                                    
047901       05 W-IDLEVNR-F4           PIC X(5)     VALUE SPACE.                
048001                                                                          
048101   03 W-WDF411KY-MIN-X.                                                   
048201     05 W-IDPRODNR-F4-MIN     PIC S9(7) COMP-3  VALUE ZERO.               
048301     05 W-IDPURAD-F4-MIN      PIC S9(5) COMP-3  VALUE ZERO.               
048401     05 W-TIUTSKR-F4-MIN      PIC S9(7) COMP-3  VALUE ZERO.               
048501   03 W-WDF411KY-MAX-X.                                                   
048601     05 W-IDPRODNR-F4-MAX     PIC S9(7) COMP-3  VALUE ZERO.               
048701     05 W-IDPURAD-F4-MAX      PIC S9(5) COMP-3  VALUE ZERO.               
048801     05 W-TIUTSKR-F4-MAX      PIC S9(7) COMP-3  VALUE 9999999.            
048901                                                                          
049001   03  W-4503-WDGXKEY-X.                                                  
049101     05 W-4503-IDHTYP           PIC X(4)      VALUE '4503'.               
049201     05 W-4503-NYCKEL-VALFRI    PIC X(26)     VALUE LOW-VALUE.            
049301                                                                          
049401   03  W-4504-WDGXKEY-X.                                                  
049501     05 W-4504-IDPRODNR         PIC S9(7)   VALUE ZERO COMP-3.            
049601     05 W-4504-IDKOLLI          PIC S9(5)   VALUE ZERO COMP-3.            
049701*                                                                         
049801 01    FILLER                   PIC X(16) VALUE 'FG-TABELL'.              
049901                                                                          
050001 01    FG-TABELL.                                                         
050101   03    TAB-POST OCCURS 10.                                              
050201     05  TAB-IDPSN            PIC 9(3)              VALUE ZERO.           
050301     05  TAB-VKART-FG         PIC S9(7)      COMP-3 VALUE ZERO.           
050401     05  TAB-VLFG             PIC S9(4)V9(3) COMP-3 VALUE ZERO.           
050501                                                                          
050601******************************************************************        
050701*                                                                *        
050801*                AREOR FÖR MFS OCH SKÄRMHANTERING                *        
050901*                                                                *        
051001******************************************************************        
051101*                                                                         
051201*    --- AREOR FÖR MSG-IO                                                 
051301 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
051401*01  -COPY W4I39001.                                                      
051501 01  FILLER                      PIC X(16)  VALUE 'MSG-AREA '.            
051601                                                                          
051701*01  -COPY WMSGAREA.                                                      
051801*                                                                         
051901*01  -COPY W40636I1   -PRE MOD4636-                                       
052001                                                                          
052101 01  FILLER                      PIC X(16)   VALUE 'KOM-IO-AREA'.         
052201 01  KOM-IO-AREA.                                                         
052301*03  -COPY WMSGKOM                                                        
052401                                                                          
052501*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
052601*                                                                         
052701 01    IMS-WS.                                                            
052801   03    FILLER                  PIC X(16)   VALUE 'IMS-WS     '.         
052901                                                                          
053001*                        **** STATUS-KOD FRÅN IMS                         
053101   03    STATUS-WS               PIC XX.                                  
053201     88    SEGMENT-FINNS                     VALUE '  '.                  
053301     88    SEGMENT-SLUT                      VALUE 'GB'.                  
053401     88    SEGMENT-SAKNAS                    VALUE 'GE'.                  
053501     88    SEGMENT-FINNS-REDAN               VALUE 'II'.                  
053601                                                                          
053701   03    GODK-STATUSKODER.                                                
053801     05    GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.              
053901                                                                          
054001 01  ALL-SSA.                                                             
054101     03  SSA1                    PIC X(128).                              
054201     03  SSA2                    PIC X(160).                              
054301     03  SSA3                    PIC X(128).                              
054401     03  SSA4                    PIC X(128).                              
054501                                                                          
054601*                            IMS FUNKTIONSKODER                           
054701*01    -COPY W0003                                                        
054801                                                                          
054901 01    FILLER    PIC X(25)  VALUE 'DLI INPUT-OUTPUT AREA1'.               
055001 01  FILLER                      PIC X(16)  VALUE 'WDE401-AR'.            
055101 01  -COPY WDE401                                                         
055201                                                                          
055301 01  FILLER                      PIC X(16)  VALUE 'WDE411-AR'.            
055401 01  -COPY WDE411                                                         
055501                                                                          
055601 01  FILLER                      PIC X(16)  VALUE 'WDE421-AR'.            
055701 01  -COPY WDE421                                                         
055801                                                                          
055901 01  FILLER                      PIC X(16)  VALUE 'WDE4F1-AR'.            
056001 01  -COPY WDE4F1                                                         
056101                                                                          
056201 01  FILLER                      PIC X(16)  VALUE 'WDE601-AR'.            
056301 01  -COPY WDE601                                                         
056401                                                                          
056501 01  FILLER                      PIC X(16)  VALUE 'WDE611-AR'.            
056601 01  -COPY WDE611                                                         
056701                                                                          
056801 01  FILLER                      PIC X(16)  VALUE '4512-AREA'.            
056901 01  -COPY WDGX4512                                                       
057001                                                                          
057101 01  FILLER                      PIC X(16)  VALUE 'WDQ3-AREA'.            
057201 01  -COPY WDQ301                                                         
057301                                                                          
057401 01  FILLER                      PIC X(16)  VALUE 'WDQ2-AREA'.            
057501 01  -COPY WDQ201                                                         
057601                                                                          
057701 01  FILLER                      PIC X(16)  VALUE 'WDQ211-AREA'.          
057801 01  -COPY WDQ211                                                         
057901                                                                          
058001 01  FILLER                      PIC X(16)  VALUE '4448-AREA'.            
058101 01  -COPY WDGX4448                                                       
058201                                                                          
058301 01  FILLER                      PIC X(16)  VALUE '4474-AREA'.            
058401 01  -COPY WDGX4474                                                       
058501                                                                          
058601 01  FILLER                      PIC X(16)   VALUE '4322-AREA'.           
058701 01  -COPY WDGX01      -PRE 4321-                                         
058801 01  -COPY WDGX4322                                                       
058901                                                                          
059001 01  FILLER               PIC X(16)   VALUE 'DLI-IO-WDB201'.              
059101 01  DLI-IO-WDB201.                                                       
059201*      03  -COPY WDB201                                                   
059301 01  FILLER               PIC X(16)   VALUE 'DLI-IO-WDE201'.              
059401 01  DLI-IO-WDE201.                                                       
059501*      03  -COPY WDE201                                                   
059601                                                                          
059701 01  FILLER               PIC X(16)   VALUE 'DLI-IO-WDE211'.              
059801 01  DLI-IO-WDE211.                                                       
059901*      03  -COPY WDE211                                                   
060001                                                                          
060101 01  FILLER               PIC X(16)   VALUE 'DLI-IO-WDE221'.              
060201 01  DLI-IO-WDE221.                                                       
060301*      03  -COPY WDE221                                                   
060401                                                                          
060501 01  FILLER               PIC X(16)   VALUE 'DLI-IO-WDE101'.              
060601 01  DLI-IO-WDE101.                                                       
060701*      03  -COPY WDE101                                                   
060801                                                                          
060901 01  FILLER               PIC X(16)   VALUE 'DLI-IO-WDE111'.              
061001 01  DLI-IO-WDE111.                                                       
061101*      03  -COPY WDE111                                                   
061201                                                                          
061301 01  FILLER               PIC X(16)   VALUE 'DLI-IO-WDE121'.              
061401 01  DLI-IO-WDE121.                                                       
061501*      03  -COPY WDE121                                                   
061601                                                                          
061701 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
061801 01   DLI-IO-AREA-B601.                                                   
061901*     03  -COPY WDB601                                                    
062001                                                                          
062101 01  FILLER               PIC X(16)   VALUE 'DLI-IO-F411'.                
062201 01  DLI-IO-F411.                                                         
062301*    03  -COPY WDF411                                                     
062401                                                                          
062501 01  FILLER               PIC X(16)  VALUE 'DLI-IO-4504'.                 
062601 01  DLI-IO-4504.                                                         
062701*    03  -COPY WDGX4504                                                   
062801                                                                          
062901 01  FILLER               PIC X(16)   VALUE 'DLI-IO-WDK601'.              
063001 01  DLI-IO-WDK601.                                                       
063101*    03  -COPY WDK601                                                     
063201     EJECT                                                                
063301 01  FILLER               PIC X(16)   VALUE 'DLI-IO-WDK611'.              
063401 01  DLI-IO-WDK611.                                                       
063501*    03  -COPY WDK611                                                     
063601                                                                          
063701     EJECT                                                                
063801 01  SEND-RAD                    PIC X(120)  VALUE SPACE.                 
063901                                                                          
064001 01  FILLER                      PIC X(16)   VALUE 'MAILAREA'.            
064101                                                                          
064201*    --- LISTLAYOUT                                                       
064301 01  F4-LARM.                                                             
064401     03  F4-RUBRIK1.                                                      
064501         05  FILLER              PIC X(26)   VALUE                        
064601            'VOLVO CAR CORP., WDF4 LARM'.                                 
064701         05  FILLER              PIC X(2)    VALUE SPACE.                 
064801         05  F4R1-DATUM          PIC 9(6).                                
064901                                                                          
065001     03  F4-RUBRIK2.                                                      
065101         05  FILLER              PIC X(31)   VALUE                        
065201            'FÖLJANDE RAD(ER) SAKNAS PÅ WDF4'.                            
065301                                                                          
065401     03  F4-KOMMENTAR-RAD.                                                
065501         05 F4-IDPGM             PIC X(8)    VALUE SPACE.                 
065601         05 FILLER               PIC X(2)    VALUE ': '.                  
065701         05 F4-KOMMENTAR         PIC X(25)   VALUE SPACE.                 
065801                                                                          
065901     03  F4-RAD.                                                          
066001         05  FILLER              PIC X(6)    VALUE 'LEVNR '.              
066101         05  FILLER              PIC X(1)    VALUE SPACE.                 
066201         05  F4-RAD-IDLEVNR      PIC X(5).                                
066301         05  FILLER              PIC X(9)    VALUE '  PRODNR '.           
066401         05  FILLER              PIC X(1)    VALUE SPACE.                 
066501         05  F4-RAD-IDPRODNR     PIC 9(7).                                
066601         05  FILLER              PIC X(6)    VALUE '  RAD '.              
066701         05  FILLER              PIC X(1)    VALUE SPACE.                 
066801         05  F4-RAD-IDPURAD      PIC 9(6).                                
066901         05  FILLER              PIC X(7)    VALUE '  UTSKR'.             
067001         05  FILLER              PIC X(1)    VALUE SPACE.                 
067101         05  F4-RAD-TIUTSKR      PIC 9(6).                                
067201                                                                          
067301 LINKAGE SECTION.                                                         
067401*                                                                         
067501*01    -COPY W0009     -PRE MSG-                                          
067601                                                                          
067701*01    -COPY W0009     -PRE ALT-                                          
067801                                                                          
067901*01    -COPY W0009     -PRE DISTRDOC-                                     
068001                                                                          
068101*01    -COPY W0009     -PRE AD36-                                         
068201                                                                          
068301*01    -COPY W0009     -PRE DISP-                                         
068401     EJECT                                                                
068501 01  TMS-CRE-PCB                 PIC X.                                   
068601 01  TMS-DEL-PCB                 PIC X.                                   
068701 01  ATAB-PCB                    PIC X.                                   
068801     EJECT                                                                
068901                                                                          
069001*01    -COPY W0008     -PRE USEA-                                         
069101     05  FILLER                  PIC X.                                   
069201                                                                          
069301*01    -COPY W0008     -PRE WDE4-                                         
069401     05  FILLER                  PIC X.                                   
069501                                                                          
069601*01    -COPY W0008     -PRE WDE41-                                        
069701     05  FILLER                  PIC X.                                   
069801                                                                          
069901*01    -COPY W0008     -PRE WDE4F-                                        
070001     05  FILLER                  PIC X.                                   
070101                                                                          
070201*01    -COPY W0008     -PRE ORQA-                                         
070301     05  FILLER                  PIC X.                                   
070401                                                                          
070501*01    -COPY W0008     -PRE ORQI-                                         
070601     05  FILLER                  PIC X.                                   
070701                                                                          
070801*01    -COPY W0008     -PRE WDE6-                                         
070901     05  FILLER                  PIC X.                                   
071001                                                                          
071101*01    -COPY W0008     -PRE XXKH-                                         
071201     05  FILLER                  PIC X.                                   
071301                                                                          
071401*01    -COPY W0008     -PRE XXJK-                                         
071501     05  FILLER                  PIC X.                                   
071601*01    -COPY W0008     -PRE WDB2-                                         
071701     05  FILLER                  PIC X.                                   
071801*01    -COPY W0008     -PRE WDE2-                                         
071901     05  FILLER                  PIC X.                                   
072001*01    -COPY W0008     -PRE WDE1-                                         
072101     05  FILLER                  PIC X.                                   
072201*01    -COPY W0008     -PRE 4517-                                         
072301     05  FILLER                  PIC X.                                   
072401                                                                          
072501*01    -COPY W0008     -PRE WDB6-                                         
072601     05  FILLER                  PIC X.                                   
072701                                                                          
072801*01    -COPY W0008     -PRE WDF4-                                         
072901     05  FILLER                  PIC X.                                   
073001                                                                          
073101*01    -COPY W0008     -PRE 4503-                                         
073201     05  FILLER                  PIC X.                                   
073301                                                                          
073401     05  FILLER                  PIC X.                                   
073501*01  -COPY W0008       -PRE WDK6-                                         
073601     05  FILLER                  PIC X.                                   
073701                                                                          
073801 01  TMS-1165-PCB                PIC X.                                   
073901 01  TMS-4141-PCB                PIC X.                                   
074001 01  TMS-WDB2-PCB                PIC X.                                   
074101 01  TMS-WDB6-PCB                PIC X.                                   
074201 01  TMS-WDD3-PCB                PIC X.                                   
074301 01  TMS-WDB1-PCB                PIC X.                                   
074401 01  TMS-WDE4A-PCB               PIC X.                                   
074501 01  TMS-WDE4F-PCB               PIC X.                                   
074601 01  TMS-WDQ2-PCB                PIC X.                                   
074701 01  TMS-WDQ3-PCB                PIC X.                                   
074801 01  TMS-WDK6-PCB                PIC X.                                   
074901 01  TMS-WDE6-PCB                PIC X.                                   
075001 01  TMS-WDK5-PCB                PIC X.                                   
075101 01  TMS-WDQ2C-PCB               PIC X.                                   
075201                                                                          
075301  PROCEDURE DIVISION USING MSG-PCB  ALT-PCB   DISTRDOC-PCB                
075401                           AD36-PCB DISP-PCB                              
075501                           TMS-CRE-PCB TMS-DEL-PCB                        
075601                           ATAB-PCB USEA-PCB                              
075701                           WDE4-PCB WDE41-PCB WDE4F-PCB                   
075801                           ORQA-PCB ORQI-PCB  WDE6-PcB                    
075901                           XXKH-PCB WDB2-PCB  XXJK-PCB                    
076001                           WDE2-PCB WDE1-PCB  4517-pcb                    
076101                           WDB6-PCB WDF4-PCB  4503-PCB                    
076201                           WDK6-PCB                                       
076301                           TMS-1165-PCB TMS-4141-PCB TMS-WDB2-PCB         
076401                           TMS-WDB6-PCB TMS-WDD3-PCB TMS-WDB1-PCB         
076501                           TMS-WDE4A-PCB TMS-WDE4F-PCB                    
076601                           TMS-WDQ2-PCB TMS-WDQ3-PCB TMS-WDK6-PCB         
076701                           TMS-WDE6-PCB TMS-WDK5-PCB                      
076801                           TMS-WDQ2C-PCB.                                 
076901     PERFORM IMS-GU-MSG-AREA                                              
077001     IF SEGMENT-FINNS                                                     
077101       PERFORM IMS-GN-KOM-AREA                                            
077201       PERFORM A-INIT                                                     
077301       PERFORM B-KONTROLL-INDATA                                          
077401       IF WS-INDATA-RATT AND WS-GODKAND-TRANS                             
077501                                                                          
077601          PERFORM C-LAGG-UPP-KOLLI-SEG                                    
077701          PERFORM D-BEHANDLA-RADER                                        
077801          PERFORM F-UPPDATERA-KOLLIREG                                    
077901          PERFORM J-UPDATE-E401-Q301-CALL-TMS                             
078001                                                                          
078101          IF  WS-VORD-KDVIA NOT = '01'                                    
078201          AND MID-FLSLUT = JA                                             
078301             PERFORM H-UPDATE-LAST-OCH-FAKT-REG                           
078401             PERFORM K-CHECK-EDI                                          
078501          END-IF                                                          
078601          IF  WS-VORD-KDVIA  = '01'                                       
078701          AND MID-FLSLUT = JA                                             
078801             PERFORM L-CHECK-EDI-EXCEPTION                                
078901          END-IF                                                          
079001       END-IF                                                             
079101     END-IF                                                               
079201     PERFORM Z-DISPATCH-AVSLUT                                            
079301                                                                          
079401     MOVE ZERO TO RETURN-CODE                                             
079501     GOBACK                                                               
079601     .                                                                    
079701                                                                          
079801                                                                          
079901 A-INIT             SECTION.                                              
080001     MOVE 'STA A-SEC'                     TO CURRENT-SECTION              
080101                                                                          
080201     IF MSG-DUBBLA-TRANSKODER                                             
080301       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO   MID-W4I39001               
080401       MOVE MSG-IDTRANS-2                 TO   WS-IDTRANS                 
080501       MOVE MSG-KDMFSFOR-2                TO   WS-KDMFSFOR                
080601     ELSE                                                                 
080701       MOVE MSG-INDATA-MINUS-1-TRANSKOD   TO   MID-W4I39001               
080801       MOVE MSG-IDTRANS-1                 TO   WS-IDTRANS                 
080901       MOVE MSG-KDMFSFOR-1                TO   WS-KDMFSFOR                
081001     END-IF                                                               
081101*                                                                         
081201     PERFORM S21-INIT-WS-FIELDS                                           
081301     PERFORM AA-FLYTTA-INDATA-MID                                         
081401                                                                          
081501     ACCEPT WS-DAGENS-DATUM               FROM DATE                       
081601     ACCEPT WS-HHMMSSDD                   FROM TIME                       
081701     ACCEPT WS-TID-W                      FROM TIME                       
081801                                                                          
081901     PERFORM S08-HAMTA-MASKINDATUM                                        
082001                                                                          
082101     IF DCS-NDC-NA                                                        
082201       MOVE ALL '+'           TO MSGI-WMSGINIT                            
082301       MOVE '011'             TO MSGI-KDCALL                              
082401       MOVE WS-DCUSER         TO MSGI-IDUSER                              
082501       MOVE WS-DAGENS-DATUM   TO MSGI-TILOKDAT                            
082601       MOVE WS-HHMMSSDD(1:4)  TO MSGI-TILOKTID                            
082701       CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                         
082801       MOVE MSGI-TILOKDAT     TO WS-DAGENS-DATUM                          
082901                                 DAT-I-TIDATUM                            
083001       MOVE MSGI-TILOKTID     TO WS-HHMMSSDD (1:4)                        
083101                                 WS-TTMMSS   (1:4)                        
083201                                                                          
083301       MOVE 'AAMMDD'          TO DAT-KDDATFORM                            
083401       CALL WDATKONV USING DAT-KDDATFORM                                  
083501                           DAT-I-TIDATUM                                  
083601                           DAT-O-TIDATUM                                  
083701                           DAT-KDSVAR                                     
083801     END-IF                                                               
083901                                                                          
084001     MOVE '1'                          TO W-KDSEGKEY                      
084101                                                                          
084201     MOVE SPACE                        TO MSG-KOM-IDMFSMED                
084301     .                                                                    
084401                                                                          
084501 AA-FLYTTA-INDATA-MID    SECTION.                                         
084601     MOVE 'AA-FLYTTA-INDATA'              TO CURRENT-SECTION              
084701                                                                          
084801     MOVE MID-IDANSTNR                 TO WS-IDANSTNR                     
084901     MOVE MID-IDDC                     TO WS-IDDC                         
085001                                          WS-DCUSER-IDDC                  
085101     MOVE MID-IDPRODNR                 TO WS-IDPRODNR                     
085201                                          W-401-IDPRODNR                  
085301     MOVE MID-IDDISTR                  TO W-401-IDDISTR                   
085401                                          WS-IDDISTR                      
085501     MOVE MID-IDKUNDNR                 TO WS-IDKUNDNR-NUM                 
085601                                          W-401-IDKUNDNR                  
085701                                          WS-IDKUNDNR                     
085801     MOVE MID-IDORDNR                  TO WS-IDORDNR5                     
085901                                          W-401-IDORDNR                   
086001     MOVE MID-IDSUPREF                 TO WS-IDSUPREF                     
086101     MOVE MID-IDKOLLI                  TO WS-IDKOLLI-NUM                  
086201     MOVE MID-KDEMBTYP                 TO WS-KDEMBTYP                     
086301     MOVE MID-DIKOLLIL                 TO WS-DIKOLLIL                     
086401     MOVE MID-DIKOLLIB                 TO WS-DIKOLLIB                     
086501     MOVE MID-DIKOLLIH                 TO WS-DIKOLLIH                     
086601     MOVE MID-DASUPREF                 TO WS-DASUPREF                     
086701     MOVE MID-TISUPTID                 TO WS-TISUPTID                     
086801                                                                          
086901     COMPUTE WS-TISUPTID = WS-TISUPTID / 10                               
087001     END-COMPUTE                                                          
087101                                                                          
087201     MOVE MID-VKORDBTO-KOLLI         TO DEC-IDFRIDATA                     
087301     MOVE 6                          TO DEC-KVHELTAL                      
087401     MOVE 1                          TO DEC-KVDECIMAL                     
087501     CALL WDECEDIT USING DEC-WDECAREA                                     
087601     IF   DEC-KDSVAR-OK                                                   
087701         MOVE DEC-IDEDITDATA      TO WS-KOLLI-VKORDBTO                    
087801     ELSE                                                                 
087901*   RAD FR.MID-AREA SAKNAS I WDE411'                                      
088001         MOVE ERR-VIKT-SAKNAS      TO MSG-KOM-IDMFSMED                    
088101         MOVE '4'                  TO MSG-KOM-KDSVAR                      
088201         MOVE FEL                  TO WS-INDATA-TEST                      
088301     END-IF                                                               
088401                                                                          
088501*    IF MID-VLORDBTO-KOLLI > ZERO                                         
088601     IF MID-VLORDBTO-KOLLI > '0000.000'                                   
088701       MOVE MID-VLORDBTO-KOLLI       TO DEC-IDFRIDATA                     
088801       MOVE 4                        TO DEC-KVHELTAL                      
088901       MOVE 3                        TO DEC-KVDECIMAL                     
089001       CALL WDECEDIT USING DEC-WDECAREA                                   
089101       IF DEC-KDSVAR-OK                                                   
089201           MOVE DEC-IDEDITDATA    TO WS-KOLLI-VLORDBTO                    
089301       ELSE                                                               
089401*     RAD FR.MID-AREA SAKNAS I WDE411'                                    
089501           MOVE ERR-VIKT-SAKNAS    TO MSG-KOM-IDMFSMED                    
089601           MOVE '4'                TO MSG-KOM-KDSVAR                      
089701           MOVE FEL                TO WS-INDATA-TEST                      
089801       END-IF                                                             
089901     ELSE                                                                 
090001       MOVE ZERO                   TO WS-KOLLI-VLORDBTO                   
090101     END-IF                                                               
090201                                                                          
090301     MOVE MID-IDDC TO W-IDDC-B6                                           
090401     PERFORM IMS-GU-WDB601                                                
090501     .                                                                    
090601                                                                          
090701                                                                          
090801 B-KONTROLL-INDATA      SECTION.                                          
090901     MOVE 'STA B-SEC'                     TO CURRENT-SECTION              
091001     MOVE RAETT                        TO WS-INDATA-TEST                  
091101     MOVE +1 TO IN-RAD-INDX                                               
091201     MOVE +0 TO SENASTE-IDRADNR                                           
091301     PERFORM UNTIL IN-RAD-INDX > MAX-RAD-ANTAL OR                         
091401                   MID-IDRADNR (IN-RAD-INDX) = ALL '+'                    
091501                                                                          
091601         PERFORM BA-KONTROLL-AV-RAD                                       
091701         ADD +1                TO IN-RAD-INDX                             
091801     END-PERFORM                                                          
091901       MOVE 'SLUT B-SEC'                    TO CURRENT-SECTION            
092001     .                                                                    
092101                                                                          
092201                                                                          
092301 BA-KONTROLL-AV-RAD       SECTION.                                        
092401       MOVE 'STA BA-SEC '      TO CURRENT-SECTION                         
092501     MOVE WS-IDDISTR           TO W-401-IDDISTR                           
092601     MOVE WS-IDKUNDNR          TO W-401-IDKUNDNR                          
092701     MOVE WS-IDORDNR5          TO W-401-IDORDNR                           
092801     MOVE WS-IDPRODNR          TO W-401-IDPRODNR                          
092901     MOVE +1                   TO W-401-IDPLKLST                          
093001                                                                          
093101     PERFORM IMS-GU-WDE401                                                
093201                                                                          
093301     PERFORM UNTIL SEGMENT-FINNS                                          
093401          OR W-401-IDPLKLST > 15                                          
093501       ADD +1                  TO W-401-IDPLKLST                          
093601       PERFORM IMS-GU-WDE401                                              
093701     END-PERFORM                                                          
093801                                                                          
093901     IF SEGMENT-FINNS                                                     
094001       MOVE KORD-IDPLKLST        TO W-401-IDPLKLST                        
094101       MOVE KORD-IDORDER         TO W-201-IDORDER                         
094201       MOVE KORD-KDORDKL         TO WS-KORD-KDORDKL                       
094301       MOVE KORD-IDORDER         TO WS-KORD-IDORDER                       
094401                                                                          
094501       PERFORM IMS-GU-ORQI01                                              
094601       IF SEGMENT-FINNS                                                   
094701         MOVE MID-IDRADNR(IN-RAD-INDX) TO W-411-IDPURAD2                  
094801         PERFORM IMS-GNP-WDE411                                           
094901         IF SEGMENT-FINNS                                                 
095001                                                                          
095101            PERFORM BAA-KONTROLLERA-RADEN                                 
095201         ELSE                                                             
095301*     RAD FR.MID-AREA SAKNAS I WDE411'                                    
095401           MOVE ERR-ORDERRAD-SAKN TO MSG-KOM-IDMFSMED                     
095501           MOVE '4'              TO MSG-KOM-KDSVAR                        
095601           MOVE FEL              TO WS-INDATA-TEST                        
095701         END-IF                                                           
095801       ELSE                                                               
095901*        ORDERHUVUD RENSAD? '                                             
096001         MOVE ERR-ORDERHUVUD-SAKNAS   TO MSG-KOM-IDMFSMED                 
096101         MOVE '4'                     TO MSG-KOM-KDSVAR                   
096201         MOVE FEL                     TO WS-INDATA-TEST                   
096301       END-IF                                                             
096401     ELSE                                                                 
096501*   ORDER FR.MID-AREA SAKNAS I WDE401'                                    
096601       MOVE ERR-ORDER-SAKNAS     TO MSG-KOM-IDMFSMED                      
096701       MOVE '4'                  TO MSG-KOM-KDSVAR                        
096801       MOVE FEL                  TO WS-INDATA-TEST                        
096901     END-IF                                                               
097001       MOVE 'SLUT BA-SEC'                    TO CURRENT-SECTION           
097101     .                                                                    
097201                                                                          
097301                                                                          
097401 BAA-KONTROLLERA-RADEN SECTION.                                           
097501     MOVE 'BAA-KOLLA-RAD   '              TO CURRENT-SECTION              
097601                                                                          
097701     MOVE MID-KVLEVART(IN-RAD-INDX) TO WS-MID-KVLEVART                    
097801                                                                          
097901     IF (WS-MID-KVLEVART + ORAD-KVLEVART) > ORAD-KVAVBART                 
098001*   ÖVER LEVERANS!'                                                       
098101       MOVE ERR-OVER-LEVERANS    TO MSG-KOM-IDMFSMED                      
098201       MOVE '4'                  TO MSG-KOM-KDSVAR                        
098301       MOVE FEL                  TO WS-INDATA-TEST                        
098401     END-IF                                                               
098501                                                                          
098601     MOVE MID-IDRADNR(IN-RAD-INDX) TO AKTUELLT-IDRADNR                    
098701     IF AKTUELLT-IDRADNR > SENASTE-IDRADNR                                
098801       MOVE AKTUELLT-IDRADNR     TO SENASTE-IDRADNR                       
098901     ELSE                                                                 
099001         MOVE ERR-FELAKTIGA-RADER  TO MSG-KOM-IDMFSMED                    
099101         MOVE '4'                  TO MSG-KOM-KDSVAR                      
099201         MOVE FEL                  TO WS-INDATA-TEST                      
099301     END-IF                                                               
099401     .                                                                    
099501                                                                          
099601                                                                          
099701 C-LAGG-UPP-KOLLI-SEG   SECTION.                                          
099801       MOVE 'STA C-SEC  '                   TO CURRENT-SECTION            
099901     MOVE WS-IDPRODNR            TO W-601-IDPRODNR                        
100001     MOVE WS-IDKOLLI-NUM         TO W-421-IDKOLLI                         
100101                                    W-611-IDKOLLI                         
100201     PERFORM IMS-GHU-WDE601                                               
100301                                                                          
100401     MOVE VORD-IDLEVNR           TO WS-VORD-IDLEVNR                       
100501     MOVE VORD-KDVIA             TO WS-VORD-KDVIA                         
100601     MOVE VORD-KVKOLLI-FAKT      TO WS-VORD-KVKOLLI-FAKT                  
100701     MOVE VORD-KVKOLLI-LAST      TO WS-VORD-KVKOLLI-LAST                  
100801     MOVE VORD-FLAUTFAK          TO WS-VORD-FLAUTFAK                      
100901     MOVE VORD-KDFRAKT           TO WS-VORD-KDFRAKT                       
101001                                                                          
101101     PERFORM S09-INIT-KOLLI-VALUE                                         
101201                                                                          
101301     IF WS-INDATA-RATT AND WS-GODKAND-TRANS                               
101401       PERFORM IMS-ISRT-WDE611                                            
101501                                                                          
101601       IF SEGMENT-FINNS-REDAN                                             
101701         PERFORM IMS-GHU-WDE611                                           
101801         IF SEGMENT-SAKNAS                                                
101901         OR KOLLI-KDKOLSTA > 0                                            
102001           MOVE ERR-KOLLI-REDAN-RAPP TO MSG-KOM-IDMFSMED                  
102101           MOVE '4'                  TO MSG-KOM-KDSVAR                    
102201           MOVE FEL                  TO WS-INDATA-TEST                    
102301         END-IF                                                           
102401       END-IF                                                             
102501     END-IF                                                               
102601     MOVE 'SLUT C-SEC  '                  TO CURRENT-SECTION              
102701     .                                                                    
102801                                                                          
102901                                                                          
103001 D-BEHANDLA-RADER     SECTION.                                            
103101       MOVE 'STA D-SEC '                    TO CURRENT-SECTION            
103201     PERFORM IMS-GHU-WDE401                                               
103301                                                                          
103401     MOVE +1 TO IN-RAD-INDX                                               
103501     MOVE MID-IDRADNR (IN-RAD-INDX) TO W-411-IDPURAD2                     
103601     PERFORM IMS-GHNP-WDE411                                              
103701                                                                          
103801     PERFORM UNTIL IN-RAD-INDX > MAX-RAD-ANTAL OR                         
103901                   MID-IDRADNR (IN-RAD-INDX) = ALL '+'                    
104001                                                                          
104101        IF MID-KVLEVART(IN-RAD-INDX) > '0000000'                          
104201          PERFORM DAB-UPPDATERA-RAD                                       
104301          PERFORM DAC-LAGG-UPP-WDE421                                     
104401          PERFORM DAD-NOLLSTALL-SPAR-ORAD                                 
104501        END-IF                                                            
104601*KOLLA NÄSTA RAD.                                                         
104701        IF (IN-RAD-INDX + 0001) <=  MAX-RAD-ANTAL  AND                    
104801           MID-IDRADNR (IN-RAD-INDX + 0001) NOT = ALL '+'                 
104901          MOVE MID-IDRADNR (IN-RAD-INDX + 0001) TO W-411-IDPURAD2         
105001*IMS LÄSN.ÄR BEROENDE OM NÄSTA RAD ÄR MINDRE EL. STÖRRE ÄN FÖREG.         
105101          IF IN-RAD-INDX >  1 AND                                         
105201           ( MID-IDRADNR (IN-RAD-INDX) <                                  
105301             MID-IDRADNR (IN-RAD-INDX - 1) )                              
105401            PERFORM IMS-GHNP-WDE411-F                                     
105501          ELSE                                                            
105601            PERFORM IMS-GHNP-WDE411                                       
105701          END-IF                                                          
105801        END-IF                                                            
105901        ADD +1                      TO IN-RAD-INDX                        
106001     END-PERFORM                                                          
106101                                                                          
106201     IF MAIL-SKICKAT                                                      
106301        PERFORM S90-SEND-CLOSE                                            
106401     END-IF                                                               
106501     MOVE 'SLUT D-SEC '             TO CURRENT-SECTION                    
106601     .                                                                    
106701                                                                          
106801                                                                          
106901 DAB-UPPDATERA-RAD         SECTION.                                       
107001       MOVE 'STA DAB-SEC '          TO CURRENT-SECTION                    
107101     MOVE ORAD-VKARTNTO             TO WS-ORAD-VKARTNTO                   
107201     MOVE ORAD-VLARTNTO             TO WS-ORAD-VLARTNTO                   
107301     MOVE ORAD-KVFLAMP              TO WS-ORAD-KVFLAMP                    
107401     MOVE ORAD-KDFARLIG             TO WS-ORAD-KDFARLIG                   
107501     MOVE ORAD-PRARTNTO             TO WS-ORAD-PRARTNTO                   
107601     MOVE ORAD-PRARTNTO-LOC         TO WS-ORAD-PRARTNTO-LOC               
107701     MOVE ORAD-PRARTNTO-LOCPREL     TO WS-ORAD-PRARTNTO-LOCPREL           
107801     MOVE ORAD-KDVALISO             TO WS-ORAD-KDVALISO                   
107901*                                                                         
108001     MOVE ORAD-IDPSN                TO WS-ORAD-IDPSN                      
108101     MOVE ORAD-VKART-FG             TO WS-ORAD-VKART-FG                   
108201     MOVE ORAD-VLFG                 TO WS-ORAD-VLFG                       
108301     MOVE ORAD-SUEQFG               TO WS-ORAD-SUEQFG                     
108401*                                                                         
108501     MOVE MID-KVLEVART(IN-RAD-INDX) TO WS-MID-KVLEVART                    
108601     MOVE MID-KDARTURS(IN-RAD-INDX) TO ORAD-KDARTURS                      
108701     ADD  WS-MID-KVLEVART           TO ORAD-KVLEVART                      
108801                                       WS-ORAD-KVLEVART                   
108901     PERFORM S10-UPPD-SPAR-KOLLI                                          
109001                                                                          
109101     IF ORAD-KVLEVART = ORAD-KVAVBART                                     
109201        PERFORM DABA-RENSA-DIREKTLEVERANS                                 
109301        MOVE +4                     TO ORAD-KDRADSTA                      
109401        ADD 1                       TO WS-ANT-KLARA-RADER                 
109501     END-IF                                                               
109601                                                                          
109701     PERFORM IMS-REPL-BEHANDLAD-RAD                                       
109801       MOVE 'SLUT DAB-SEC '         TO CURRENT-SECTION                    
109901     .                                                                    
110001                                                                          
110101                                                                          
110201 DABA-RENSA-DIREKTLEVERANS SECTION.                                       
110301     MOVE 'DABA-RENSA-DIRL '      TO CURRENT-SECTION                      
110401                                                                          
110501     MOVE ORAD-IDLEVNR       TO W-IDLEVNR-F4                              
110601     MOVE ORAD-IDPRODNR      TO W-IDPRODNR-F4-MIN                         
110701                                W-IDPRODNR-F4-MAX                         
110801     MOVE ORAD-IDPURAD       TO W-IDPURAD-F4-MIN                          
110901                                W-IDPURAD-F4-MAX                          
111001*    OM VI HAR FÖRDRÖJDA DDGS-ORDER STÄMMER INTE                          
111101*    ORDERRADENS TIUTSKR(ORDEREG-DATUM)                                   
111201*    MED DATUM PÅ WDF4(DATUM NÄR ORDEN SKICKAS TILL LEV.)                 
111301*    MOVE ORAD-TIUTSKR       TO W-TIUTSKR-F4                              
111401                                                                          
111501     PERFORM IMS-GHU-WDF411                                               
111601     IF SEGMENT-FINNS                                                     
111701        IF DLOR-TIPACKN > 0                                               
111801          MOVE ZERO            TO WS-TIUTSKR-WDF4                         
111901                                  WS-TIUTSTID-WDF4                        
112001          MOVE 'SKEPPAD RAD'   TO F4-KOMMENTAR                            
112101          PERFORM S30-GENERERA-LARM-MAIL                                  
112201        ELSE                                                              
112301          COMPUTE WS-TIUTSKR-WDF4 = DLOR-TIUTSKR + 20000000               
112401          MOVE DLOR-TIUTSTID   TO WS-TIUTSTID-WDF4                        
112501          MOVE WS-DAGENS-DATUM TO  DLOR-TIPACKN                           
112601          PERFORM IMS-REPL-WDF411                                         
112701        END-IF                                                            
112801     ELSE                                                                 
112901        MOVE ZERO            TO WS-TIUTSKR-WDF4                           
113001                                WS-TIUTSTID-WDF4                          
113101        MOVE 'SKEPPAD RAD'   TO F4-KOMMENTAR                              
113201        PERFORM S30-GENERERA-LARM-MAIL                                    
113301     END-IF                                                               
113401     .                                                                    
113501                                                                          
113601                                                                          
113701 DAC-LAGG-UPP-WDE421       SECTION.                                       
113801       MOVE 'STA DAC-SEC '          TO CURRENT-SECTION                    
113901     MOVE WS-IDPRODNR               TO KKOLLI-IDPRODNR                    
114001     MOVE WS-IDKOLLI-NUM            TO KKOLLI-IDKOLLI                     
114101     MOVE WS-ORAD-KVLEVART          TO KKOLLI-KVLEVART                    
114201     PERFORM IMS-ISRT-WDE421                                              
114301     IF SEGMENT-FINNS-REDAN                                               
114401         MOVE WS-IDPRODNR           TO W-421-IDPRODNR                     
114501         MOVE WS-IDKOLLI-NUM        TO W-421-IDKOLLI                      
114601         PERFORM IMS-GHNP-WDE421                                          
114701         ADD WS-ORAD-KVLEVART       TO KKOLLI-KVLEVART                    
114801         PERFORM IMS-REPL-WDE421                                          
114901     ELSE                                                                 
115001         ADD +1                     TO ACC-KOLLI-KVORDRAD                 
115101                                                                          
115201         IF WS-ORAD-KDFARLIG = +4                                         
115301         OR WS-ORAD-KDFARLIG = +7                                         
115401             ADD +1                 TO ACC-KOLLI-KVFALRAD                 
115501         END-IF                                                           
115601     END-IF                                                               
115701*                                                                         
115801     IF WS-ORAD-IDPSN > ZERO                                              
115901       PERFORM DACA-SPARA-FG-DATA                                         
116001     END-IF                                                               
116101       MOVE 'SLUT DAC-SEC '         TO CURRENT-SECTION                    
116201     .                                                                    
116301                                                                          
116401                                                                          
116501 DACA-SPARA-FG-DATA SECTION.                                              
116601       MOVE 'STA DACA-SEC  '        TO CURRENT-SECTION                    
116701     MOVE +1 TO FG-INDX                                                   
116801     PERFORM UNTIL FG-INDX > FG-MAX-INDX                                  
116901                                                                          
117001       IF TAB-IDPSN(FG-INDX) = ZERO                                       
117101         MOVE WS-ORAD-IDPSN         TO TAB-IDPSN(FG-INDX)                 
117201         PERFORM S17-BERAEKNA-FG-FAELT                                    
117301                                                                          
117401       ELSE                                                               
117501         IF WS-ORAD-IDPSN = TAB-IDPSN(FG-INDX)                            
117601           PERFORM S17-BERAEKNA-FG-FAELT                                  
117701         END-IF                                                           
117801       END-IF                                                             
117901                                                                          
118001       ADD +1 TO FG-INDX                                                  
118101     END-PERFORM                                                          
118201                                                                          
118301     COMPUTE TOTAL-SUEQFG = TOTAL-SUEQFG      +                           
118401                            (WS-ORAD-SUEQFG *                             
118501                             KKOLLI-KVLEVART)                             
118601     END-COMPUTE                                                          
118701       MOVE 'SLUT DACA-SEC  '       TO CURRENT-SECTION                    
118801     .                                                                    
118901                                                                          
119001                                                                          
119101 DAD-NOLLSTALL-SPAR-ORAD      SECTION.                                    
119201     MOVE 'DANOLLA SPAR-RAD'              TO CURRENT-SECTION              
119301                                                                          
119401     MOVE ZEROES              TO WS-ORAD-KVFLAMP                          
119501                                 WS-ORAD-KDFARLIG                         
119601                                 WS-ORAD-KVLEVART                         
119701                                 WS-ORAD-IDPSN                            
119801                                 WS-ORAD-VKART-FG                         
119901                                 WS-ORAD-VLFG                             
120001                                 WS-ORAD-SUEQFG                           
120101     .                                                                    
120201     SKIP2                                                                
120301                                                                          
120401                                                                          
120501 F-UPPDATERA-KOLLIREG      SECTION.                                       
120601       MOVE 'STA F-SEC  '                   TO CURRENT-SECTION            
120701     PERFORM IMS-GHU-WDE601                                               
120801                                                                          
120901     IF  MID-FLSLUT = JA                                                  
121001        IF VORD-KDVIA NOT = '01'                                          
121101           ADD +1             TO  VORD-KVKOLLI                            
121201        END-IF                                                            
121301                                                                          
121401        ADD +1                TO  VORD-KVKOLPAC                           
121501     END-IF                                                               
121601     MOVE WS-DAGENS-DATUM     TO  VORD-TIPACKN-SK                         
121701                                                                          
121801     COMPUTE VORD-KVORDRAD-PACK                                           
121901           = VORD-KVORDRAD-PACK + WS-ANT-KLARA-RADER                      
122001     END-COMPUTE                                                          
122101                                                                          
122201     IF  VORD-KVORDRAD = VORD-KVORDRAD-PACK                               
122301     AND VORD-KDVIA NOT = '01'                                            
122401        MOVE  3               TO  VORD-KDORDSTA                           
122501     END-IF                                                               
122601                                                                          
122701     IF VORD-KDORDSTA        =   1                                        
122801         MOVE 2               TO  VORD-KDORDSTA                           
122901     END-IF                                                               
123001                                                                          
123101     COMPUTE VORD-VLORDNTO    ROUNDED                                     
123201           = VORD-VLORDNTO + ACC-ORAD-VLARTNTO                            
123301     END-COMPUTE                                                          
123401                                                                          
123501     COMPUTE VORD-VKORDNTO    ROUNDED                                     
123601           = VORD-VKORDNTO + ACC-KOLLI-VKORDNTO                           
123701     END-COMPUTE                                                          
123801                                                                          
123901     IF  MID-FLSLUT = JA                                                  
124001       COMPUTE VORD-VKORDBTO    ROUNDED                                   
124101             = VORD-VKORDBTO + WS-KOLLI-VKORDBTO                          
124201       END-COMPUTE                                                        
124301                                                                          
124401       COMPUTE VORD-VLORDBTO    ROUNDED                                   
124501             = VORD-VLORDBTO + WS-KOLLI-VLORDBTO                          
124601       END-COMPUTE                                                        
124701                                                                          
124801       IF  MID-FLSLUT-VORD = JA                                           
124901       AND VORD-VKORDNTO > VORD-VKORDBTO                                  
125001         MOVE VORD-VKORDBTO     TO VORD-VKORDNTO                          
125101       END-IF                                                             
125201     END-IF                                                               
125301                                                                          
125401     COMPUTE VORD-SUORDV-PACK  ROUNDED                                    
125501           = VORD-SUORDV-PACK  + ACC-KOLLI-SUORDV                         
125601     END-COMPUTE                                                          
125701     COMPUTE VORD-SUORDV-PACK-LOC  ROUNDED                                
125801           = VORD-SUORDV-PACK-LOC  + ACC-KOLLI-SUORDV-LOC                 
125901     END-COMPUTE                                                          
126001     COMPUTE VORD-SUORDV-PACK-LOCPREL  ROUNDED                            
126101           = VORD-SUORDV-PACK-LOCPREL  + ACC-KOLLI-SUORDV-LOCPREL         
126201     END-COMPUTE                                                          
126301     MOVE ACC-KOLLI-KDVALISO      TO VORD-KDVALISO                        
126401*                                                                         
126501     MOVE VORD-DARFS              TO WS-DARFS                             
126601*                                                                         
126701     PERFORM IMS-REPL-WDE601                                              
126801                                                                          
126901     PERFORM FA-BEHANDLA-KOLLI                                            
127001       MOVE 'SLUT F-SEC '                   TO CURRENT-SECTION            
127101     .                                                                    
127201                                                                          
127301                                                                          
127401 FA-BEHANDLA-KOLLI      SECTION.                                          
127501       MOVE 'STA FA-SEC   '                  TO CURRENT-SECTION           
127601     MOVE WS-IDKOLLI-NUM          TO  W-611-IDKOLLI                       
127701     PERFORM IMS-GHU-WDE611                                               
127801*                                                                         
127901     PERFORM S11-UPPD-KOLLI-FRAN-ARB                                      
128001*                                                                         
128101     PERFORM S13-UPPDAT-KDORDSTA                                          
128201*                                                                         
128301     PERFORM S16-UPPD-FARLIGT-GODS-DATA                                   
128401*                                                                         
128501     PERFORM IMS-REPL-WDE611                                              
128601       MOVE 'SLUT FA-SEC '                   TO CURRENT-SECTION           
128701     .                                                                    
128801                                                                          
128901                                                                          
129001 J-UPDATE-E401-Q301-CALL-TMS     SECTION.                                 
129101     MOVE 'STA J-SEC'                     TO CURRENT-SECTION              
129201                                                                          
129301     PERFORM IMS-GHU-KUNDORDER                                            
129401                                                                          
129501     IF VORD-KVORDRAD = VORD-KVORDRAD-PACK                                
129601                                                                          
129701       MOVE ZERO                  TO KORD-KDPAKOLL                        
129801                                                                          
129901       COMPUTE KORD-KVORDRAD-PACK = KORD-KVORDRAD  +                      
130001                                    KORD-KVORDRAD-LEVPL                   
130101       END-COMPUTE                                                        
130201                                                                          
130301       PERFORM IMS-REPL-WDE401                                            
130401                                                                          
130501       MOVE KORD-IDDISTR          TO TEST-IDDISTR                         
130601                                                                          
130701       PERFORM JB-UPDATE-ORQA                                             
130801       PERFORM JD-UPDATE-ORQI                                             
130901     END-IF                                                               
131001*CALL TMS                                                                 
131101     MOVE WS-IDDC            TO TMS-IDDC                                  
131201     MOVE KORD-IDDISTR       TO TMS-IDDISTR                               
131301     MOVE KORD-IDKUNDNR      TO TMS-IDKUNDNR                              
131401     MOVE KORD-IDORDNR5      TO TMS-IDORDNR7                              
131501     MOVE KOLLI-IDKOLLI      TO TMS-IDKOLLI(1)                            
131601     MOVE WS-IDPRODNR        TO TMS-IDPRODNR                              
131701     CALL W403TMS1 USING TMS-W403TMS1                                     
131801           TMS-CRE-PCB TMS-DEL-PCB ATAB-PCB                               
131901           TMS-1165-PCB TMS-4141-PCB TMS-WDB2-PCB                         
132001           TMS-WDB6-PCB TMS-WDD3-PCB TMS-WDB1-PCB                         
132101           TMS-WDE4A-PCB TMS-WDE4F-PCB TMS-WDQ2-PCB                       
132201           TMS-WDQ3-PCB TMS-WDK6-PCB TMS-WDE6-PCB                         
132301           TMS-WDK5-PCB TMS-WDQ2C-PCB                                     
132401                                                                          
132501       MOVE 'SLUT J-SEC '                   TO CURRENT-SECTION            
132601     .                                                                    
132701                                                                          
132801                                                                          
132901 JB-UPDATE-ORQA                          SECTION.                         
133001     MOVE 'JB-UPDATE-ORQA  '              TO CURRENT-SECTION              
133101                                                                          
133201     MOVE KORD-IDORDER            TO W-WDQ301-IDORDER                     
133301     MOVE KORD-IDDC               TO W-WDQ301-IDDC                        
133401     MOVE KORD-IDPRODNR           TO W-WDQ301-IDPRODNR                    
133501     MOVE KORD-IDPLKLST           TO W-WDQ301-IDPLKLST                    
133601     PERFORM IMS-GHU-ORQA01                                               
133701                                                                          
133801     MOVE KORD-KVORDRAD-PACK      TO ODEL-KVPACKRAD-OD                    
133901     MOVE 'P'                     TO ODEL-KDODELSTA                       
134001     MOVE DAT-TIAAMMDD            TO ODEL-TIPACKN                         
134101     MOVE WS-TTMMSS               TO ODEL-TIPACTID                        
134201*                                                                         
134301     MOVE ODEL-IDDC               TO W-4447-IDDC                          
134401     MOVE ODEL-IDPRC              TO W-4448-IDPRC                         
134501*                                                                         
134601     MOVE ODEL-DATRPAVD           TO WS-ODEL-DATRPAVD                     
134701     MOVE ODEL-TIHHMM             TO WS-ODEL-TIHHMM                       
134801     MOVE ODEL-DARFS              TO WS-ODEL-DARFS                        
134901*                                                                         
135001*                                                                         
135101     IF WS-TIUTSKR-WDF4 NOT = ZERO                                        
135201        MOVE WS-TIUTSKR-WDF4        TO ODEL-DAUTSKR                       
135301        MOVE WS-TIUTSTID-WDF4       TO ODEL-TIUTSTID                      
135401     END-IF                                                               
135501     PERFORM IMS-REPL-ORQA01                                              
135601                                                                          
135701     .                                                                    
135801                                                                          
135901                                                                          
136001 JD-UPDATE-ORQI                          SECTION.                         
136101       MOVE 'STA JD-SEC '                    TO CURRENT-SECTION           
136201                                                                          
136301     MOVE    WS-KORD-IDORDER      TO W-201-IDORDER                        
136401     MOVE    WS-VORD-IDLEVNR      TO W-WDQ211-IDLEVNR                     
136501     MOVE    WS-IDDC              TO W-WDQ211-IDDC                        
136601     PERFORM IMS-GHU-ORQI11                                               
136701                                                                          
136801     IF WS-VORD-KVKOLLI-LAST > ZERO OR                                    
136901        WS-VORD-KVKOLLI-FAKT > ZERO                                       
137001         MOVE 'P*'                TO DIRL-KDORDSTA                        
137101     ELSE                                                                 
137201         MOVE 'P '                TO DIRL-KDORDSTA                        
137301     END-IF                                                               
137401                                                                          
137501     PERFORM IMS-REPL-ORQI11                                              
137601       MOVE 'SLUT JD-SEC '                   TO CURRENT-SECTION           
137701     .                                                                    
137801                                                                          
137901                                                                          
138001 H-UPDATE-LAST-OCH-FAKT-REG    SECTION.                                   
138101     MOVE 'H-UPD-FAKT-LAST '              TO CURRENT-SECTION              
138201                                                                          
138301*SOM INFO: SEKTION HA- ÄR HÄMTAD FRÅN PGM 4663 OCH ÖVRIGA                 
138401*SEKTIONER I H-  ÄR HÄMTADE FRÅN PGM 4665.                                
138501                                                                          
138601     PERFORM HA-UPPDATERA-WDE6                                            
138701                                                                          
138801     MOVE MID-IDDISTR      TO    DIST79-IDDISTR                           
138901     IF MID-FLSLUT = JA                                                   
139001       MOVE DCS-IDLANDX2 TO W-IDLANDX2                                    
139101       PERFORM S27-SHIPM-NUMBER                                           
139201       PERFORM HI-SKAPA-WDE2                                              
139301       PERFORM HJ-SKAPA-WDE1                                              
139401       PERFORM S23-OPEN-WZ01                                              
139501       PERFORM S24-SEND-WZ01                                              
139601       PERFORM S25-CLOSE-WZ01                                             
139701     END-IF                                                               
139801     .                                                                    
139901                                                                          
140001                                                                          
140101 HA-UPPDATERA-WDE6    SECTION.                                            
140201       MOVE 'STA HA-SEC '                    TO CURRENT-SECTION           
140301     MOVE WS-IDDISTR          TO TEST-IDDISTR                             
140401     MOVE WS-IDPRODNR         TO W-601-IDPRODNR                           
140501     MOVE WS-IDKOLLI-NUM      TO W-611-IDKOLLI                            
140601                                                                          
140701     PERFORM IMS-GHU-WDE611                                               
140801     MOVE NEJ                 TO KOLLI-FLUTLAST                           
140901     IF (DIST35-REFILL OR DIST35-NDCCN-CDC-REFILL)                        
141001         AND NOT DIST35-REFILL-NA                                         
141101                                                                          
141201        MOVE KOLLI-IDSUPREF   TO KOLLI-IDLBBET                            
141301     ELSE                                                                 
141401        MOVE WS-IDLBBET       TO KOLLI-IDLBBET                            
141501     END-IF                                                               
141601     MOVE +6                  TO KOLLI-KDKOLSTA                           
141701     MOVE ZERO                TO KOLLI-IDKOLLI-SAMP                       
141801                                                                          
141901*- - - - - - KOLLA OM KOLLIT INNEHÅLLER FARLIGT GODS                      
142001     MOVE +1         TO IDPSN-IX                                          
142101     PERFORM UNTIL IDPSN-IX > +10                                         
142201        IF KOLLI-IDPSN(IDPSN-IX) > +0                                     
142301           MOVE JA TO FARLIGT-GODS                                        
142401        END-IF                                                            
142501        ADD +1       TO IDPSN-IX                                          
142601     END-PERFORM                                                          
142701                                                                          
142801*-- TILASTID SÄTTS TILL 9:OR PGA. ATT 4698-BOLLA-PGM BEHÖVER              
142901*-- VETA VILKA KOLLIN I EN ORDER SOM REDAN BLEV UTLASTADE.                
143001*-- DESSA 9:OR ERSÄTTS SEDAN I BMP-FAKT MED RIKTIG TID.                   
143101*(GK)MOVE +9999999   TO KOLLI-TILASTID (rad 1165710 kan tas bort)         
143201     MOVE +9999999              TO KOLLI-TILASTID                         
143301                                                                          
143401     MOVE KOLLI-SUORDV-KOLLI    TO SPAR-KOLLI-SUORDV                      
143501     MOVE KOLLI-SUORDV-LOC      TO SPAR-KOLLI-SUORDV-LOC                  
143601     MOVE KOLLI-SUORDV-LOCPREL  TO SPAR-KOLLI-SUORDV-LOCPREL              
143701     MOVE KOLLI-VKORDBTO-KOLLI  TO SPAR-KOLLI-VKORDBTO                    
143801     MOVE KOLLI-VLORDBTO-KOLLI  TO SPAR-KOLLI-VLORDBTO                    
143901     MOVE KOLLI-KDVALISO        TO SPAR-KOLLI-KDVALISO                    
144001                                                                          
144101     PERFORM IMS-REPL-WDE611                                              
144201                                                                          
144301     MOVE ZERO                  TO SPAR-KOLLI-KVLEVART                    
144401     MOVE WS-IDPRODNR         TO W-IDPRODNR-MIN                           
144501                                 W-IDPRODNR-MAX                           
144601     MOVE WS-IDKOLLI-NUM      TO W-IDKOLLI-MIN                            
144701                                 W-IDKOLLI-MAX                            
144801     PERFORM IMS-GU-WDE4F1                                                
144901     PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                         
145001       ADD SEQF-KVLEVART       TO SPAR-KOLLI-KVLEVART                     
145101       PERFORM IMS-GN-WDE4F1                                              
145201     END-PERFORM                                                          
145301                                                                          
145401     PERFORM IMS-GHU-WDE601                                               
145501     MOVE VORD-KDFAKTYP           TO WS-KDFAKTYP                          
145601     ADD  +1                      TO VORD-KVKOLLI-FL                      
145701     ADD  SPAR-KOLLI-SUORDV       TO VORD-SUORDV-FL                       
145801     ADD  SPAR-KOLLI-SUORDV-LOC   TO VORD-SUORDV-FL-LOC                   
145901     ADD  SPAR-KOLLI-SUORDV-LOCPREL TO VORD-SUORDV-FL-LOCPREL             
146001     ADD  SPAR-KOLLI-VKORDBTO     TO VORD-VKORDBTO-FL                     
146101     ADD  SPAR-KOLLI-VLORDBTO     TO VORD-VLORDBTO-FL                     
146201     MOVE SPAR-KOLLI-KDVALISO     TO VORD-KDVALISO                        
146301     PERFORM IMS-REPL-WDE601                                              
146401                                                                          
146501       MOVE 'SLUT HA-SEC '                   TO CURRENT-SECTION           
146601     .                                                                    
146701                                                                          
146801                                                                          
146901 HI-SKAPA-WDE2 SECTION.                                                   
147001     MOVE 'HI-SKAPA-WDE2   '              TO CURRENT-SECTION              
147101                                                                          
147201     INITIALIZE BILL-WDE201                                               
147301     MOVE SHNO-IDSHIPM                    TO BILL-IDSHIPM                 
147401                                             W-IDSHIPM                    
147501     MOVE MID-IDDC                        TO BILL-IDDC                    
147601     MOVE W-IDLANDX2                      TO BILL-IDLANDX3-SEND           
147701     IF DCS-DDC                                                           
147801        MOVE WS-VORD-IDLEVNR              TO BILL-IDLEVNR                 
147901     ELSE                                                                 
148001        MOVE ZERO                         TO BILL-IDLEVNR                 
148101     END-IF                                                               
148201     MOVE WS-DAGENS-DATUM                 TO BILL-TISKEPPN                
148301     MOVE WS-TTMMSS                       TO BILL-TISKPTID                
148401     EVALUATE TRUE                                                        
148501       WHEN VORD-KDFAKTYP = 'R' OR 'G'                                    
148601         MOVE 'INV'                       TO BILL-KDFINDOC                
148701       WHEN VORD-KDFAKTYP = 'K' OR 'N'                                    
148801         MOVE 'INT'                       TO BILL-KDFINDOC                
148901     END-EVALUATE                                                         
149001     MOVE VORD-IDDC-EXP                   TO BILL-IDDC-EXP                
149101     PERFORM IMS-ISRT-WDE201                                              
149201                                                                          
149301     PERFORM HIA-RAKNA-PRFRAKT                                            
149401     INITIALIZE BGMT-WDE211                                               
149501     MOVE MID-IDDISTR           TO BGMT-IDDISTR                           
149601                                   W-WDE211-IDDISTR                       
149701     MOVE MID-IDKUNDNR          TO BGMT-IDKUNDNR                          
149801                                   W-WDE211-IDKUNDNR                      
149901     MOVE SPACE                 TO BGMT-IDPARTNR                          
150001     MOVE 'N'                   TO BGMT-FLCOD                             
150101     MOVE WS-PRFRAKT            TO BGMT-PRFRAKT                           
150201     MOVE ZERO                  TO BGMT-PRFOERS                           
150301     MOVE ZERO                  TO BGMT-REFOERS                           
150401     MOVE ZERO                  TO BGMT-REOVKOFF                          
150501     MOVE ZERO                  TO BGMT-KDLEVVIL                          
150601     MOVE ZERO                  TO BGMT-PRAVDRAG                          
150701     MOVE ZERO                  TO BGMT-PREMBHNT                          
150801     MOVE ZERO                  TO BGMT-PRLEGKST                          
150901     MOVE ZERO                  TO BGMT-REAVDRAG                          
151001     MOVE ZERO                  TO BGMT-REEMBHNT                          
151101     MOVE ZERO                  TO BGMT-RELEGKST                          
151201     MOVE SPACE                 TO BGMT-FLSEPINV                          
151301     PERFORM IMS-ISRT-WDE211                                              
151401                                                                          
151501     INITIALIZE  BKOLLI-WDE221                                            
151601     MOVE MID-IDPRODNR           TO BKOLLI-IDPRODNR                       
151701     MOVE MID-IDKOLLI            TO BKOLLI-IDKOLLI                        
151801     MOVE SPACE                  TO BGMT-IDPARTNR                         
151901     MOVE 'N'                    TO BGMT-FLCOD                            
152001     MOVE ZERO                   TO BKOLLI-IDDISTR                        
152101                                    BKOLLI-IDKUNDNR                       
152201     MOVE SPACE                  TO BKOLLI-IDKUNDRF                       
152301                                    BKOLLI-FLOVRLEV                       
152401                                    BKOLLI-KDFAKTYP                       
152501                                    BKOLLI-BEKUNDRF                       
152601     MOVE -1                     TO BKOLLI-KDORDKL                        
152701     MOVE SPACE                  TO BKOLLI-KDPRSTA                        
152801     MOVE ZERO                   TO BKOLLI-VKORDBTO-KOLLI                 
152901     MOVE SPACE                  TO BKOLLI-FLCROSS                        
153000     PERFORM IMS-ISRT-WDE221                                              
153100     .                                                                    
153200                                                                          
153300                                                                          
153400 HIA-RAKNA-PRFRAKT SECTION.                                               
153500     MOVE 'HIA-RAKNA-PRFRA '              TO CURRENT-SECTION              
153600                                                                          
153700     IF WS-VORD-KDVIA NOT = '01' AND                                      
153800        (WS-KORD-KDORDKL = 0 OR WS-KORD-KDORDKL = 1) AND                  
153900        (DIST35-REFILL-NA OR DIST07-USA-RETAILER                          
154000                          OR   DIST07-CAN-RETAILER)  AND                  
154100         (WS-VORD-IDLEVNR = '7500 ' OR                                    
154200          WS-VORD-IDLEVNR = '10121' OR                                    
154300          WS-VORD-IDLEVNR = '14562' OR                                    
154400          WS-VORD-IDLEVNR = 'BP7YA' OR                                    
154500          WS-VORD-IDLEVNR = 'BP3EA' OR                                    
154600          WS-VORD-IDLEVNR = 'BP8BA' OR                                    
154700          WS-VORD-IDLEVNR = 'AEF31')                                      
154800       COMPUTE WS-PRFRAKT = SPAR-KOLLI-KVLEVART * 1000                    
154900     ELSE                                                                 
155000       MOVE ZERO                TO  WS-PRFRAKT                            
155100     END-IF                                                               
155200     .                                                                    
155300                                                                          
155400                                                                          
155500 HJ-SKAPA-WDE1 SECTION.                                                   
155600     MOVE 'HJ-SKAPA-WDE1   '              TO CURRENT-SECTION              
155700                                                                          
155800     INITIALIZE SHIP-WDE101                                               
155900     MOVE SHNO-IDSHIPM          TO SHIP-IDSHIPM                           
156000                                   W-IDSHIPM                              
156100     MOVE +999                  TO SHIP-IDTRPTNR                          
156200     MOVE MID-IDDC              TO SHIP-IDDC                              
156300     MOVE W-IDLANDX2            TO SHIP-IDLANDX3-SEND                     
156400     MOVE WS-DAGENS-DATUM       TO SHIP-TISKEPPN                          
156500     MOVE WS-HHMMSS (1:6)       TO SHIP-TISKPTID                          
156600     MOVE 'N'                   TO SHIP-FLSKRIV-NU                        
156700     MOVE MID-IDDISTR           TO TEST-IDDISTR                           
156800                                                                          
156900     IF ((DCS-DDC AND DCS-GERMANY)                                        
157000                AND (WS-VORD-IDLEVNR = '6492 ' OR                         
157100                     WS-VORD-IDLEVNR = 'BZFFA'))                          
157200       OR  (DCS-DDC AND DCS-BELGIUM)                                      
157300       OR  (DCS-DDC AND DCS-SWEDEN)                                       
157400       OR  (DCS-DDC AND DCS-FRANCE)                                       
157500       OR  (DCS-DDC AND DCS-KOREA )                                       
157510       OR  (DCS-DDC AND DCS-ENGLAND)                                      
157600       OR  (DCS-DDC AND DCS-TURKEY)                                       
157701       OR  (DCS-DDC AND DCS-SOUTH-AFRICA)                                 
157701       OR  (DCS-DDC AND DCS-AUSTRALIA)                                    
157800*      OR  (DCS-DDC AND DCS-HUNGARY)                                      
157900*      OR  (DCS-DDC AND DCS-POLAND)                                       
158000*      OR  (DCS-DDC AND DCS-MAROCKO)                                      
158100       MOVE 'J'                 TO SHIP-FLSKRIV-NU                        
158200     END-IF                                                               
158300                                                                          
158400     MOVE 'N'                   TO SHIP-KDKLAR                            
158500     MOVE 'DDGS'                TO SHIP-IDLBBET                           
158600     EVALUATE TRUE                                                        
158700       WHEN VORD-KDFAKTYP = 'R' OR 'G'                                    
158800         MOVE 'INV'             TO SHIP-KDFINDOC                          
158900       WHEN VORD-KDFAKTYP = 'K' OR 'N'                                    
159000         MOVE 'INT'             TO SHIP-KDFINDOC                          
159100     END-EVALUATE                                                         
159200     MOVE VORD-IDDC-EXP         TO SHIP-IDDC-EXP                          
159300     MOVE '0'                   TO SHIP-KDFAKSTA-EXP                      
159400     MOVE ZERO                  TO SHIP-SUNTO-TOT                         
159500                                   SHIP-PRKURS-BET                        
159600     MOVE SPACE                 TO SHIP-IDSYSTEM                          
159700                                   SHIP-KDVALISO-BET                      
159800                                                                          
159900     MOVE NEJ                     TO SHIP-FLFARLIG                        
160000     MOVE SPACES                  TO SHIP-KDVALISO-EXP                    
160100     MOVE ZEROES                  TO SHIP-SUORDV-EXP                      
160200                                     SHIP-SUORDV-FAKT                     
160300                                     SHIP-VKORDBTO-FAKT                   
160400                                     SHIP-VLORDBTO-FAKT                   
160500                                                                          
160600     PERFORM IMS-ISRT-WDE101                                              
160700                                                                          
160800     INITIALIZE SGMT-WDE111                                               
160900     MOVE MID-IDDISTR       TO SGMT-IDDISTR                               
161000                               W-WDE111-IDDISTR                           
161100     MOVE MID-IDKUNDNR      TO SGMT-IDKUNDNR                              
161200                               W-WDE111-IDKUNDNR                          
161300     MOVE 'N'               TO SGMT-FLCOD                                 
161400     MOVE MID-IDDC          TO SGMT-IDDC                                  
161500     MOVE  SPACE            TO  SGMT-IDPARTNR                             
161600     MOVE  ZERO             TO  SGMT-KDFORSKN                             
161700                                SGMT-KDFKBIL                              
161800     MOVE -1                TO SGMT-KDLEVVIL                              
161900     MOVE 9                 TO SGMT-KDORDKL-MAX                           
162000     MOVE  SPACE            TO  SGMT-KDVALISO                             
162100     MOVE  ZERO             TO  SGMT-PRKURS                               
162200     COMPUTE SGMT-TISKEPPN-9KOMPL = 9999999 -                             
162300                                    SHIP-TISKEPPN                         
162400     PERFORM IMS-ISRT-WDE111                                              
162500                                                                          
162600     INITIALIZE  SKOLLI-WDE121                                            
162700     MOVE MID-IDPRODNR           TO SKOLLI-IDPRODNR                       
162800     MOVE MID-IDKOLLI            TO SKOLLI-IDKOLLI                        
162900     MOVE -1                     TO SKOLLI-KDORDKL                        
163000     MOVE ZERO                   TO SKOLLI-DIKOLLIB                       
163100                                    SKOLLI-DIKOLLIH                       
163200                                    SKOLLI-DIKOLLIL                       
163300     MOVE SPACE                  TO SKOLLI-FLDIRLEV                       
163400     MOVE ZERO                   TO SKOLLI-IDORDER                        
163500                                    SKOLLI-KDEMBTYP                       
163600                                    SKOLLI-KDFRAKT                        
163700                                    SKOLLI-KDFARLIG-KOLLI                 
163800     MOVE SPACE                  TO SKOLLI-KDKOLLI                        
163900     MOVE ZERO                   TO SKOLLI-KVFLAMP-KOLLI                  
164000                                    SKOLLI-SUORDV-LOC                     
164100                                    SKOLLI-SUORDV-LOCPREL                 
164200     MOVE SPACE                  TO SKOLLI-KDVALISO                       
164300                                    SKOLLI-KDVALISO-EXP                   
164400     MOVE ZERO                   TO SKOLLI-SUORDV                         
164500                                    SKOLLI-SUORDV-EXP                     
164600                                    SKOLLI-TIPACKN                        
164700                                    SKOLLI-VKORDBTO-KOLLI                 
164800                                    SKOLLI-VKORDNTO-KOLLI                 
164900                                    SKOLLI-VLORDBTO-KOLLI                 
165000                                    SKOLLI-IDDISTR                        
165100                                    SKOLLI-IDKUNDNR                       
165200                                    SKOLLI-IDFAKT-EXP                     
165300     MOVE SPACE                  TO SKOLLI-IDKUNDRF                       
165400                                    SKOLLI-KDFAKTYP                       
165500     MOVE ZERO                   TO SKOLLI-TIORDREG                       
165600                                    SKOLLI-IDFAKT                         
165700                                    SKOLLI-IDKOLLI-SAMP                   
165801     MOVE SPACE                  TO SKOLLI-FLCROSS                        
165900     PERFORM IMS-ISRT-WDE121                                              
166000     .                                                                    
166100                                                                          
166200                                                                          
166300 K-CHECK-EDI               SECTION.                                       
166400     MOVE 'K-CHECK-EDI     '              TO CURRENT-SECTION              
166500                                                                          
166600     IF WS-VORD-IDLEVNR = 'BQ8VA' OR 'BP8BA' OR                           
166700                          'BP7YA' OR 'BP3EA' OR                           
166800                          'BQ9QB' OR 'CWZYA' OR                           
166900                          'AE4PC' OR 'AE420' OR                           
167001                          'AEHBJ' OR 'AEL4V' OR                           
167100                          'AEBR5' OR 'AD0FR' OR                           
167200                          'AEO7U' OR 'AEF31' OR                           
167300                          'AEQJS' OR 'AERJP' OR                           
167400                          'AFGQF' OR 'AFPKB' OR                           
167400                          'AFQDR' OR                                      
167500                          'CBBNA' OR 'D1V4A' OR                           
167600                          'CXBGA' OR 'BSBZA' OR                           
167700                          'D0DMC' OR                                      
167800                          'N81ZA' OR 'S5XBA'                              
167900                                                                          
168000       IF DIST87-DIV-TRANSP                                               
168100         IF NOT DIST87-TRUCKWHEEL                                         
168200                                                                          
168300           MOVE W-IDSHIPM TO BC-URV-IDSHIPM                               
168400           MOVE '4390' TO MSGSOP-IDTRANS                                  
168500           MOVE '1'     TO MSGSOP-KDMFSFOR                                
168600           MOVE 'W476ST' TO MSGSOP-IDPROCESS                              
168700           MOVE 'O'     TO MSGSOP-KDSOPFUNK                               
168800                                                                          
168900           STRING 'URVAL(' WS-BC ')'                                      
169000                   DELIMITED BY SIZE INTO MSGSOP-TESYMBV                  
169100           PERFORM IMS-INSERT-ALTMSG-SOP                                  
169200         ELSE                                                             
169300           IF DIST87-TRUCKWHEEL AND                                       
169400              WS-VORD-IDLEVNR = 'AEL4V'                                   
169500                                                                          
169600             MOVE W-IDSHIPM TO BC-URV-IDSHIPM                             
169700             MOVE '4390' TO MSGSOP-IDTRANS                                
169800             MOVE '1'   TO MSGSOP-KDMFSFOR                                
169900             MOVE 'W476ST' TO MSGSOP-IDPROCESS                            
170000             MOVE 'O'   TO MSGSOP-KDSOPFUNK                               
170100                                                                          
170200             STRING 'URVAL(' WS-BC ')'                                    
170300                     DELIMITED BY SIZE INTO MSGSOP-TESYMBV                
170400             PERFORM IMS-INSERT-ALTMSG-SOP                                
170500           END-IF                                                         
170600         END-IF                                                           
170700       END-IF                                                             
170800     END-IF                                                               
170900                                                                          
171000* DDGS GODS FÖR DISTR. 778, 771                                           
171100* TRANSPORTÖR DANX,                                                       
171200* GÄLLER BARA LEVERANTÖREN OCH FRAKTKODER NEDAN.                          
171300*                                                                         
171400     IF WS-VORD-IDLEVNR = 'AEO7U'                                         
171500                                                                          
171601       IF DIST87-DANX-SE AND (WS-VORD-KDFRAKT = 5 OR 87)                  
172501                                                                          
172700         MOVE W-IDSHIPM TO BC-URV-IDSHIPM                                 
172800         MOVE '4390' TO MSGSOP-IDTRANS                                    
172900         MOVE '1'       TO MSGSOP-KDMFSFOR                                
173000         MOVE 'W476ST' TO MSGSOP-IDPROCESS                                
173100         MOVE 'O'       TO MSGSOP-KDSOPFUNK                               
173200                                                                          
173300         STRING 'URVAL(' WS-BC ')'                                        
173400                 DELIMITED BY SIZE INTO MSGSOP-TESYMBV                    
173500         PERFORM IMS-INSERT-ALTMSG-SOP                                    
173600       END-IF                                                             
173700                                                                          
173800*                                                                         
173900                                                                          
174000       IF DIST87-DANX-INT                                                 
174100                                                                          
174200         MOVE W-IDSHIPM TO BC-URV-IDSHIPM                                 
174300         MOVE '4390' TO MSGSOP-IDTRANS                                    
174400         MOVE '1'       TO MSGSOP-KDMFSFOR                                
174500         MOVE 'W476ST' TO MSGSOP-IDPROCESS                                
174600         MOVE 'O'       TO MSGSOP-KDSOPFUNK                               
174700                                                                          
174800         STRING 'URVAL(' WS-BC ')'                                        
174900                 DELIMITED BY SIZE INTO MSGSOP-TESYMBV                    
175000         PERFORM IMS-INSERT-ALTMSG-SOP                                    
175100       END-IF                                                             
175200                                                                          
175300*                                                                         
175400                                                                          
175500       IF DIST87-DANX-POLESTAR AND WS-VORD-KDFRAKT = 5                    
175600                                                                          
175700         MOVE W-IDSHIPM TO BC-URV-IDSHIPM                                 
175800         MOVE '4390' TO MSGSOP-IDTRANS                                    
175900         MOVE '1'       TO MSGSOP-KDMFSFOR                                
176000         MOVE 'W476ST' TO MSGSOP-IDPROCESS                                
176100         MOVE 'O'       TO MSGSOP-KDSOPFUNK                               
176200                                                                          
176300         STRING 'URVAL(' WS-BC ')'                                        
176400                 DELIMITED BY SIZE INTO MSGSOP-TESYMBV                    
176500         PERFORM IMS-INSERT-ALTMSG-SOP                                    
176600       END-IF                                                             
176700     END-IF                                                               
176800*                                                                         
176900* DDGS GODS FÖR DISTR. 1090 och 2635                                      
177000* TRANSPORTÖR DANX,                                                       
177100* GÄLLER BARA LEVERANTÖRNA NEDAN.                                         
177200*                                                                         
177300     IF WS-VORD-IDLEVNR = 'AEO7U' OR                                      
177400                          'AEF31' OR                                      
177500                          'BP7YA' OR                                      
177600                          'BP8BA'                                         
177700                                                                          
177800       IF DIST87-DANX                                                     
177900                                                                          
178000         MOVE W-IDSHIPM TO BC-URV-IDSHIPM                                 
178100         MOVE '4390' TO MSGSOP-IDTRANS                                    
178200         MOVE '1'       TO MSGSOP-KDMFSFOR                                
178300         MOVE 'W476ST' TO MSGSOP-IDPROCESS                                
178400         MOVE 'O'       TO MSGSOP-KDSOPFUNK                               
178500                                                                          
178600         STRING 'URVAL(' WS-BC ')'                                        
178700                 DELIMITED BY SIZE INTO MSGSOP-TESYMBV                    
178800         PERFORM IMS-INSERT-ALTMSG-SOP                                    
178900       END-IF                                                             
179000     END-IF                                                               
179100                                                                          
179200*                                                                         
179300* DDGS GODS FÖR DISTR. 974 och 978                                        
179400* TRANSPORTÖR DANX,                                                       
179500* GÄLLER BARA LEVERANTÖRNA NEDAN.                                         
179600*                                                                         
179700     IF WS-VORD-IDLEVNR = 'AEL4V' OR                                      
179800                          'BP8BA'                                         
179900                                                                          
180001       IF DIST87-DANX-DK AND                                              
180101         (WS-VORD-KDFRAKT = 34 OR 35 OR 37 OR 63)                         
180201                                                                          
180301         MOVE W-IDSHIPM TO BC-URV-IDSHIPM                                 
180401         MOVE '4390' TO MSGSOP-IDTRANS                                    
180501         MOVE '1'       TO MSGSOP-KDMFSFOR                                
180601         MOVE 'W476ST' TO MSGSOP-IDPROCESS                                
180701         MOVE 'O'       TO MSGSOP-KDSOPFUNK                               
180801                                                                          
180901         STRING 'URVAL(' WS-BC ')'                                        
181001                 DELIMITED BY SIZE INTO MSGSOP-TESYMBV                    
181101         PERFORM IMS-INSERT-ALTMSG-SOP                                    
181201       END-IF                                                             
181301     END-IF                                                               
181401                                                                          
181501                                                                          
181601* DDGS GODS FÖR NORGE - DISTR. 878, 8859                                  
181701* TRANSPORTÖR DHL                                                         
181801* GÄLLER BARA LEVERANTÖREN NEDAN                                          
181901*                                                                         
182001     IF WS-VORD-IDLEVNR = 'BP7YA'                                         
182101                                                                          
182201       IF DIST87-DHL-NO                                                   
182301                                                                          
182401         MOVE W-IDSHIPM TO BC-URV-IDSHIPM                                 
182501         MOVE '4390' TO MSGSOP-IDTRANS                                    
182601         MOVE '1'       TO MSGSOP-KDMFSFOR                                
182701         MOVE 'W476ST' TO MSGSOP-IDPROCESS                                
182801         MOVE 'O'       TO MSGSOP-KDSOPFUNK                               
182901                                                                          
183001         STRING 'URVAL(' WS-BC ')'                                        
183101                 DELIMITED BY SIZE INTO MSGSOP-TESYMBV                    
183201         PERFORM IMS-INSERT-ALTMSG-SOP                                    
183301                                                                          
183401       END-IF                                                             
183501                                                                          
183601     END-IF                                                               
183701     .                                                                    
183801 L-CHECK-EDI-EXCEPTION     SECTION.                                       
183901     MOVE 'L-CHECK-EDI-EXCEPTION'         TO CURRENT-SECTION              
184001                                                                          
184101* DDGS GODS FÖR TRANSPORTÖRER SOM FÅR EN ANNAN TYP                        
184201* AV EDI FIL ÄN DE VANLIGA TRANSPORTÖRERNA OCH DETTA                      
184301* NEDAN ÄR ETT UNDANTAG FÖR DE DDGS-LEV. VARS LEVERANSER                  
184401* GÅR VIA 'GÅRDEN'- CDC LAGRET.                                           
184501* DETTA GÄLLER FÖR GODS FÖR DANMARK (HIT=POSTNORD)                        
184601                                                                          
184701     IF WS-VORD-IDLEVNR = 'BQ8VA' OR                                      
184801                          'CWZYA' OR                                      
184901                          'BP7YA' OR                                      
185001                          'BP3EA' OR                                      
185101                          'AEF31' OR                                      
185201                          'AEO7U'                                         
185301                                                                          
185401       MOVE WS-IDDISTR       TO TEST-IDDISTR                              
185501                                                                          
185601       IF DIST83-HIT-DK                                                   
185701         MOVE WS-IDPRODNR    TO W-4504-IDPRODNR                           
185801         MOVE WS-IDKOLLI-NUM TO W-4504-IDKOLLI                            
185901         PERFORM IMS-GU-WDGX4503-04                                       
186001                                                                          
186101         IF SEGMENT-SAKNAS                                                
186201           MOVE WS-IDPRODNR              TO 4504-IDPRODNR                 
186301           MOVE WS-IDKOLLI-NUM           TO 4504-IDKOLLI                  
186401           MOVE OHUV-ADGMT-PADR          TO 4504-ADGMT-PADR               
186501                                                                          
186601           PERFORM LA-HAMTA-SORT-POS                                      
186701                                                                          
186801           MOVE KORD-IDGMTREF            TO 4504-IDGMTREF                 
186901                                                                          
187001           IF WS-KORD-KDORDKL = 0 OR 1 OR 2                               
187101             MOVE 06                     TO 4504-IDGODS                   
187201           ELSE                                                           
187301             IF WS-KORD-KDORDKL = 3 OR 4                                  
187401               MOVE 07                   TO 4504-IDGODS                   
187501             END-IF                                                       
187601           END-IF                                                         
187701                                                                          
187801           MOVE ZERO                     TO 4504-IDKLIID                  
187901                                                                          
188001           MOVE FUNCTION CURRENT-DATE(1:8)                                
188101                                         TO 4504-DAREGDAT                 
188201           MOVE WS-HHMMSSDD(1:4)         TO 4504-TIHHMM                   
188301           MOVE KOLLI-VKORDBTO-KOLLI     TO 4504-VKORDBTO-KOLLI           
188401           MOVE KOLLI-VLORDBTO-KOLLI     TO 4504-VLORDBTO-KOLLI           
188501           MOVE KOLLI-VLORDBTO-KOLLI     TO 4504-VLORDBTO-KOLLI           
188601                                                                          
188701           PERFORM IMS-ISRT-WDGX4503-04                                   
188801         END-IF                                                           
188901       END-IF                                                             
189001     END-IF                                                               
189101     .                                                                    
189201                                                                          
189301 LA-HAMTA-SORT-POS SECTION.                                               
189401     MOVE 'STA LA-HAMTA-'                 TO CURRENT-SECTION              
189501                                                                          
189601     MOVE WS-IDKUNDNR           TO W-IDKUNDNR-WDB2                        
189701     MOVE WS-IDDISTR            TO W-IDDISTR-WDB2                         
189801     PERFORM IMS-GU-WDB201                                                
189901                                                                          
190001     MOVE GMT-IDROUTE           TO WS-IDROUTE-HIT                         
190101     MOVE GMT-IDDEPOT           TO WS-IDDEPOT-HIT                         
190201                                                                          
190301     MOVE WS-ADFLGEO-HIT        TO 4504-ADFLGEO                           
190401     .                                                                    
190501     EJECT                                                                
190601                                                                          
190701 S08-HAMTA-MASKINDATUM     SECTION.                                       
190801     MOVE 'S08-MASKINDATUM '              TO CURRENT-SECTION              
190901                                                                          
191001     MOVE 'IDAG'                 TO   DAT-KDDATFORM                       
191101     CALL WDATKONV USING DAT-KDDATFORM                                    
191201                         DAT-I-TIDATUM                                    
191301                         DAT-O-TIDATUM                                    
191401                         DAT-KDSVAR                                       
191501     .                                                                    
191601                                                                          
191701                                                                          
191801 S09-INIT-KOLLI-VALUE      SECTION.                                       
191901     MOVE 'S09-INIT-KOLLI  '              TO CURRENT-SECTION              
192001                                                                          
192101     MOVE 0                 TO  KOLLI-KDKOLSTA                            
192201                                                                          
192301     IF WS-VORD-KDVIA NOT = '01'                                          
192401        MOVE WS-DAGENS-DATUM   TO  KOLLI-TIPACKN                          
192501        MOVE WS-HHMMSSDD       TO  WS-HHMMSSDD-RED                        
192601        MOVE WS-HHMMSS         TO  KOLLI-TIPACTID                         
192701     ELSE                                                                 
192801        MOVE ZERO              TO  KOLLI-TIPACKN                          
192901                                   KOLLI-TIPACTID                         
193001     END-IF                                                               
193101                                                                          
193201     MOVE WS-DARFS          TO  KOLLI-DARFS                               
193301     MOVE WS-IDKOLLI-NUM    TO  KOLLI-IDKOLLI                             
193401     MOVE WS-IDANSTNR       TO  KOLLI-IDPLOCK                             
193501     MOVE WS-IDDISTR        TO  KOLLI-IDDISTR                             
193601     MOVE WS-IDKUNDNR-NUM   TO  KOLLI-IDKUNDNR                            
193701     MOVE WS-KDEMBTYP       TO  KOLLI-KDEMBTYP                            
193801     MOVE WS-DIKOLLIL       TO  KOLLI-DIKOLLIL                            
193901     MOVE WS-DIKOLLIB       TO  KOLLI-DIKOLLIB                            
194001     MOVE WS-DIKOLLIH       TO  KOLLI-DIKOLLIH                            
194101                                                                          
194201*    IF WS-KOLLI-VLORDBTO > ZERO                                          
194301     IF WS-KOLLI-VLORDBTO > 0000.000                                      
194401       MOVE WS-KOLLI-VLORDBTO TO KOLLI-VLORDBTO-KOLLI                     
194501     ELSE                                                                 
194601       COMPUTE KOLLI-VLORDBTO-KOLLI ROUNDED                               
194701              = WS-DIKOLLIL                                               
194801              * WS-DIKOLLIB                                               
194901              * WS-DIKOLLIH                                               
195001              / 1000000                                                   
195101     END-IF                                                               
195201                                                                          
195301     MOVE KOLLI-VLORDBTO-KOLLI TO WS-KOLLI-VLORDBTO                       
195401                                                                          
195501     MOVE WS-KOLLI-VKORDBTO TO KOLLI-VKORDBTO-KOLLI                       
195601     MOVE WS-VORD-IDLEVNR   TO  KOLLI-IDLEVNR                             
195701     MOVE WS-VORD-KDVIA     TO  KOLLI-KDVIA                               
195801     MOVE WS-VORD-FLAUTFAK  TO  KOLLI-FLAUTFAK                            
195901     MOVE WS-IDDC           TO  KOLLI-IDDC                                
196001     MOVE WS-IDSUPREF       TO  KOLLI-IDSUPREF                            
196101     MOVE WS-DASUPREF       TO  KOLLI-DASUPREF                            
196201     MOVE WS-TISUPTID       TO  KOLLI-TISUPTID                            
196301     MOVE WS-KORD-KDORDKL   TO  KOLLI-KDORDKL                             
196401     MOVE SPACE             TO  KOLLI-ADFLGEO                             
196501                                KOLLI-KDKOLLI                             
196601                                KOLLI-KDARTURS-KOLLI                      
196701                                KOLLI-KDVALISO                            
196801                                KOLLI-KDVALISO-EXP                        
196901     MOVE NEJ               TO  KOLLI-FLBANDST                            
197001                                KOLLI-FLFRSUTS                            
197101     MOVE VORD-DEAL-PR-SUM  TO  KOLLI-DEAL-PR-SUM                         
197201     MOVE ZERO              TO  KOLLI-IDKOLLI-FLER                        
197301                                KOLLI-IDKOLLI-SAMP                        
197401                                KOLLI-VKORDNTO-KOLLI                      
197501                                KOLLI-IDFAKT                              
197601                                KOLLI-IDFAKT-EXP                          
197701                                KOLLI-IDTRPTNR                            
197801                                KOLLI-ADFLOMR                             
197901                                KOLLI-ADRUTNIV                            
198001                                KOLLI-ADVMODUL                            
198101                                KOLLI-ADHMODUL                            
198201                                KOLLI-IDFAKLOP                            
198301                                KOLLI-DIDMODUL                            
198401                                KOLLI-DIHMODUL                            
198501                                KOLLI-KVFALRAD                            
198601                                KOLLI-TIFAKT                              
198701                                KOLLI-TIFAKT-EXP                          
198801                                KOLLI-TIFAKTID                            
198901                                KOLLI-TIFAKTID-EXP                        
199001                                KOLLI-TILASTN                             
199101                                KOLLI-TILASTID                            
199201                                KOLLI-SUORDV-KOLLI                        
199301                                KOLLI-SUORDV-KLI-EXP                      
199401                                KOLLI-SUORDV-LOC                          
199501                                KOLLI-SUORDV-LOCPREL                      
199601                                KOLLI-KVORDRAD                            
199701                                KOLLI-TIAAVVD-PATR                        
199801                                KOLLI-KDFARLIG-KOLLI                      
199901                                KOLLI-KVFLAMP-KOLLI                       
200001                                KOLLI-IDLASTN                             
200101                                KOLLI-SUEQFG                              
200201                                KOLLI-IDSHIPM                             
200301                                KOLLI-IDTULLNR                            
200401                                KOLLI-RETULKS                             
200501*                                                                         
200601     MOVE +1 TO FG-INDX                                                   
200701     PERFORM UNTIL FG-INDX > FG-MAX-INDX                                  
200801       MOVE ZERO             TO KOLLI-IDPSN(FG-INDX)                      
200901                                KOLLI-VKART-FG(FG-INDX)                   
201001                                KOLLI-VLFG(FG-INDX)                       
201101       ADD +1 TO FG-INDX                                                  
201201     END-PERFORM                                                          
201301*                                                                         
201401     MOVE SPACE             TO  KOLLI-FLUTLAST                            
201501                                KOLLI-FLTULLG                             
201601                                KOLLI-IDLBBET                             
201701                                KOLLI-IDTRPLOS                            
201801                                KOLLI-IDTRPVAR                            
201901                                KOLLI-IDTULFTG                            
202001                                KOLLI-KDSTASKLI                           
202101                                KOLLI-FILLERX2                            
202201                                                                          
202301* FIX FÖR FARLIGT GODS FÖR VÅTA BAT.                                      
202401     IF VORD-IDLEVNR = 'S5PQB' OR 'V022A'                                 
202501       MOVE '0402'          TO  KOLLI-KDKOLLI                             
202601     END-IF                                                               
202701* END FIX                                                                 
202801     .                                                                    
202901                                                                          
203001                                                                          
203101 S10-UPPD-SPAR-KOLLI    SECTION.                                          
203201       MOVE 'STA S10-SEC '                    TO CURRENT-SECTION          
203301     COMPUTE ACC-KOLLI-VKORDNTO ROUNDED = ACC-KOLLI-VKORDNTO +            
203401                    WS-ORAD-VKARTNTO * WS-ORAD-KVLEVART                   
203501     END-COMPUTE                                                          
203601*                                                                         
203701     COMPUTE ACC-ORAD-VLARTNTO          = ACC-ORAD-VLARTNTO +             
203801                    WS-ORAD-VLARTNTO * WS-ORAD-KVLEVART                   
203901     END-COMPUTE                                                          
204001*                                                                         
204101     MOVE MID-IDDISTR     TO TEST-IDDISTR                                 
204201                                                                          
204301     IF DIST79-DEALER-PRICE                                               
204401       IF  ORAD-PRARTNTO-LOCPREL > 0                                      
204501         COMPUTE ACC-KOLLI-SUORDV-LOCPREL =                               
204601                   ACC-KOLLI-SUORDV-LOCPREL +                             
204701                   WS-ORAD-PRARTNTO-LOCPREL * WS-ORAD-KVLEVART            
204801         END-COMPUTE                                                      
204901       ELSE                                                               
205001         COMPUTE ACC-KOLLI-SUORDV-LOC =                                   
205101                   ACC-KOLLI-SUORDV-LOC +                                 
205201                   WS-ORAD-PRARTNTO-LOC * WS-ORAD-KVLEVART                
205301          END-COMPUTE                                                     
205401       END-IF                                                             
205501     ELSE                                                                 
205601       IF DIST79-ECOM-PRICE                                               
205701         COMPUTE ACC-KOLLI-SUORDV-LOC =                                   
205801                   ACC-KOLLI-SUORDV-LOC +                                 
205901                   WS-ORAD-PRARTNTO-LOC * WS-ORAD-KVLEVART                
206001       ELSE                                                               
206101         COMPUTE ACC-KOLLI-SUORDV = ACC-KOLLI-SUORDV +                    
206201                      WS-ORAD-PRARTNTO * WS-ORAD-KVLEVART                 
206301         END-COMPUTE                                                      
206401       END-IF                                                             
206501     END-IF                                                               
206601     MOVE WS-ORAD-KDVALISO   TO ACC-KOLLI-KDVALISO                        
206701*                                                                         
206801*                                                                         
206901*                                                                         
207001     IF   WS-ORAD-KVFLAMP   > ZERO                                        
207101     AND (WS-ORAD-KVFLAMP   < ACC-KOLLI-KVFLAMP                           
207201     OR   ACC-KOLLI-KVFLAMP = ZERO)                                       
207301       MOVE WS-ORAD-KVFLAMP  TO ACC-KOLLI-KVFLAMP                         
207401     END-IF                                                               
207501                                                                          
207601     IF  WS-ORAD-KDFARLIG = +2 OR +3 OR +4 OR +7                          
207701     AND WS-ORAD-KDFARLIG > ACC-KOLLI-KDFARLIG                            
207801       MOVE WS-ORAD-KDFARLIG TO ACC-KOLLI-KDFARLIG                        
207901     END-IF                                                               
208001*                                                                         
208101       MOVE 'SLUT S10-SEC '                   TO CURRENT-SECTION          
208201     .                                                                    
208301                                                                          
208401                                                                          
208501 S11-UPPD-KOLLI-FRAN-ARB   SECTION.                                       
208601       MOVE 'STA S11-SEC '                    TO CURRENT-SECTION          
208701                                                                          
208801     IF  MID-FLSLUT = JA                                                  
208901     AND WS-VORD-KDVIA NOT = '01'                                         
209001        MOVE 1                   TO  KOLLI-KDKOLSTA                       
209101        PERFORM S22-SKAPA-TRANS-TILL-SV-AF                                
209201                                                                          
209301     END-IF                                                               
209401*                                                                         
209501     MOVE WS-DARFS               TO KOLLI-DARFS                           
209601*                                                                         
209701                                                                          
209801     ADD  ACC-KOLLI-KVFALRAD     TO KOLLI-KVFALRAD                        
209901     ADD  ACC-KOLLI-KVORDRAD     TO KOLLI-KVORDRAD                        
210001     ADD  ACC-KOLLI-VKORDNTO     TO KOLLI-VKORDNTO-KOLLI                  
210101     ADD  ACC-KOLLI-SUORDV       TO KOLLI-SUORDV-KOLLI                    
210201     ADD  ACC-KOLLI-SUORDV-LOC   TO KOLLI-SUORDV-LOC                      
210301     ADD  ACC-KOLLI-SUORDV-LOCPREL TO KOLLI-SUORDV-LOCPREL                
210401     MOVE ACC-KOLLI-KDVALISO     TO KOLLI-KDVALISO                        
210501*                                                                         
210601     IF KOLLI-VKORDNTO-KOLLI >= KOLLI-VKORDBTO-KOLLI                      
210701       ADD 0.1                      TO KOLLI-VKORDBTO-KOLLI               
210801     END-IF                                                               
210901*                                                                         
211001     IF  ACC-KOLLI-KVFLAMP    > ZERO                                      
211101     AND (ACC-KOLLI-KVFLAMP   < KOLLI-KVFLAMP-KOLLI                       
211201     OR   KOLLI-KVFLAMP-KOLLI = ZERO)                                     
211301       MOVE ACC-KOLLI-KVFLAMP    TO KOLLI-KVFLAMP-KOLLI                   
211401     END-IF                                                               
211501*                                                                         
211601     IF ACC-KOLLI-KDFARLIG > KOLLI-KDFARLIG-KOLLI                         
211701         MOVE ACC-KOLLI-KDFARLIG TO KOLLI-KDFARLIG-KOLLI                  
211801     END-IF                                                               
211901                                                                          
212001     IF KOLLI-KDFARLIG-KOLLI = +4                                         
212101     OR KOLLI-KDFARLIG-KOLLI = +7                                         
212201       MOVE +950                    TO KOLLI-ADFLOMR                      
212301     END-IF                                                               
212401       MOVE 'SLUT S11-SEC '                   TO CURRENT-SECTION          
212501     .                                                                    
212601                                                                          
212701 S13-UPPDAT-KDORDSTA SECTION.                                             
212801       MOVE 'STA S13-SEC  '                   TO CURRENT-SECTION          
212901       MOVE  WS-KORD-IDORDER      TO W-201-IDORDER                        
213001       MOVE  WS-VORD-IDLEVNR      TO W-WDQ211-IDLEVNR                     
213101       MOVE  WS-IDDC              TO W-WDQ211-IDDC                        
213201       PERFORM IMS-GHU-ORQI11                                             
213301                                                                          
213401       MOVE 'U*'                  TO DIRL-KDORDSTA                        
213501       PERFORM IMS-REPL-ORQI11                                            
213601       MOVE 'SLUT S13-SEC '                   TO CURRENT-SECTION          
213701     .                                                                    
213801                                                                          
213901                                                                          
214001 S16-UPPD-FARLIGT-GODS-DATA SECTION.                                      
214101       MOVE 'STA S16-SEC '                    TO CURRENT-SECTION          
214201                                                                          
214301     MOVE +1 TO FG-INDX                                                   
214401     PERFORM UNTIL FG-INDX > FG-MAX-INDX                                  
214501       IF TAB-IDPSN(FG-INDX) > ZERO                                       
214601                                                                          
214701         MOVE +1                   TO IDPSN-IX                            
214801         PERFORM UNTIL IDPSN-IX > FG-MAX-INDX                             
214901           IF KOLLI-IDPSN(IDPSN-IX) = 0                                   
215001             MOVE TAB-IDPSN(FG-INDX)   TO KOLLI-IDPSN(IDPSN-IX)           
215101           END-IF                                                         
215201           IF TAB-IDPSN(FG-INDX) = KOLLI-IDPSN(IDPSN-IX)                  
215301             ADD TAB-VKART-FG(FG-INDX) TO KOLLI-VKART-FG(IDPSN-IX)        
215401             ADD TAB-VLFG(FG-INDX)     TO KOLLI-VLFG(IDPSN-IX)            
215501             MOVE FG-MAX-INDX      TO IDPSN-IX                            
215601           END-IF                                                         
215701           ADD +1                  TO IDPSN-IX                            
215801         END-PERFORM                                                      
215901                                                                          
216001       END-IF                                                             
216101       ADD +1 TO FG-INDX                                                  
216201     END-PERFORM                                                          
216301                                                                          
216401     IF TAB-IDPSN(1) > ZERO                                               
216501       MOVE TOTAL-SUEQFG TO KOLLI-SUEQFG                                  
216601     ELSE                                                                 
216701       MOVE ZERO         TO KOLLI-SUEQFG                                  
216801     END-IF                                                               
216901       MOVE 'SLUT S16-SEC '                   TO CURRENT-SECTION          
217001     .                                                                    
217101                                                                          
217201                                                                          
217301 S17-BERAEKNA-FG-FAELT SECTION.                                           
217401     MOVE 'STA S17-SEC '                    TO CURRENT-SECTION            
217501     COMPUTE TAB-VLFG(FG-INDX) = TAB-VLFG(FG-INDX) +                      
217601                                 (WS-ORAD-VLFG          *                 
217701                                  KKOLLI-KVLEVART)                        
217801     END-COMPUTE                                                          
217901     IF WS-ORAD-IDPSN = 10 OR 11                                          
218001       COMPUTE TAB-VKART-FG(FG-INDX) = TAB-VKART-FG(FG-INDX) +            
218101                                       (WS-ORAD-VKART-FG     *            
218201                                        KKOLLI-KVLEVART)                  
218301       END-COMPUTE                                                        
218401     ELSE                                                                 
218501       MOVE ZERO TO TAB-VKART-FG(FG-INDX)                                 
218601     END-IF                                                               
218701                                                                          
218801     MOVE 10 TO FG-INDX                                                   
218901     MOVE 'SLUT S17-SEC '                   TO CURRENT-SECTION            
219001     .                                                                    
219101                                                                          
219201                                                                          
219301 S21-INIT-WS-FIELDS    SECTION.                                           
219401     MOVE 'STA S21-SEC  '                   TO CURRENT-SECTION            
219501                                                                          
219601     MOVE ZERO                   TO INX-TOT-ANT-RADER                     
219701                                    ACC-KOLLI-VKORDNTO                    
219801                                    ACC-ORAD-VLARTNTO                     
219901                                    ACC-ORAD-VKARTNTO                     
220001                                    WS-ORAD-KVFLAMP                       
220101                                    WS-ORAD-KDFARLIG                      
220201                                    WS-ORAD-PRARTNTO                      
220301                                    WS-ORAD-PRARTNTO-LOC                  
220401                                    WS-ORAD-PRARTNTO-LOCPREL              
220501                                    WS-ORAD-VKARTNTO                      
220601                                    WS-ORAD-KVLEVART                      
220701                                    WS-ORAD-IDPSN                         
220801                                    WS-ORAD-VKART-FG                      
220901                                    WS-ORAD-VLFG                          
221001                                    WS-ORAD-SUEQFG                        
221101                                    TOTAL-SUEQFG                          
221201                                    WS-ANT-KLARA-RADER                    
221301                                    WS-KORD-KDORDKL                       
221401                                    WS-KORD-IDORDER                       
221501                                    WS-KOLLI-VLORDBTO                     
221601                                    WS-DIKOLLIL                           
221701                                    WS-DIKOLLIB                           
221801                                    WS-DIKOLLIH                           
221901                                    WS-DARFS                              
222001                                    WS-KDORDSTA                           
222101                                    WS-TTMMSS                             
222201                                    WS-HH                                 
222301                                    WS-HHMMSS                             
222401                                    WS-DD                                 
222501                                    ACC-KOLLI-VKORDNTO                    
222601                                    ACC-KOLLI-KVFLAMP                     
222701                                    ACC-KOLLI-KDFARLIG                    
222801                                    ACC-KOLLI-KVORDRAD                    
222901                                    ACC-KOLLI-KVFALRAD                    
223001                                    ACC-KOLLI-SUORDV                      
223101                                    ACC-KOLLI-SUORDV-LOC                  
223201                                    ACC-KOLLI-SUORDV-LOCPREL              
223301                                                                          
223401     MOVE +74                    TO MAX-RAD-ANTAL                         
223501     MOVE SPACE                  TO WS-INDATA-TEST                        
223601                                    WS-ORAD-KDVALISO                      
223701                                    ACC-KOLLI-KDVALISO                    
223801     MOVE 'SLUT S21-SEC '                   TO CURRENT-SECTION            
223901     .                                                                    
224001                                                                          
224101                                                                          
224201 S22-SKAPA-TRANS-TILL-SV-AF SECTION.                                      
224301     MOVE 'S22-TRAN-TILL-AF'              TO CURRENT-SECTION              
224401                                                                          
224501     MOVE WS-IDDISTR          TO TEST-IDDISTR                             
224601*SKAPA INFO TILL SVENSKA ÅF, SÄNDS VIA VR.                                
224701     IF DIST03-SVERIGE-100-799                                            
224801     OR DIST03-NORGE                                                      
224901     OR DIST03-DANMARK-900                                                
225001     OR DIST85-PU-VIA-VR                                                  
225101     OR DIST21-TYRE                                                       
225201     AND NOT DIST47-INTERNA                                               
225301        MOVE WS-IDPRODNR TO XXJK-4322-IDPRODNR                            
225401        MOVE KOLLI-IDKOLLI  TO XXJK-4322-IDKOLLI                          
225501        MOVE XXJK-4322-WDGX4322 TO 4322-WDGX4322                          
225601        PERFORM IMS-ISRT-4322-SEGM                                        
225701     END-IF                                                               
225801     .                                                                    
225901                                                                          
226001                                                                          
226101 S23-OPEN-WZ01 SECTION.                                                   
226201     MOVE 'S23-OPEN-WZ01   '              TO CURRENT-SECTION              
226301                                                                          
226401     MOVE 'OPEN'                     TO SEND-KDFUNC                       
226501     MOVE 'CARPARTS.PULS.ADDIT   '   TO SEND-ADDISPABS                    
226601     MOVE SPACE                      TO SEND-ADDISPABS-RETURN             
226701                                                                          
226801     CALL WZ01SEND   USING      SEND-CONTROL-AREA                         
226901                                SEND-OPEN-AREA                            
227001     IF SEND-KDRC > 0                                                     
227101       MOVE SEND-KDRC           TO KDRC-DISP                              
227201       STRING 'WZ01SEND-OPEN RC-ERR = ' KDRC-DISP                         
227301            DELIMITED BY SIZE INTO ERROR-TEXT                             
227401       CALL FELLOG                                                        
227501     ELSE                                                                 
227601       MOVE SEND-IDCOM               TO WS-IDCOM                          
227701     END-IF                                                               
227801     .                                                                    
227901                                                                          
228001                                                                          
228101 S24-SEND-WZ01 SECTION.                                                   
228201     MOVE 'S24-SEND-WZ01   '              TO CURRENT-SECTION              
228301                                                                          
228401     MOVE W-IDSHIPM          TO MOD4636-MID-IDSHIPM                       
228501     MOVE 'N'                TO MOD4636-MID-KDTRPINF                      
228601     MOVE MID-IDDISTR        TO TEST-IDDISTR                              
228701                                                                          
228801     IF (DCS-DDC AND DCS-GERMANY                                          
228901                AND (WS-VORD-IDLEVNR = '6492 ' OR                         
229001                     WS-VORD-IDLEVNR = 'BZFFA'))                          
229101        OR (DCS-DDC AND DCS-BELGIUM)                                      
229201        OR (DCS-DDC AND DCS-SWEDEN)                                       
229301        OR (DCS-DDC AND DCS-NORWAY)                                       
229401        OR (DCS-DDC AND DCS-FRANCE)                                       
229501        OR (DCS-DDC AND DCS-KOREA )                                       
229502        OR (DCS-DDC AND DCS-ENGLAND)                                      
229601        OR (DCS-DDC AND DCS-TURKEY)                                       
229601        OR (DCS-DDC AND DCS-AUSTRALIA)                                    
229701*       OR (DCS-DDC AND DCS-HUNGARY)                                      
229801*       OR (DCS-DDC AND DCS-POLAND)                                       
229901*       OR (DCS-DDC AND DCS-MAROCKO)                                      
230001       MOVE 'J'              TO MOD4636-MID-KDTRPINF                      
230101     END-IF                                                               
230201                                                                          
230301     MOVE ZERO               TO MOD4636-MID-IDDISTR                       
230401                                MOD4636-MID-IDKUNDNR                      
230501                                MOD4636-MID-IDPRODNR                      
230601                                MOD4636-MID-IDKOLLI                       
230701                                MOD4636-MID-SUORDV-DIST                   
230801                                MOD4636-MID-SUORDV-DIST-LOC               
230901                                MOD4636-MID-SUORDV-DIST-PREL              
231001                                MOD4636-MID-SUORDV-NOLL                   
231101                                MOD4636-MID-SUORDV-NOLL-LOC               
231201                                MOD4636-MID-SUORDV-NOLL-PREL              
231301                                MOD4636-MID-KDORDKL                       
231401     MOVE 'PUT'                      TO SEND-KDFUNC                       
231501     COMPUTE SEND-KVDLEN = LENGTH OF MOD4636-MID-W40636I1                 
231601                                                                          
231701     CALL WZ01SEND   USING      SEND-CONTROL-AREA                         
231801                                SEND-KVDLEN                               
231901                                MOD4636-MID-W40636I1                      
232001     IF SEND-KDRC > 0                                                     
232101       MOVE SEND-KDRC           TO KDRC-DISP                              
232201       STRING 'WZ01SEND-PUT RC-ERR = ' KDRC-DISP                          
232301            DELIMITED BY SIZE INTO ERROR-TEXT                             
232401       CALL FELLOG                                                        
232501**  DVKTST  START                                                         
232601*    ELSE                                                                 
232701*       ADD  1  TO DVK-INDX                                               
232801*       STRING 'ADDIT*' MOD4636-MID-IDSHIPM '*'                           
232901*               DELIMITED BY SIZE INTO DVK-TEXT (DVK-INDX)                
233001**  DVKTST  END                                                           
233101     END-IF                                                               
233201     .                                                                    
233301                                                                          
233401                                                                          
233501 S25-CLOSE-WZ01  SECTION.                                                 
233601     MOVE 'S25-CLOSE-WZ01  '              TO CURRENT-SECTION              
233701                                                                          
233801     MOVE 'CLOSE'               TO SEND-KDFUNC                            
233901                                                                          
234001     CALL WZ01SEND   USING      SEND-CONTROL-AREA                         
234101     IF SEND-KDRC > 0                                                     
234201       MOVE SEND-KDRC           TO KDRC-DISP                              
234301       STRING 'WZ01SEND-CLOSE RC-ERR = ' KDRC-DISP                        
234401            DELIMITED BY SIZE INTO ERROR-TEXT                             
234501       CALL FELLOG                                                        
234601     END-IF                                                               
234701     .                                                                    
234801                                                                          
234901                                                                          
235001 S27-SHIPM-NUMBER SECTION.                                                
235101     MOVE 'S27-SHIPM-NUMBER'              TO CURRENT-SECTION              
235201                                                                          
235301     CALL W476SHNO USING SHNO-W476SHNO 4517-PCB                           
235401     .                                                                    
235501                                                                          
235601                                                                          
235701 S30-GENERERA-LARM-MAIL SECTION.                                          
235801     MOVE 'S30-LARM-MAIL   ' TO CURRENT-SECTION                           
235901                                                                          
236001     IF FIRST-MAIL-RAD                                                    
236101        PERFORM S90-SEND-OPEN                                             
236201        MOVE JA TO SW-MAIL-SKICKAT                                        
236301        PERFORM S90-PUT-DAP-START                                         
236401        PERFORM S31-REDIGERA-RUBRIK                                       
236501        PERFORM S32-REDIGERA-KOMMENTAR                                    
236601     END-IF                                                               
236701                                                                          
236801     PERFORM S33-REDIGERA-RAD                                             
236901     .                                                                    
237001                                                                          
237101                                                                          
237201 S31-REDIGERA-RUBRIK    SECTION.                                          
237301     MOVE 'S31-REIGERA-RUBR' TO CURRENT-SECTION                           
237401                                                                          
237501                                                                          
237601     MOVE WS-DAGENS-DATUM   TO F4R1-DATUM                                 
237701     MOVE F4-RUBRIK1        TO SEND-RAD                                   
237801     PERFORM S90-PUT-DOC-LINE                                             
237901                                                                          
238001     MOVE SPACE             TO SEND-RAD                                   
238101     PERFORM S90-PUT-DOC-LINE                                             
238201                                                                          
238301     MOVE SPACE             TO SEND-RAD                                   
238401     PERFORM S90-PUT-DOC-LINE                                             
238501                                                                          
238601     MOVE F4-RUBRIK2        TO SEND-RAD                                   
238701     PERFORM S90-PUT-DOC-LINE                                             
238801                                                                          
238901     MOVE SPACE             TO SEND-RAD                                   
239001     PERFORM S90-PUT-DOC-LINE                                             
239101                                                                          
239201     .                                                                    
239301                                                                          
239401                                                                          
239501 S32-REDIGERA-KOMMENTAR SECTION.                                          
239601     MOVE 'S32-RED-KOMMENT ' TO CURRENT-SECTION                           
239701                                                                          
239801     MOVE IDPGM                    TO F4-IDPGM                            
239901                                                                          
240001     MOVE F4-KOMMENTAR-RAD         TO SEND-RAD                            
240101     PERFORM S90-PUT-DOC-LINE                                             
240201                                                                          
240301     .                                                                    
240401                                                                          
240501                                                                          
240601 S33-REDIGERA-RAD       SECTION.                                          
240701     MOVE 'S32-REIGERA-RAD ' TO CURRENT-SECTION                           
240801                                                                          
240901     MOVE W-IDLEVNR-F4             TO F4-RAD-IDLEVNR                      
241001     MOVE W-IDPRODNR-F4-MIN        TO F4-RAD-IDPRODNR                     
241101     MOVE W-IDPURAD-F4-MIN         TO F4-RAD-IDPURAD                      
241201     MOVE ORAD-TIUTSKR             TO F4-RAD-TIUTSKR                      
241301                                                                          
241401     MOVE F4-RAD                   TO SEND-RAD                            
241501     PERFORM S90-PUT-DOC-LINE                                             
241601     .                                                                    
241701                                                                          
241801                                                                          
241901 S90-SEND-OPEN SECTION.                                                   
242001     MOVE 'S90-SEND-OPEN'           TO CURRENT-SECTION.                   
242101                                                                          
242201     MOVE 'OPEN'                    TO SEND-KDFUNC                        
242301     MOVE 'CARPARTS.DAP.DISTRDOC'   TO SEND-ADDISPABS                     
242401     CALL WZ01SEND USING SEND-CONTROL-AREA                                
242501                         SEND-OPEN-AREA                                   
242601     IF SEND-KDRC > 0                                                     
242701       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
242801       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
242901       DELIMITED BY SIZE INTO FELTEXT                                     
243001       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
243101     END-IF                                                               
243201     .                                                                    
243301                                                                          
243401                                                                          
243501 S90-PUT-DAP-START SECTION.                                               
243601     MOVE 'S90-PUT-DAP-START'     TO CURRENT-SECTION.                     
243701                                                                          
243801     MOVE 1                       TO REQU-IDMSGVER                        
243901     MOVE 'R'                     TO REQU-KDPGMACT                        
244001     MOVE IDPGM                   TO REQU-IDUSER                          
244101     MOVE 'WDF4'                  TO HDR-IDOUTTYPE                        
244201     MOVE SPACE                   TO HDR-IDOUTREC                         
244301                                     HDR-IDLIST                           
244401     MOVE 'LARM'                  TO HDR-IDOUTREC (1:4)                   
244501                                     HDR-IDLIST                           
244601     MOVE 'PUT'                   TO SEND-KDFUNC                          
244701     MOVE LENGTH OF HDR-AREA      TO SEND-KVDLEN                          
244801                                                                          
244901                                                                          
245001     CALL WZ01SEND USING SEND-CONTROL-AREA                                
245101                         SEND-KVDLEN                                      
245201                         HDR-AREA                                         
245301     IF SEND-KDRC > ZERO                                                  
245401       MOVE SEND-KDRC               TO KDRC-DISPLAY                       
245501       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
245601       DELIMITED BY SIZE          INTO FELTEXT-STR                        
245701       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
245801     END-IF                                                               
245901     .                                                                    
246001                                                                          
246101                                                                          
246201 S90-PUT-DOC-LINE SECTION.                                                
246301     MOVE 'S90-PUT-DOC-LINE' TO CURRENT-SECTION.                          
246401                                                                          
246501     MOVE 'PUT'                           TO SEND-KDFUNC                  
246601     MOVE LENGTH OF SEND-RAD              TO SEND-KVDLEN                  
246701     CALL WZ01SEND USING SEND-CONTROL-AREA                                
246801                         SEND-KVDLEN                                      
246901                         SEND-RAD                                         
247001     IF SEND-KDRC > ZERO                                                  
247101       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
247201       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
247301       DELIMITED BY SIZE INTO FELTEXT-STR                                 
247401       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
247501     END-IF                                                               
247601     .                                                                    
247701                                                                          
247801                                                                          
247901 S90-SEND-CLOSE SECTION.                                                  
248001     MOVE 'S90-SEND-CLOSE' TO CURRENT-SECTION.                            
248101                                                                          
248201     MOVE 'CLOSE'                    TO SEND-KDFUNC                       
248301     CALL WZ01SEND USING SEND-CONTROL-AREA                                
248401                                                                          
248501     IF SEND-KDRC > 0                                                     
248601       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
248701       STRING 'WZ01SEND CLOSE ERROR RC=' KDRC-DISPLAY                     
248801       DELIMITED BY SIZE INTO FELTEXT                                     
248901       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
249001     END-IF                                                               
249101     .                                                                    
249201 Z-DISPATCH-AVSLUT     SECTION.                                           
249301     MOVE 'Z-DISPATCH-AVSL '              TO CURRENT-SECTION              
249401                                                                          
249501*    SKRIV FEL/KLAR MEDDELANDE TILL MPP DISPATCHERN                       
249601     IF MSG-KOM-IDMFSMED = SPACE                                          
249701        MOVE OK-BEHANDLAD      TO MSG-KOM-IDMFSMED                        
249801     END-IF                                                               
249901     PERFORM IMS-INSERT-DISP-MSG                                          
250001     .                                                                    
250101                                                                          
250201                                                                          
250301                                                                          
250401*IMS SEKTIONER                                                            
250501*                                                                         
250601*                 IIIIIIIIIII MMMMMMMMMMM SSSSSSSSSSS                     
250701*                 III     III MM MMMMM MM SSSS   SSSS                     
250801*                 IIIII IIIII MM  MMM  MM SSS SSS SSS                     
250901*                 IIIII IIIII MM M M M MM SSS  SSSSSS                     
251001*                 IIIII IIIII MM MM MM MM SSSSSS  SSS                     
251101*                 IIIII IIIII MM MMMMM MM SSS SSS SSS                     
251201*                 III     III MM MMMMM MM SSSS   SSSS                     
251301*                 IIIIIIIIIII MMMMMMMMMMM SSSSSSSSSSS                     
251401*                                                                         
251501*                                                                         
251601 IMS-INSERT-ALTMSG-SOP SECTION.                                           
251701                                                                          
251801     MOVE SPACE TO GODK-STATUSKODER                                       
251901     CALL CBLTDLI USING ISRT ALT-PCB PROG-TO-PROG-SW                      
252001     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
252101     PERFORM IMS-STATUSKONTROLL                                           
252201     .                                                                    
252301                                                                          
252401                                                                          
252501 IMS-GU-MSG-AREA SECTION.                                                 
252601                                                                          
252701     MOVE '  QC' TO GODK-STATUSKODER                                      
252801     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
252901     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
253001     PERFORM IMS-STATUSKONTROLL                                           
253101     .                                                                    
253201                                                                          
253301                                                                          
253401 IMS-GN-KOM-AREA SECTION.                                                 
253501                                                                          
253601     MOVE '  '   TO GODK-STATUSKODER                                      
253701     CALL CBLTDLI USING GN MSG-PCB KOM-IO-AREA                            
253801     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
253901     PERFORM IMS-STATUSKONTROLL                                           
254001     .                                                                    
254101                                                                          
254201                                                                          
254301 IMS-INSERT-DISP-MSG SECTION.                                             
254401                                                                          
254501     MOVE SPACE TO GODK-STATUSKODER                                       
254601     CALL CBLTDLI USING ISRT DISP-PCB KOM-IO-AREA                         
254701     MOVE DISP-STATUS-CODE TO STATUS-WS                                   
254801     PERFORM IMS-STATUSKONTROLL                                           
254901     .                                                                    
255001                                                                          
255101                                                                          
255201 IMS-GU-WDE401 SECTION.                                                   
255301     MOVE 'IMS-GU-WDE401   '    TO CURRENT-IMS-SECTION                    
255401                                                                          
255501     MOVE SPACE                 TO ALL-SSA                                
255601     STRING 'WDE401  (WDE401KY =' W-WDE401-KUNDORDER-X ')'                
255701            DELIMITED BY SIZE INTO SSA1                                   
255801     MOVE '  GE'                TO GODK-STATUSKODER                       
255901     CALL CBLTDLI USING GU     WDE4-PCB KORD-WDE401 SSA1                  
256001     MOVE WDE4-STATUS-CODE      TO STATUS-WS                              
256101     PERFORM IMS-STATUSKONTROLL                                           
256201     .                                                                    
256301                                                                          
256401                                                                          
256501 IMS-GNP-WDE411 SECTION.                                                  
256601     MOVE 'IMS-GNP-WDE411  '    TO CURRENT-IMS-SECTION                    
256701                                                                          
256801     MOVE SPACE                 TO ALL-SSA                                
256901     STRING 'WDE411  (IDPURAD  =' W-WDE411-IDPURAD-X ')'                  
257001            DELIMITED BY SIZE INTO SSA1                                   
257101     MOVE '  GE'                TO GODK-STATUSKODER                       
257201     CALL CBLTDLI USING GNP    WDE4-PCB ORAD-WDE411 SSA1                  
257301     MOVE WDE4-STATUS-CODE      TO STATUS-WS                              
257401     PERFORM IMS-STATUSKONTROLL                                           
257501     .                                                                    
257601                                                                          
257701                                                                          
257801 IMS-GHU-KUNDORDER SECTION.                                               
257901     MOVE 'IMS-GHU-KUNDORDE'    TO CURRENT-IMS-SECTION                    
258001                                                                          
258101     MOVE SPACE                 TO ALL-SSA                                
258201     STRING 'WDE401  (WDE401KY =' W-WDE401-KUNDORDER-X ')'                
258301            DELIMITED BY SIZE INTO SSA1                                   
258401     MOVE '    '                TO GODK-STATUSKODER                       
258501     CALL CBLTDLI USING GHU    WDE4-PCB KORD-WDE401 SSA1                  
258601     MOVE WDE4-STATUS-CODE      TO STATUS-WS                              
258701     PERFORM IMS-STATUSKONTROLL                                           
258801     .                                                                    
258901                                                                          
259001                                                                          
259101 IMS-REPL-WDE401    SECTION.                                              
259201     MOVE 'IMS-REPL-WDE401 '    TO CURRENT-IMS-SECTION                    
259301                                                                          
259401     MOVE SPACE                 TO ALL-SSA                                
259501     MOVE '  '                  TO GODK-STATUSKODER                       
259601     CALL CBLTDLI USING REPL WDE4-PCB KORD-WDE401                         
259701     MOVE WDE4-STATUS-CODE      TO STATUS-WS                              
259801     PERFORM IMS-STATUSKONTROLL                                           
259901     .                                                                    
260001                                                                          
260101                                                                          
260201 IMS-GHU-WDE401              SECTION.                                     
260301     MOVE 'IMS-GHU-WDE401  '    TO CURRENT-IMS-SECTION                    
260401                                                                          
260501     MOVE SPACE                 TO ALL-SSA                                
260601     STRING 'WDE401  (WDE401KY =' W-WDE401-KUNDORDER-X ')'                
260701            DELIMITED BY SIZE INTO SSA1                                   
260801     MOVE '    '                TO GODK-STATUSKODER                       
260901     CALL CBLTDLI USING GHU    WDE41-PCB KORD-WDE401 SSA1                 
261001     MOVE WDE41-STATUS-CODE     TO STATUS-WS                              
261101     PERFORM IMS-STATUSKONTROLL                                           
261201     .                                                                    
261301                                                                          
261401                                                                          
261501 IMS-GHNP-WDE411            SECTION.                                      
261601     MOVE 'IMS-GHU-WDE401  '    TO CURRENT-IMS-SECTION                    
261701                                                                          
261801     MOVE SPACE                 TO ALL-SSA                                
261901     STRING 'WDE411  (IDPURAD  =' W-WDE411-IDPURAD-X ')'                  
262001            DELIMITED BY SIZE INTO SSA1                                   
262101     MOVE '    '                TO GODK-STATUSKODER                       
262201     CALL CBLTDLI USING GHNP   WDE41-PCB ORAD-WDE411 SSA1                 
262301     MOVE WDE41-STATUS-CODE     TO STATUS-WS                              
262401     PERFORM IMS-STATUSKONTROLL                                           
262501     .                                                                    
262601                                                                          
262701                                                                          
262801 IMS-GHNP-WDE411-F          SECTION.                                      
262901     MOVE 'IMS-GHNP-WDE411F'    TO CURRENT-IMS-SECTION                    
263001                                                                          
263101     MOVE SPACE                 TO ALL-SSA                                
263201     STRING 'WDE411  *F(IDPURAD  =' W-WDE411-IDPURAD-X ')'                
263301            DELIMITED BY SIZE INTO SSA1                                   
263401     MOVE '    '                TO GODK-STATUSKODER                       
263501     CALL CBLTDLI USING GHNP   WDE41-PCB ORAD-WDE411 SSA1                 
263601     MOVE WDE41-STATUS-CODE     TO STATUS-WS                              
263701     PERFORM IMS-STATUSKONTROLL                                           
263801     .                                                                    
263901                                                                          
264001                                                                          
264101 IMS-REPL-BEHANDLAD-RAD SECTION.                                          
264201     MOVE 'IMS-REPL-BEH-RAD'    TO CURRENT-IMS-SECTION                    
264301                                                                          
264401     MOVE SPACE                 TO ALL-SSA                                
264501     MOVE '    '                TO GODK-STATUSKODER                       
264601     CALL CBLTDLI USING REPL WDE41-PCB ORAD-WDE411                        
264701     MOVE WDE41-STATUS-CODE     TO STATUS-WS                              
264801     PERFORM IMS-STATUSKONTROLL                                           
264901     .                                                                    
265001                                                                          
265101                                                                          
265201 IMS-GHNP-WDE421         SECTION.                                         
265301     MOVE 'IMS-GHNP-WDE421 '    TO CURRENT-IMS-SECTION                    
265401                                                                          
265501     MOVE SPACE                 TO ALL-SSA                                
265601     STRING 'WDE421  *F(WDE421KY =' W-WDE421-IDKOLLI-X ')'                
265701            DELIMITED BY SIZE INTO SSA1                                   
265801     MOVE '  '                  TO GODK-STATUSKODER                       
265901     CALL CBLTDLI USING GHNP WDE41-PCB KKOLLI-WDE421 SSA1                 
266001     MOVE WDE41-STATUS-CODE     TO STATUS-WS                              
266101     PERFORM IMS-STATUSKONTROLL                                           
266201     .                                                                    
266301                                                                          
266401                                                                          
266501 IMS-REPL-WDE421       SECTION.                                           
266601     MOVE 'IMS-REPL-WDE421 '    TO CURRENT-IMS-SECTION                    
266701                                                                          
266801     MOVE SPACE                 TO ALL-SSA                                
266901     MOVE '  '                  TO GODK-STATUSKODER                       
267001     CALL CBLTDLI USING REPL WDE41-PCB KKOLLI-WDE421                      
267101     MOVE WDE41-STATUS-CODE     TO STATUS-WS                              
267201     PERFORM IMS-STATUSKONTROLL                                           
267301     .                                                                    
267401                                                                          
267501                                                                          
267601 IMS-ISRT-WDE421       SECTION.                                           
267701     MOVE 'IMS-ISRT-WDE421 '    TO CURRENT-IMS-SECTION                    
267801                                                                          
267901     MOVE SPACE                 TO ALL-SSA                                
268001     MOVE 'WDE421'              TO SSA1                                   
268101     MOVE '  II'                TO GODK-STATUSKODER                       
268201     CALL CBLTDLI USING ISRT WDE41-PCB KKOLLI-WDE421 SSA1                 
268301     MOVE WDE41-STATUS-CODE     TO STATUS-WS                              
268401     PERFORM IMS-STATUSKONTROLL                                           
268501     .                                                                    
268601                                                                          
268701                                                                          
268801 IMS-GHU-WDE601      SECTION.                                             
268901     MOVE 'IMS-GHU-WDE601  '    TO CURRENT-IMS-SECTION                    
269001                                                                          
269101     MOVE SPACE                 TO ALL-SSA                                
269201     STRING 'WDE601  (IDPRODNR =' W-WDE601-IDPRODNR-X ')'                 
269301            DELIMITED BY SIZE INTO SSA1                                   
269401     MOVE '    '                TO GODK-STATUSKODER                       
269501     CALL CBLTDLI USING GHU WDE6-PCB VORD-WDE601 SSA1                     
269601     MOVE WDE6-STATUS-CODE      TO STATUS-WS                              
269701     PERFORM IMS-STATUSKONTROLL                                           
269801     .                                                                    
269901                                                                          
270001                                                                          
270101 IMS-REPL-WDE601 SECTION.                                                 
270201     MOVE 'IMS-REPL-WDE601 '    TO CURRENT-IMS-SECTION                    
270301                                                                          
270401     MOVE SPACE                 TO ALL-SSA                                
270501     MOVE '    '                TO GODK-STATUSKODER                       
270601     CALL CBLTDLI USING REPL WDE6-PCB VORD-WDE601                         
270701     MOVE WDE6-STATUS-CODE      TO STATUS-WS                              
270801     PERFORM IMS-STATUSKONTROLL                                           
270901     .                                                                    
271001                                                                          
271101                                                                          
271201 IMS-GHU-WDE611 SECTION.                                                  
271301     MOVE 'IMS-GHU-WDE611  '    TO CURRENT-IMS-SECTION                    
271401                                                                          
271501     MOVE SPACE                 TO ALL-SSA                                
271601     STRING 'WDE601  (IDPRODNR =' W-WDE601-IDPRODNR-X ')'                 
271701            DELIMITED BY SIZE INTO SSA1                                   
271801     STRING 'WDE611  (IDKOLLI  =' W-WDE611-IDKOLLI-X ')'                  
271901            DELIMITED BY SIZE INTO SSA2                                   
272001     MOVE '  GE'                TO GODK-STATUSKODER                       
272101     CALL CBLTDLI USING GHU    WDE6-PCB KOLLI-WDE611 SSA1 SSA2            
272201     MOVE WDE6-STATUS-CODE      TO STATUS-WS                              
272301     PERFORM IMS-STATUSKONTROLL                                           
272401     .                                                                    
272501                                                                          
272601                                                                          
272701 IMS-REPL-WDE611 SECTION.                                                 
272801     MOVE 'IMS-REPL-WDE611 '    TO CURRENT-IMS-SECTION                    
272901                                                                          
273001     MOVE SPACE                 TO ALL-SSA                                
273101     MOVE '    '                TO GODK-STATUSKODER                       
273201     CALL CBLTDLI USING REPL WDE6-PCB KOLLI-WDE611                        
273301     MOVE WDE6-STATUS-CODE      TO STATUS-WS                              
273401     PERFORM IMS-STATUSKONTROLL                                           
273501     .                                                                    
273601                                                                          
273701                                                                          
273801 IMS-ISRT-WDE611  SECTION.                                                
273901     MOVE 'IMS-ISRT-WDE611 '    TO CURRENT-IMS-SECTION                    
274001                                                                          
274101     MOVE SPACE                 TO ALL-SSA                                
274201     STRING 'WDE601  (IDPRODNR =' W-WDE601-IDPRODNR-X ')'                 
274301            DELIMITED BY SIZE INTO SSA1                                   
274401     MOVE   'WDE611 '           TO   SSA2                                 
274501     MOVE '  II'                TO GODK-STATUSKODER                       
274601     CALL CBLTDLI USING ISRT WDE6-PCB KOLLI-WDE611 SSA1 SSA2              
274701     MOVE WDE6-STATUS-CODE      TO STATUS-WS                              
274801     PERFORM IMS-STATUSKONTROLL                                           
274901     .                                                                    
275001                                                                          
275101                                                                          
275201 IMS-GU-WDE4F1 SECTION.                                                   
275301     MOVE 'IMS-GU-WDE4F1   '    TO CURRENT-IMS-SECTION                    
275401                                                                          
275501     MOVE SPACE                 TO ALL-SSA                                
275601     STRING 'WDE4F1  (WDE4F1KY>=' W-WDE4F1KY-MIN-X                        
275701                    '&WDE4F1KY<=' W-WDE4F1KY-MAX-X ')'                    
275801            DELIMITED BY SIZE INTO SSA1                                   
275901     MOVE '  GE'                TO GODK-STATUSKODER                       
276001     CALL CBLTDLI USING GU  WDE4F-PCB SEQF-WDE4F1 SSA1                    
276101     MOVE WDE4F-STATUS-CODE     TO STATUS-WS                              
276201     PERFORM IMS-STATUSKONTROLL                                           
276301     .                                                                    
276401                                                                          
276501                                                                          
276601 IMS-GN-WDE4F1 SECTION.                                                   
276701     MOVE 'IMS-GN-WDE4F1   '    TO CURRENT-IMS-SECTION                    
276801                                                                          
276901     MOVE SPACE                 TO ALL-SSA                                
277001     STRING 'WDE4F1  (WDE4F1KY>=' W-WDE4F1KY-MIN-X                        
277101                    '&WDE4F1KY<=' W-WDE4F1KY-MAX-X ')'                    
277201            DELIMITED BY SIZE INTO SSA1                                   
277301     MOVE '  GEGB'              TO GODK-STATUSKODER                       
277401     CALL CBLTDLI USING GN  WDE4F-PCB SEQF-WDE4F1 SSA1                    
277501     MOVE WDE4F-STATUS-CODE     TO STATUS-WS                              
277601     PERFORM IMS-STATUSKONTROLL                                           
277701     .                                                                    
277801                                                                          
277901                                                                          
278001 IMS-REPL-ORQA01        SECTION.                                          
278101     MOVE 'IMS-REPL-ORQA01 '    TO CURRENT-IMS-SECTION                    
278201                                                                          
278301     MOVE SPACE                 TO ALL-SSA                                
278401     MOVE '    '                TO GODK-STATUSKODER                       
278501     CALL CBLTDLI USING REPL ORQA-PCB ODEL-WDQ301                         
278601     MOVE ORQA-STATUS-CODE      TO STATUS-WS                              
278701     PERFORM IMS-STATUSKONTROLL                                           
278801     .                                                                    
278901                                                                          
279001                                                                          
279101 IMS-GHU-ORQA01   SECTION.                                                
279201     MOVE 'IMS-GHU-ORQA01  '    TO CURRENT-IMS-SECTION                    
279301                                                                          
279401     MOVE SPACE                 TO ALL-SSA                                
279501     STRING 'WLORQA01(WDQ301KY =' W-WDQ301-KEY-X ')'                      
279601            DELIMITED BY SIZE INTO SSA1                                   
279701     MOVE '  '                  TO GODK-STATUSKODER                       
279801     CALL CBLTDLI USING GHU    ORQA-PCB ODEL-WDQ301 SSA1                  
279901     MOVE ORQA-STATUS-CODE      TO STATUS-WS                              
280001     PERFORM IMS-STATUSKONTROLL                                           
280101     .                                                                    
280201                                                                          
280301                                                                          
280401 IMS-GU-ORQI01    SECTION.                                                
280501     MOVE 'IMS-GU-ORQI01   '    TO CURRENT-IMS-SECTION                    
280601                                                                          
280701     MOVE SPACE                 TO ALL-SSA                                
280801     STRING 'WLORQI01(IDORDER  =' W-WDQ201-X ')'                          
280901            DELIMITED BY SIZE INTO SSA1                                   
281001     MOVE '  GE'                TO GODK-STATUSKODER                       
281101     CALL CBLTDLI USING GU   ORQI-PCB OHUV-WDQ201 SSA1                    
281201     MOVE ORQI-STATUS-CODE      TO STATUS-WS                              
281301     PERFORM IMS-STATUSKONTROLL                                           
281401     .                                                                    
281501                                                                          
281601                                                                          
281701 IMS-GHU-ORQI11    SECTION.                                               
281801     MOVE 'IMS-GHU-ORQI11  '    TO CURRENT-IMS-SECTION                    
281901                                                                          
282001     MOVE SPACE                 TO ALL-SSA                                
282101     STRING 'WLORQI01(IDORDER  =' W-WDQ201-X ')'                          
282201            DELIMITED BY SIZE INTO SSA1                                   
282301     STRING 'WLORQI11(WDQ211KY =' W-WDQ211-KEY-X ')'                      
282401            DELIMITED BY SIZE INTO SSA2                                   
282501     MOVE '    '                TO GODK-STATUSKODER                       
282601     CALL CBLTDLI USING GHU   ORQI-PCB DIRL-WDQ211 SSA1 SSA2              
282701     MOVE ORQI-STATUS-CODE      TO STATUS-WS                              
282801     PERFORM IMS-STATUSKONTROLL                                           
282901     .                                                                    
283001                                                                          
283101                                                                          
283201 IMS-REPL-ORQI11      SECTION.                                            
283301     MOVE 'IMS-REPL-ORQI11 '    TO CURRENT-IMS-SECTION                    
283401                                                                          
283501     MOVE SPACE                 TO ALL-SSA                                
283601     MOVE '    '                TO GODK-STATUSKODER                       
283701     CALL CBLTDLI USING REPL ORQI-PCB DIRL-WDQ211                         
283801     MOVE ORQI-STATUS-CODE      TO STATUS-WS                              
283901     PERFORM IMS-STATUSKONTROLL                                           
284001     .                                                                    
284101                                                                          
284201                                                                          
284301 IMS-GU-WDB201 SECTION.                                                   
284401     MOVE 'IMS-GU-WDB201   '    TO CURRENT-IMS-SECTION                    
284501                                                                          
284601     MOVE SPACE                 TO ALL-SSA                                
284701     STRING 'WDB201  (IDGMT    =' W-IDGMT-X ')'                           
284801            DELIMITED BY SIZE INTO SSA1                                   
284901     MOVE '  GE'                TO GODK-STATUSKODER                       
285001     CALL CBLTDLI USING GU WDB2-PCB GMT-WDB201 SSA1                       
285101     MOVE WDB2-STATUS-CODE      TO STATUS-WS                              
285201     PERFORM IMS-STATUSKONTROLL                                           
285301     .                                                                    
285401                                                                          
285501                                                                          
285601 IMS-ISRT-4322-SEGM SECTION.                                              
285701     MOVE 'IMS-ISRT-4322SEG'    TO CURRENT-IMS-SECTION                    
285801                                                                          
285901     MOVE SPACE                 TO ALL-SSA                                
286001     STRING 'WLXXJK01(WDGXKEY  =' W-4321-IDHTYP-X ')'                     
286101            DELIMITED BY SIZE INTO SSA1                                   
286201     MOVE 'WLXXJK11*L'          TO SSA2                                   
286301     MOVE '  '                  TO GODK-STATUSKODER                       
286401     CALL CBLTDLI USING ISRT XXJK-PCB 4322-WDGX4322 SSA1 SSA2             
286501     MOVE XXJK-STATUS-CODE      TO STATUS-WS                              
286601     PERFORM IMS-STATUSKONTROLL                                           
286701     .                                                                    
286801                                                                          
286901                                                                          
287001*******  WDE2 ******                                                      
287101                                                                          
287201 IMS-ISRT-WDE201 SECTION.                                                 
287301     MOVE 'IMS-ISRT-WDE201 '    TO CURRENT-IMS-SECTION                    
287401                                                                          
287501     MOVE SPACE                 TO ALL-SSA                                
287601     MOVE 'WDE201  '            TO SSA1                                   
287701     MOVE '  II'                TO GODK-STATUSKODER                       
287801     CALL CBLTDLI USING ISRT WDE2-PCB DLI-IO-WDE201 SSA1                  
287901     MOVE WDE2-STATUS-CODE      TO STATUS-WS                              
288001     PERFORM IMS-STATUSKONTROLL                                           
288101     .                                                                    
288201                                                                          
288301                                                                          
288401 IMS-ISRT-WDE211 SECTION.                                                 
288501     MOVE 'IMS-ISRT-WDE211 '    TO CURRENT-IMS-SECTION                    
288601                                                                          
288701     MOVE SPACE                 TO ALL-SSA                                
288801     STRING 'WDE201  (IDSHIPM  =' W-IDSHIPM-X ')'                         
288901            DELIMITED BY SIZE INTO SSA1                                   
289001     MOVE 'WDE211  '            TO SSA2                                   
289101     MOVE '  II'                TO GODK-STATUSKODER                       
289201     CALL CBLTDLI USING ISRT WDE2-PCB DLI-IO-WDE211 SSA1 SSA2             
289301     MOVE WDE2-STATUS-CODE      TO STATUS-WS                              
289401     PERFORM IMS-STATUSKONTROLL                                           
289501     .                                                                    
289601                                                                          
289701                                                                          
289801 IMS-ISRT-WDE221 SECTION.                                                 
289901     MOVE 'IMS-ISRT-WDE221 '    TO CURRENT-IMS-SECTION                    
290001                                                                          
290101     MOVE SPACE                 TO ALL-SSA                                
290201     STRING 'WDE201  (IDSHIPM  =' W-IDSHIPM-X ')'                         
290301            DELIMITED BY SIZE INTO SSA1                                   
290401     STRING 'WDE211  (WDE211KY =' W-WDE211KY-X ')'                        
290501            DELIMITED BY SIZE INTO SSA2                                   
290601     MOVE 'WDE221  '            TO SSA3                                   
290701     MOVE '  II'                TO GODK-STATUSKODER                       
290801     CALL CBLTDLI USING ISRT WDE2-PCB DLI-IO-WDE221 SSA1 SSA2 SSA3        
290901     MOVE WDE2-STATUS-CODE TO STATUS-WS                                   
291001     PERFORM IMS-STATUSKONTROLL                                           
291101     .                                                                    
291201                                                                          
291301                                                                          
291401*******  WDE1 ******                                                      
291501                                                                          
291601 IMS-ISRT-WDE101 SECTION.                                                 
291701     MOVE 'IMS-ISRT-WDE101 '    TO CURRENT-IMS-SECTION                    
291801                                                                          
291901     MOVE SPACE                 TO ALL-SSA                                
292001     MOVE 'WDE101  '            TO SSA1                                   
292101     MOVE '  II'                TO GODK-STATUSKODER                       
292201     CALL CBLTDLI USING ISRT WDE1-PCB DLI-IO-WDE101 SSA1                  
292301     MOVE WDE1-STATUS-CODE      TO STATUS-WS                              
292401     PERFORM IMS-STATUSKONTROLL                                           
292501     .                                                                    
292601                                                                          
292701                                                                          
292801 IMS-ISRT-WDE111 SECTION.                                                 
292901     MOVE 'IMS-ISRT-WDE111 '    TO CURRENT-IMS-SECTION                    
293001                                                                          
293101     MOVE SPACE                 TO ALL-SSA                                
293201     STRING 'WDE101  (IDSHIPM  =' W-IDSHIPM-X ')'                         
293301            DELIMITED BY SIZE INTO SSA1                                   
293401     MOVE 'WDE111  '            TO SSA2                                   
293501     MOVE '  II'                TO GODK-STATUSKODER                       
293601     CALL CBLTDLI USING ISRT WDE1-PCB DLI-IO-WDE111 SSA1 SSA2             
293701     MOVE WDE1-STATUS-CODE      TO STATUS-WS                              
293801     PERFORM IMS-STATUSKONTROLL                                           
293901     .                                                                    
294001                                                                          
294101                                                                          
294201 IMS-ISRT-WDE121 SECTION.                                                 
294301     MOVE 'IMS-ISRT-WDE121 '    TO CURRENT-IMS-SECTION                    
294401                                                                          
294501     MOVE SPACE                 TO ALL-SSA                                
294601     STRING 'WDE101  (IDSHIPM  =' W-IDSHIPM-X ')'                         
294701            DELIMITED BY SIZE INTO SSA1                                   
294801     STRING 'WDE111  (WDE111KY =' W-WDE111KY-X ')'                        
294901            DELIMITED BY SIZE INTO SSA2                                   
295001     MOVE 'WDE121  '            TO SSA3                                   
295101     MOVE '  II'                TO GODK-STATUSKODER                       
295201     CALL CBLTDLI USING ISRT WDE1-PCB DLI-IO-WDE121 SSA1 SSA2 SSA3        
295301     MOVE WDE1-STATUS-CODE      TO STATUS-WS                              
295401     PERFORM IMS-STATUSKONTROLL                                           
295501     .                                                                    
295601                                                                          
295701                                                                          
295801 IMS-GU-WDB601    SECTION.                                                
295901     MOVE 'IMS-GU-WDB601   '    TO CURRENT-IMS-SECTION                    
296001                                                                          
296101     MOVE SPACE                 TO ALL-SSA                                
296201     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
296301            DELIMITED BY SIZE INTO SSA1                                   
296401     MOVE '    '                TO GODK-STATUSKODER                       
296501     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
296601     MOVE WDB6-STATUS-CODE      TO STATUS-WS                              
296701     PERFORM IMS-STATUSKONTROLL                                           
296801     IF SEGMENT-SAKNAS                                                    
296901        MOVE SPACE TO DCS-KDDC                                            
297001     END-IF                                                               
297101     .                                                                    
297201                                                                          
297301                                                                          
297401 IMS-GHU-WDF411 SECTION.                                                  
297501     MOVE 'IMS-GHU-WDF411  ' TO CURRENT-IMS-SECTION                       
297601                                                                          
297701     MOVE SPACE               TO ALL-SSA                                  
297801     STRING 'WDF401  (IDLEVNR  =' W-IDLEVNR-F4-X ')'                      
297901          DELIMITED BY SIZE INTO SSA1                                     
298001     STRING 'WDF411  (WDF411KY>=' W-WDF411KY-MIN-X                        
298101                    '&WDF411KY<=' W-WDF411KY-MAX-X ')'                    
298201          DELIMITED BY SIZE INTO SSA2                                     
298301     MOVE '  GE'              TO GODK-STATUSKODER                         
298401     CALL CBLTDLI USING GHU WDF4-PCB DLI-IO-F411 SSA1 SSA2                
298501     MOVE WDF4-STATUS-CODE    TO STATUS-WS                                
298601     PERFORM IMS-STATUSKONTROLL                                           
298701     .                                                                    
298801                                                                          
298901                                                                          
299001 IMS-REPL-WDF411 SECTION.                                                 
299101     MOVE 'IMS-REPL-WDF411 ' TO CURRENT-IMS-SECTION                       
299201                                                                          
299301     MOVE SPACE              TO ALL-SSA                                   
299401     MOVE '  '               TO GODK-STATUSKODER                          
299501     CALL CBLTDLI USING REPL WDF4-PCB DLI-IO-F411                         
299601     MOVE WDF4-STATUS-CODE   TO STATUS-WS                                 
299701     PERFORM IMS-STATUSKONTROLL                                           
299801     .                                                                    
299901                                                                          
300001                                                                          
300101 IMS-GU-WDGX4503-04       SECTION.                                        
300201                                                                          
300301     STRING 'WDR401  (WDGXKEY  =' W-4503-WDGXKEY-X ')'                    
300401         DELIMITED BY SIZE INTO SSA1                                      
300501     STRING 'WDGX4504(KY4504   =' W-4504-WDGXKEY-X ')'                    
300601         DELIMITED BY SIZE INTO SSA2                                      
300701     MOVE '  GE' TO GODK-STATUSKODER                                      
300801     CALL CBLTDLI USING GU 4503-PCB DLI-IO-4504 SSA1 SSA2                 
300901     MOVE 4503-STATUS-CODE TO STATUS-WS                                   
301001     PERFORM IMS-STATUSKONTROLL                                           
301101     .                                                                    
301201                                                                          
301301                                                                          
301401 IMS-ISRT-WDGX4503-04       SECTION.                                      
301501                                                                          
301601     STRING 'WDR401  (WDGXKEY  =' W-4503-WDGXKEY-X ')'                    
301701         DELIMITED BY SIZE INTO SSA1                                      
301801     MOVE 'WDGX4504'       TO   SSA2                                      
301901     MOVE '  GE' TO GODK-STATUSKODER                                      
302001     CALL CBLTDLI USING ISRT 4503-PCB DLI-IO-4504 SSA1 SSA2               
302101     MOVE 4503-STATUS-CODE TO STATUS-WS                                   
302201     PERFORM IMS-STATUSKONTROLL                                           
302301     .                                                                    
302401                                                                          
302501 IMS-GU-WDK601  SECTION.                                                  
302601     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
302701          DELIMITED BY SIZE INTO SSA1                                     
302801     MOVE '  GE' TO GODK-STATUSKODER                                      
302901     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
303001     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
303101     PERFORM IMS-STATUSKONTROLL                                           
303201     .                                                                    
303301     SKIP3                                                                
303401                                                                          
303501 IMS-GNP-WDK611  SECTION.                                                 
303601     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
303701          DELIMITED BY SIZE INTO SSA1                                     
303801     MOVE '  GE' TO GODK-STATUSKODER                                      
303901     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK611 SSA1                   
304001     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
304101     PERFORM IMS-STATUSKONTROLL                                           
304201     .                                                                    
304301     SKIP3                                                                
304401                                                                          
304501                                                                          
304601 IMS-STATUSKONTROLL SECTION.                                              
304701     SET STATUS-IX TO 1                                                   
304801     SEARCH GODK-STATUS AT END CALL FELLOG                                
304901       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
305001     END-SEARCH                                                           
305101     CONTINUE                                                             
306001     .                                                                    
