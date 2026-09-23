000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W3712300.                                                
000400 AUTHOR.         INGVAR SKJELBRÖD.                                        
000500 DATE-WRITTEN.   96/09/09.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800                                                                          
000900*    FUNKTION:                                                            
001000*       .UPPDATERAR LAGERSALDO PÅ WDK6/WDK7                               
001100*       .SKAPAR EKONOMITRANSAR TILL W510 PÅ WDR9 (PEDAL)                  
001200*       .SKAPAR EKONOMITRANSAR TILL W560 PÅ WDR8 (LAB)                    
001300*       .SKAPAR EKONOMITRANSAR TILL W570 PÅ WDR8 (PEDAL2)                 
001400*       .UPPDATERAR WDL9 (SOL)                                            
001500*       .SKAPAR SKROTORDER VIA DISPATCHEN                                 
001600*       .UPPDATERAR WDP8                                                  
001700*       .LÄSER WDR1 (ORDERNR)                                             
001800*                                                                         
001900*    ABENDKODER:                                                          
002000*        U0016 -  . . . .                                                 
002100*        U1000 -  . . . .                                                 
002200*                                                                         
002300* 060211/ETRACKER 2816396 ANPASSA FÖR ATT MAN REDAN LAGT UPP ART          
002400*        PÅ WDK701/11 I BILD 3171+3172.                                   
002500*                                                                         
002600* 070625/E'TRACKER 5230526 RÄTTA FEL AV CHKP SOM GÖR ATT SKROT-           
002700*                          ORDRAR IBLAND TAPPAR RADER.                    
002800*                                                                         
002900* 070627/E'TRACKER 5242928 ÖKA ANTAL POS. AV CHKP-ANT.                    
003000*                                                                         
003100 ENVIRONMENT DIVISION.                                                    
003200                                                                          
003300 INPUT-OUTPUT SECTION.                                                    
003400                                                                          
003500 FILE-CONTROL.                                                            
003600                                                                          
003700*          --- BYTESRAPPORTER                                             
003800     SELECT W37104                     ASSIGN TO W37123D1.                
003900                                                                          
004000*          --- INFIL KVITTNINGSTABELL                                     
004100     SELECT W37181                     ASSIGN TO W37123D2.                
004200     EJECT                                                                
004300                                                                          
004400 DATA DIVISION.                                                           
004500                                                                          
004600 FILE SECTION.                                                            
004700                                                                          
004800 FD  W37104                                                               
004900     RECORDING       F                                                    
005000     BLOCK CONTAINS  0.                                                   
005100                                                                          
005200*01  -COPY W37104      -L.                                                
005300     EJECT                                                                
005400                                                                          
005500 FD  W37181                                                               
005600     RECORDING      F                                                     
005700     BLOCK CONTAINS 0.                                                    
005800     SKIP2                                                                
005900*01  POST -COPY W37181  -PRE  KVITT-  -L.                                 
006000     EJECT                                                                
006100                                                                          
006200 WORKING-STORAGE SECTION.                                                 
006300                                                                          
006400*    -- CHECKED BY WY2000                                                 
006500     SKIP3                                                                
006600 77  IDPGM                       PIC X(8)    VALUE 'W3712300'.            
006700 77  JA                          PIC X       VALUE 'J'.                   
006800 77  NEJ                         PIC X       VALUE 'N'.                   
006900                                                                          
007000 77  WS-SDC-91                   PIC X(2)    VALUE '91'.                  
007100 77  WS-CDC-11                   PIC X(2)    VALUE '11'.                  
007200                                                                          
007300 77  IX                          PIC S9(7)  COMP-3 VALUE ZERO.            
007400 77  IX2                         PIC S9(7)  COMP-3 VALUE ZERO.            
007500 77  ORAD-IX                     PIC S9(4)  VALUE +0    COMP SYNC.        
007600 77  ORAD-IX-MAX                 PIC S9(4)  VALUE +5    COMP SYNC.        
007700 77  LNG-P-TO-P-PREFIX           PIC S9(4)  VALUE +17   COMP SYNC.        
007800                                                                          
007900 01  SPAR-IDDISTR                PIC 9(5)   VALUE ZERO.                   
008000 01  SPAR-IDKUNDNR               PIC 9(7)   VALUE ZERO.                   
008100 01  SPAR-IDBYTRAP               PIC 9(7)   VALUE ZERO.                   
008200 01  W-TIKLOCK                   PIC 9(8)   VALUE ZERO.                   
008300 01  W-KVRETUR-GODK-6            PIC 9(6)   VALUE ZERO.                   
008400 01  W-IDKUNDNR-X.                                                        
008500     03 FILLER           PIC 9(4)           VALUE ZERO.                   
008600     03 W-IDKUNDNR-2     PIC 9(2).                                        
008700                                                                          
008800 01  W-IDORDNR-X.                                                         
008900     03  FILLER          PIC 9(2).                                        
009000     03 W-IDORDNR.                                                        
009100       07  W-IDORDNR-VV    PIC 9(2).                                      
009200       07  W-IDORDNR-LLL   PIC 9(3).                                      
009201                                                                          
009210 01  W-DATE-AAMM                  PIC 9(4)    VALUE ZERO.                 
009220 77  WS-KDVALISO-HUV              PIC X(3)    VALUE 'SEK'.                
009300                                                                          
009310 01  W-PRKURS                PIC S9(2)V9(5) VALUE +0   COMP-3.            
009320 01  WS-SLAG-PRAVCOST        PIC S9(7)V9(2) VALUE +0   COMP-3.            
009330                                                                          
009400 01  KONTROLL-SIFFRA.                                                     
009500     03  REK-IDARTNR             PIC 9(9)    VALUE 0.                     
009600     03  REK-LNGD                PIC 9(1)    VALUE 9.                     
009700     03  REK-REKSIFFR            PIC 9(1)    VALUE 0.                     
009800                                                                          
009900 01  CHKP-VAR.                                                            
010000 03  CHKP-MSG-IO-AREA-LENGTH PIC S9(9)   VALUE +32 COMP SYNC.             
010100 03  CHKP-MSG-IO-AREA        PIC X(32)   VALUE SPACE.                     
010200 03  CHKP-AREA-LENGTH        PIC S9(9)   VALUE +32 COMP SYNC.             
010300 03  CHKP-AREA               PIC X(32)   VALUE SPACE.                     
010400 03  CHKP-ANT                PIC S9(7)   VALUE +0.                        
010500 03  CHKP-MAX                PIC S9(7)   VALUE +2.                        
010600 03  CHKP-TOT                PIC S9(7)   VALUE ZERO.                      
010700                                                                          
010800 01  FELTEXT.                                                             
010900     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
011000     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
011100                                                                          
011200**************************************************************            
011300 77  WS-IDTABNR                  PIC S9(3)   VALUE ZERO.                  
011400 77  SOK-SW                      PIC X(1)    VALUE 'N'.                   
011500     88 SOK-OK                               VALUE 'J'.                   
011600 77  SKROT-SW                    PIC X(1)    VALUE 'N'.                   
011700     88 SKROTORDER-SKAPAD                    VALUE 'J'.                   
011800 77  ORDERAVSLUT-SW              PIC X(1)    VALUE 'N'.                   
011900     88 ORDER-AVSLUTAD                       VALUE 'J'.                   
012000 77  OMSTART-SW                  PIC X(1)    VALUE 'N'.                   
012100     88 OMSTART                              VALUE 'J'.                   
012200**************************************************************            
012300 77  W-KVPOST-3144               PIC S9(7)   VALUE ZERO COMP-3.           
012400 77  INDX                        PIC S9(9)   VALUE ZERO COMP SYNC.        
012500 77  WS-INDX                     PIC  9(3).                               
012600 77  WS-AAAAMMDD                 PIC 9(8)    VALUE ZERO.                  
012700 77  WS-TTMMSSTH                 PIC 9(8)    VALUE ZERO.                  
012800**************************************************************            
012900                                                                          
013000 77  W37104-EOF-SW               PIC X       VALUE 'N'.                   
013100     88  END-OF-W37104                       VALUE 'J'.                   
013200 77  W37181-EOF-SW               PIC X       VALUE 'N'.                   
013300     88  END-OF-W37181                       VALUE 'J'.                   
013400                                                                          
013500     EJECT                                                                
013600 01  W-IDDCTEXT-MSGI.                                                     
013700     03  FILLER              PIC X(5)   VALUE 'WIDDC'.                    
013800     03  W-IDDC-MSGI         PIC X(2).                                    
013900                                                                          
014000 01  W-TILLOKDAT                 PIC 9(6)    VALUE ZERO.                  
014100                                                                          
014200 01  WS-TILLOKDAT.                                                        
014300     03  SEKEL-TILLOKDAT         PIC 9(2)    VALUE ZERO.                  
014400     03  DATE-TILLOKDAT          PIC 9(6)    VALUE ZERO.                  
014500                                                                          
014600 01  DAGENS-KLOCKSLAG            PIC 9(8)    VALUE ZERO.                  
014700 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
014800 01  FILLER REDEFINES DAGENS-DATUM.                                       
014900     03  DAGENS-DATUM-AAR        PIC 9(2).                                
015000     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
015100     03  DAGENS-DATUM-DAG        PIC 9(2).                                
015200     EJECT                                                                
015300 01  TRANS-TID                   PIC 9(9).                                
015400 01  LOG-DAGENS-DATUM            PIC 9(8).                                
015500 01  WS-DAT-TIVV-ORDER           PIC 9(2).                                
015600                                                                          
015700**************************************************************            
015800 01  WLOGG-TID                   PIC S9(9) VALUE ZERO.                    
015900 01  LOGG-DATUM                  PIC S9(8) VALUE ZERO.                    
016000 01  TABELL.                                                              
016100  03  KVITT-IX-MAX               PIC S9(7) COMP-3.                        
016200     03  KVITT-TAB OCCURS 1 TO 5000 DEPENDING ON KVITT-IX-MAX             
016300                                    INDEXED BY KVITT-IX.                  
016400       05  TAB-IDARTNR           PIC S9(9) COMP-3.                        
016500       05  TAB-IDTABNR           PIC S9(3) COMP-3.                        
016600                                                                          
016700 01  FILLER                      PIC X(16) VALUE 'SAMLINGSTAB**'.         
016800*01  -COPY WWBYT17.                                                       
016900     EJECT                                                                
017000*01  -COPY WWPRODSL                                                       
017100     EJECT                                                                
017200**************************************************************            
017300 01  DIV-VAR.                                                             
017400     03  WS-IDLKTO              PIC S9(7) COMP-3 VALUE ZERO.              
017500     03  WS-PRARTSTD            PIC S9(7)V9(2) COMP-3 VALUE ZERO.         
017600**************************************************************            
017700     EJECT                                                                
017800                                                                          
017900 01  DYNAMISKA-SUBPROGRAM.                                                
018000*                                                                         
018100     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
018200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
018300     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
018400     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
018500     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
018600     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
018700     03  W006KOM                 PIC X(8)    VALUE 'W006KOM '.            
018800     03  W009CIA                 PIC X(8)    VALUE 'W009CIA '.            
018900     03  W009KSIF                PIC X(8)    VALUE 'W009KSIF'.            
019000     03  W005WDK7                PIC X(8)    VALUE 'W005WDK7'.            
019010     03  W510CURR                PIC X(8)    VALUE 'W510CURR'.            
019100     EJECT                                                                
019200 01 FILLER                       PIC X(8)    VALUE 'W005WDK7'.            
019300*   -COPY W005WDK7                                                        
019400     EJECT                                                                
019410*   -COPY W510CURR                                                        
019500     EJECT                                                                
019600*- - - - - - - - - - - - - -  PARAMETRAR TILL ABEND                       
019700 01  RETURKODER.                                                          
019800   03  RKOD                      PIC S9(4)  COMP SYNC VALUE ZERO.         
019900   03  RKOD-ABEND-UTAN-DUMP      PIC S9(4)  COMP SYNC VALUE +16.          
020000   03  RKOD-ABEND-MED-DUMP       PIC S9(4)  COMP SYNC VALUE +1000.        
020100*- - - - - - - - - - - - - -  PARAMETRAR TILL WDATKONV                    
020200 01  FILLER                      PIC X(16)   VALUE 'WDATAREA'.            
020300     SKIP2                                                                
020400*01  -COPY WDATAREA.                                                      
020500     EJECT                                                                
020600*    --- PARAMETERS TILL POSTSUM                                          
020700*                                                                         
020800*01  -COPY W0005   -PRE  POSTSUM-                                         
020900     EJECT                                                                
021000*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
021100 01  FILLER                      PIC X(16)  VALUE 'WMSGINIT '.            
021200*01 -COPY WMSGINIT                                                        
021300     EJECT                                                                
021400                                                                          
021500*    --- PARAMETRAR TILL W009CIA                                          
021600*01  -COPY W009CIA                                                        
021700     EJECT                                                                
021800                                                                          
021900 01  TEST-KDBYTREF               PIC X(3).                                
022000*01  FILLER -COPY WWBYT15   -RED TEST-KDBYTREF                            
022100     EJECT                                                                
022200                                                                          
022300 01  FILLER                   PIC X(16) VALUE 'KVITT-AREA*****'.          
022400                                                                          
022500*01  AREA                   -PRE KVITT- -COPY W37181                      
022600     EJECT                                                                
022700                                                                          
022800 01  INFIL-AREA-START            PIC X(24)   VALUE                        
022900                                             'INFIL-AREA-START'.          
023000                                                                          
023100*01  AREA -COPY W37104      -PRE INFIL-                                   
023200     EJECT                                                                
023300                                                                          
023400 01  UTFIL-AREA-START            PIC X(24)   VALUE                        
023500                                             'UTFIL-AREA-START'.          
023600                                                                          
023700*01  AREA -COPY W510A14     -PRE A14-                                     
023800     EJECT                                                                
023900                                                                          
024000 01  FILLER                      PIC X(16)   VALUE 'MSG-IO-AREA'.         
024100     SKIP3                                                                
024200*01  -COPY WMSGAREA                                                       
024300     EJECT                                                                
024400     05 FILLER REDEFINES MSG-MID-OUT.                                     
024500*      07  -COPY W4I25101   -PRE 4251-                                    
024600     EJECT                                                                
024700     05 FILLER REDEFINES MSG-MID-OUT.                                     
024800*      07  -COPY W4I25201   -PRE 4252-                                    
024900     EJECT                                                                
025000 01  FILLER                      PIC X(16)  VALUE 'KOM-IO-AREA'.          
025100     SKIP3                                                                
025200 01  KOM-IO-AREA.                                                         
025300*03  -COPY WMSGKOM                                                        
025400     EJECT                                                                
025500 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
025600     SKIP3                                                                
025700 01  NYCKLAR-TILL-DLI.                                                    
025800     03  W-WDGXKEY-X.                                                     
025900         05  FILLER          PIC X(4)   VALUE '3143'.                     
026000         05  FILLER          PIC X(26)  VALUE LOW-VALUE.                  
026100     03  W-IDARTNR-X.                                                     
026200         05  W-IDARTNR       PIC S9(9)  VALUE ZERO COMP-3.                
026300     03  W-DATREG-X.                                                      
026400         05  W-DATREG        PIC S9(18) VALUE ZERO COMP-3.                
026500     03  W-IDDC-X.                                                        
026600         05 W-IDDC           PIC X(2).                                    
026700     03  W-IDDC-B6-X.                                                     
026800         05 W-IDDC-B6                  PIC X(2).                          
026900     03  W-WDGXKEY-4111-X.                                                
027000         05  FILLER              PIC  X(4)   VALUE '4111'.                
027100         05  W-IDRT-4111         PIC  X(3)   VALUE 'CDC'.                 
027200         05  FILLER              PIC  X(23)  VALUE LOW-VALUE.             
027290     03  W-WDB101KY-X.                                                    
027291         05  W-WDB1-IDPARTNR      PIC X(9)    VALUE SPACE.                
027292         05  W-WDB1-IDFTG         PIC 9(2)    VALUE ZERO.                 
027300                                                                          
027400     SKIP2                                                                
027500*    --- STATUS-KOD FRÅN IMS                                              
027600 01  STATUS-WS                   PIC XX.                                  
027700     88  SEGMENT-FINNS                       VALUE '  '.                  
027800     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
027900     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
028000     88  SEGMENT-SLUT                        VALUE 'GB'.                  
028100     88  IMS-EJ-OK                           VALUE 'XD'.                  
028200     SKIP2                                                                
028300 01  GODK-STATUSKODER.                                                    
028400     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
028500     SKIP3                                                                
028600 01  SSA1                        PIC X(128).                              
028700 01  SSA2                        PIC X(128).                              
028710 01  SSA3                        PIC X(128).                              
028800     EJECT                                                                
028900*    --- IMS FUNKTIONSKODER                                               
029000*01  -COPY W0003                                                          
029100     EJECT                                                                
029200*    ---  DLI INPUT-OUTPUT AREA                                           
029300 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-K601'.         
029400     SKIP3                                                                
029500 01  DLI-IO-K601.                                                         
029600     03  IO-K601.                                                         
029700*        05  -COPY WDK601  -PRE ARTC-                                     
029800     SKIP3                                                                
029900 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-K611'.         
030000 01  DLI-IO-K611.                                                         
030100     03  IO-K611.                                                         
030200*        05  -COPY WDK611  -PRE ARTC-                                     
030300     EJECT                                                                
030400                                                                          
030500 01  DLI-IO-WDK711.                                                       
030600*    03 WDK711 -COPY WDK711                                               
030700     EJECT                                                                
030800 01  FILLER                   PIC X(16)  VALUE 'DLI-IO-WDGX4112 '.        
030900 01  DLI-IO-WDGX4112.                                                     
031000*    03  -COPY WDGX4112                                                   
031100     SKIP2                                                                
031200                                                                          
031300 01  FILLER                      PIC X(16) VALUE 'WLLOGA01'.              
031400*01  WLLOGA01    -COPY WDL901                                             
031500     EJECT                                                                
031600                                                                          
031700 01  FILLER                  PIC X(16) VALUE 'CHKP-AREA  '.               
031800     SKIP2                                                                
031900 01  DLI-IO-CHKP.                                                         
032000   03  XXCU-AREA               PIC X(600).                                
032100                                                                          
032200*  03  WLXXCU01 -COPY WDGX01 -PRE XXCU-  -RED XXCU-AREA.                  
032300                                                                          
032400*  03  WLXXCU11 -COPY WDGX3144 -PRE XXCU-  -RED XXCU-AREA.                
032500     EJECT                                                                
032600                                                                          
032700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDR801'.                      
032800 01  DLI-IO-WDR801.                                                       
032900*    03  -COPY WDR801 -PRE EKO-                                           
033000       05  FILLER REDEFINES EKO-FIL-WDR801-DATA.                          
033100         07 -COPY W510EKHA -PRE EKO-                                      
033200     EJECT                                                                
033300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDR901'.                      
033400 01  DLI-IO-WDR901.                                                       
033500*    03   -COPY WDR901                                                    
033600       05 -COPY W510EKHA -RED FIL-WDR901-DATA                             
033700     EJECT                                                                
033800 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
033900 01   DLI-IO-AREA-B601.                                                   
034000*     03  -COPY WDB601                                                    
034131                                                                          
034132 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLBETC01'.                    
034133 01  DLI-IO-WLBETC01.                                                     
034134*    03  -COPY WDB101                                                     
034135     EJECT                                                                
034140*                                                                         
034200 LINKAGE SECTION.                                                         
034300                                                                          
034400*01  -COPY W0009   -PRE MSG-                                              
034500     SKIP3                                                                
034600*01  -COPY W0008   -PRE DISP-                                             
034700     05  FILLER                PIC X.                                     
034800     EJECT                                                                
034900*01  -COPY W0008   -PRE KOMA-                                             
035000     05  FILLER                PIC X.                                     
035100     EJECT                                                                
035200                                                                          
035300*01  -COPY W0008   -PRE XXCU-                                             
035400     05  FILLER                PIC X.                                     
035500     EJECT                                                                
035600                                                                          
035700*01  -COPY W0009   -PRE USEA-                                             
035800     EJECT                                                                
035900                                                                          
036000*01  -COPY W0008  -PRE ARTC-                                              
036100     05  FILLER                  PIC X.                                   
036200     EJECT                                                                
036300*01  -COPY W0008  -PRE WDK7-                                              
036400     05  FILLER                  PIC X.                                   
036500     EJECT                                                                
036600*01  -COPY W0008  -PRE GMTA-                                              
036700     05  FILLER                  PIC X.                                   
036800     EJECT                                                                
036900*01  -COPY W0008  -PRE BETA-                                              
037000     05  FILLER                  PIC X.                                   
037100     EJECT                                                                
037200*01  -COPY W0008  -PRE GPRIA-                                             
037300     05  FILLER                  PIC X.                                   
037400     EJECT                                                                
037500*01  -COPY W0008  -PRE GPRIB-                                             
037600     05  FILLER                  PIC X.                                   
037700     EJECT                                                                
037800*01  -COPY W0008  -PRE LOGA-                                              
037900     05  FILLER                  PIC X.                                   
038000     EJECT                                                                
038100*01  -COPY W0008  -PRE WDR8-                                              
038200     05  FILLER                  PIC X.                                   
038300     EJECT                                                                
038400*01  -COPY W0008  -PRE WDR9-                                              
038500     05  FILLER                  PIC X.                                   
038600     EJECT                                                                
038700*01  -COPY W0008  -PRE WDB6-                                              
038800     05  FILLER                  PIC X.                                   
038900     EJECT                                                                
039000*01  -COPY W0008  -PRE 4111-                                              
039100     05  FILLER                  PIC X.                                   
039200     EJECT                                                                
039310*01  -COPY W0008  -PRE 9305-                                              
039320     05  FILLER                  PIC X.                                   
039330     EJECT                                                                
039331*01  -COPY W0008  -PRE BETC-                                              
039332     05  FILLER                  PIC X.                                   
039333     EJECT                                                                
039340                                                                          
039400 PROCEDURE DIVISION  USING MSG-PCB   DISP-PCB  KOMA-PCB                   
039500                           XXCU-PCB  USEA-PCB                             
039600                           ARTC-PCB  WDK7-PCB                             
039700                           GMTA-PCB  BETA-PCB                             
039800                           GPRIA-PCB GPRIB-PCB LOGA-PCB                   
039900                           WDR8-PCB  WDR9-PCB  WDB6-PCB                   
040000                           4111-PCB  9305-PCB BETC-PCB.                   
040100 MAIN SECTION.                                                            
040200     ENTRY 'DLITCBL' USING MSG-PCB   DISP-PCB  KOMA-PCB                   
040300                           XXCU-PCB  USEA-PCB                             
040400                           ARTC-PCB  WDK7-PCB                             
040500                           GMTA-PCB  BETA-PCB                             
040600                           GPRIA-PCB GPRIB-PCB LOGA-PCB                   
040700                           WDR8-PCB  WDR9-PCB  WDB6-PCB                   
040800                           4111-PCB  9305-PCB BETC-PCB.                   
040900                                                                          
041000     PERFORM A-INIT                                                       
041100                                                                          
041200     IF OMSTART                                                           
041300       CONTINUE                                                           
041400     ELSE                                                                 
041500       PERFORM S01-LAES-W37104                                            
041600     END-IF                                                               
041700                                                                          
041800     PERFORM UNTIL END-OF-W37104                                          
041900                                                                          
042000        ADD +1 TO CHKP-ANT                                                
042100        PERFORM B-BEARBETA                                                
042200                                                                          
042300        PERFORM S01-LAES-W37104                                           
042400     END-PERFORM                                                          
042500                                                                          
042600     PERFORM Z-FINIT                                                      
042700                                                                          
042800     MOVE ZERO TO RETURN-CODE                                             
042900     GOBACK                                                               
043000     .                                                                    
043100     EJECT                                                                
043200                                                                          
043300 A-INIT SECTION.                                                          
043400     ACCEPT DAGENS-DATUM     FROM DATE                                    
043500     ACCEPT DAGENS-KLOCKSLAG FROM TIME                                    
043600     MOVE DAGENS-KLOCKSLAG   TO W-TIKLOCK                                 
043700                                                                          
043800     PERFORM IMS-RESTART                                                  
043900                                                                          
044000     OPEN INPUT  W37104                                                   
044100                                                                          
044200     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
044300                                                                          
044400     MOVE DAGENS-DATUM   TO DAT-I-TIDATUM                                 
044500     MOVE 'AAMMDD'       TO DAT-KDDATFORM                                 
044600     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
044700                         DAT-O-TIDATUM DAT-KDSVAR                         
044800                                                                          
044900     MOVE DAT-TIVV       TO WS-DAT-TIVV-ORDER                             
045000                                                                          
045100     IF DAT-KDSVAR-FEL                                                    
045200        MOVE 'FEL FRÅN DATKONV' TO FELTEXT                                
045300        CALL ABEND USING RKOD-ABEND-UTAN-DUMP                             
045400     END-IF                                                               
045500                                                                          
045600     MOVE NEJ               TO OMSTART-SW                                 
045700     PERFORM AA-KOLLA-CHECKPOINT                                          
045800                                                                          
045900*    KVITTNINGSTABELL BYGGS                                               
046000     OPEN INPUT W37181                                                    
046100                                                                          
046200     MOVE ZERO TO KVITT-IX-MAX                                            
046300     SET KVITT-IX TO +1                                                   
046400                                                                          
046500     PERFORM S02-LAS-W37181                                               
046600                                                                          
046700     PERFORM UNTIL                                                        
046800            END-OF-W37181                                                 
046900       ADD +1 TO KVITT-IX-MAX                                             
047000       IF KVITT-IX-MAX < 5000                                             
047100         MOVE KVITT-IDARTNR TO TAB-IDARTNR(KVITT-IX)                      
047200         MOVE KVITT-IDTABNR TO TAB-IDTABNR(KVITT-IX)                      
047300         SET KVITT-IX UP BY +1                                            
047400       ELSE                                                               
047500         DISPLAY 'KVITTNINGSTABELL FYLLD'                                 
047600         CALL ABEND USING RKOD-ABEND-UTAN-DUMP                            
047700       END-IF                                                             
047800       PERFORM S02-LAS-W37181                                             
047900     END-PERFORM                                                          
048000                                                                          
048100     MOVE ALL '+'                     TO 4252-MID-W4I25201                
048200                                                                          
048300     CLOSE W37181                                                         
048400     .                                                                    
048500     EJECT                                                                
048600                                                                          
048700 AA-KOLLA-CHECKPOINT SECTION.                                             
048800     PERFORM IMS-GET-XXCU-ROT                                             
048900     PERFORM IMS-GET-XXCU-SEG                                             
049000*----DET FÖRSTA SEGMENTET ----------------------------------------        
049100     IF SEGMENT-FINNS                                                     
049200       MOVE XXCU-3144-KVPOST     TO W-KVPOST-3144                         
049300       IF W-KVPOST-3144       = ZERO                                      
049400          CONTINUE                                                        
049500       ELSE                                                               
049600*---------PGM HAR ABENDAT OCH SKA NU OMSTARTAS                            
049700          PERFORM AAA-FELHANTERING-INFIL                                  
049800          MOVE JA                TO OMSTART-SW                            
049900          COMPUTE CHKP-ANT = XXCU-3144-KVPOST - 1                         
050000       END-IF                                                             
050100     END-IF                                                               
050200     .                                                                    
050300     EJECT                                                                
050400                                                                          
050500 AAA-FELHANTERING-INFIL  SECTION.                                         
050600     MOVE +0 TO INDX                                                      
050700                                                                          
050800     PERFORM UNTIL INDX = W-KVPOST-3144                                   
050900       PERFORM S01-LAES-W37104                                            
051000       ADD +1 TO INDX                                                     
051100     END-PERFORM                                                          
051200     .                                                                    
051300     EJECT                                                                
051400                                                                          
051500 B-BEARBETA SECTION.                                                      
051600     MOVE INFIL-IDDC          TO W-IDDC                                   
051700                                 W-IDDC-B6                                
051800     PERFORM IMS-GU-WDB601                                                
051900     MOVE INFIL-IDARTNR-OBJ   TO W-IDARTNR                                
052000                                                                          
052100     IF SKROTORDER-SKAPAD                                                 
052200       IF INFIL-IDDISTR = SPAR-IDDISTR AND                                
052300          INFIL-IDKUNDNR = SPAR-IDKUNDNR AND                              
052400          INFIL-IDBYTRAP = SPAR-IDBYTRAP                                  
052500              CONTINUE                                                    
052600       ELSE                                                               
052700         PERFORM S22-AVSLUTA-ORDERRADTRANS                                
052800         MOVE NEJ TO SKROT-SW                                             
052900       END-IF                                                             
053000     END-IF                                                               
053100                                                                          
053200     IF DCS-NDC-NA                                                        
053300*    DIVERSE SALDOUPPDATERINGAR                                           
053400*    SKAPA LABTRANSAKTION                                                 
053500        PERFORM BA-BEARBETA-W560                                          
053600     ELSE                                                                 
053700                                                                          
053800        MOVE INFIL-KDBYTREF      TO TEST-KDBYTREF                         
053900        IF NOT BYT15-KDBYTREF-GAR-EJ-SALD                                 
054000           PERFORM IMS-GET-ARTC-WLARTC01                                  
054100           IF SEGMENT-FINNS                                               
054200             PERFORM IMS-GET-ARTC-WLARTC11                                
054300           END-IF                                                         
054400           MOVE ARTC-ART-KDPRODSL                                         
054500                                 TO TEST-KDPRODSL                         
054600           IF KDPRODSL-VOLVO-ALL OR                                       
054700              KDPRODSL-LOCAL-BYTES                                        
054800*    DIVERSE SALDOUPPDATERINGAR                                           
054900             PERFORM BB-BEARBETA-W510-SALDON                              
055000             IF DCS-LAND-NON-VCC-OWNED                                    
055010               IF DCS-INDIA                                               
055020                 CONTINUE                                                 
055030               ELSE                                                       
055100*    SKAPA PEDAL2-TRANSAKTION                                             
055200                 PERFORM BE-BEARBETA-W570-PEDAL2                          
055210               END-IF                                                     
055300             ELSE                                                         
055400*    SKAPA PEDAL-TRANSAKTION                                              
055500               PERFORM BC-BEARBETA-W510-PEDAL                             
055600             END-IF                                                       
055700           END-IF                                                         
055800        END-IF                                                            
055900     END-IF                                                               
056000     .                                                                    
056100     EJECT                                                                
056200                                                                          
056300 BA-BEARBETA-W560 SECTION.                                                
056400     MOVE 'A14'                  TO A14-IDPTYP                            
056500     MOVE INFIL-IDDC             TO A14-IDDC-SEND                         
056600                                    A14-IDDC-REC                          
056700     MOVE 'I40'                  TO A14-KDEKOHT                           
056800     MOVE INFIL-IDDISTR          TO A14-IDDISTR                           
056900     MOVE INFIL-IDKUNDNR         TO A14-IDKUNDNR                          
057000     MOVE INFIL-IDBYTRAP         TO A14-IDBYTRAP                          
057100     MOVE INFIL-KVRETUR-GODK     TO A14-KVRETUR                           
057200     PERFORM S06-ANROP-MSGI                                               
057300     MOVE INFIL-IDARTNR-OBJ      TO A14-IDARTNR                           
057400                                                                          
057500     IF DCS-NDC-NA AND DCS-CANADA                                         
057600        MOVE +54                 TO A14-IDFTG                             
057700     ELSE                                                                 
057800        MOVE +53                 TO A14-IDFTG                             
057900     END-IF                                                               
058000                                                                          
058100     PERFORM IMS-GET-ARTC-WLARTC01                                        
058200     IF SEGMENT-FINNS                                                     
058300       MOVE ARTC-ART-KDPRODSL      TO A14-KDPRODSL                        
058400                                                                          
058500       PERFORM IMS-GET-ARTC-WLARTC11                                      
058600       IF SEGMENT-FINNS                                                   
058700          MOVE ARTC-CLAG-KDPSLLOC  TO A14-KDPSLLOC                        
058800       END-IF                                                             
058900     END-IF                                                               
059000                                                                          
059100     PERFORM IMS-GHU-WDK711                                               
059200     IF SEGMENT-FINNS                                                     
059300        MOVE SLAG-PRAVCOST       TO A14-PRAVCOST                          
059400        MOVE INFIL-KDBYTREF      TO TEST-KDBYTREF                         
059500        IF NOT BYT15-KDBYTREF-GAR-EJ-SALD                                 
059600          ADD INFIL-KVRETUR-GODK TO SLAG-KVLS                             
059700          PERFORM IMS-REPL-WDK7-SALDO                                     
059800          PERFORM BAC-FLYTTA-LOGG                                         
059900          PERFORM IMS-ISRT-WDL901                                         
060000          IF SEGMENT-FINNS-REDAN                                          
060100             PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                        
060200                ADD -1 TO LOGG-IDSEKVNR                                   
060300                PERFORM IMS-ISRT-WDL901                                   
060400             END-PERFORM                                                  
060500          END-IF                                                          
060600          PERFORM S10-ISRT-WDR8-LAB                                       
060700          IF INFIL-FLSKROT = JA                                           
060800            PERFORM S21-SKAPA-TRANSAR-SKROTNING                           
060900          END-IF                                                          
061000        END-IF                                                            
061100     ELSE                                                                 
061200*** SAKNAS TABELLRAD PÅ WDK7 ***                                          
061300        MOVE INFIL-KDBYTREF      TO TEST-KDBYTREF                         
061400        IF NOT  BYT15-KDBYTREF-GAR-EJ-SALD                                
061500           PERFORM BAA-NYA-DC-SEGMENT-WDK7                                
061600           MOVE SLAG-PRAVCOST    TO A14-PRAVCOST                          
061700           PERFORM S10-ISRT-WDR8-LAB                                      
061800          IF INFIL-FLSKROT = JA                                           
061900            PERFORM S21-SKAPA-TRANSAR-SKROTNING                           
062000          END-IF                                                          
062100        END-IF                                                            
062200     END-IF                                                               
062300     .                                                                    
062400     EJECT                                                                
062500                                                                          
062600 BAA-NYA-DC-SEGMENT-WDK7 SECTION.                                         
062700* LÄGG UPP NYA SEGMENT                                                    
062800     MOVE ALL '+'            TO WDK7-W005WDK7                             
062900     MOVE 'WDK711'           TO WDK7-IDSEGM                               
063000     MOVE INFIL-IDARTNR-OBJ  TO WDK7-IDARTNR-KFB                          
063100     MOVE INFIL-IDDC         TO WDK7-IDDC-KFB                             
063200                                WDK7-IDDC                                 
063300     MOVE INFIL-KVRETUR-GODK TO WDK7-KVLS                                 
063400     MOVE 'N'                TO WDK7-FLREFILL                             
063500                                                                          
063600     CALL W005WDK7 USING WDK7-W005WDK7 WDB6-PCB ARTC-PCB WDK7-PCB         
063700                                                                          
063800     PERFORM BAAA-FLYTTA-LOGG                                             
063900     IF SEGMENT-FINNS-REDAN                                               
064000        PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                             
064100           ADD -1 TO LOGG-IDSEKVNR                                        
064200           PERFORM IMS-ISRT-WDL901                                        
064300        END-PERFORM                                                       
064400     END-IF                                                               
064500     .                                                                    
064600     EJECT                                                                
064700                                                                          
064800 BAAA-FLYTTA-LOGG SECTION.                                                
064900* LÄGGER UPP LOGG I WDL9                                                  
065000     MOVE W-IDARTNR               TO LOGG-IDARTNR                         
065100     MOVE FUNCTION CURRENT-DATE (1:8)                                     
065200                                  TO LOG-DAGENS-DATUM                     
065300     COMPUTE LOGG-DAREGDAT-9KOMPL = 99999999 - LOG-DAGENS-DATUM           
065400     ACCEPT TRANS-TID FROM TIME                                           
065500     COMPUTE LOGG-TIKLOCK-9KOMPL  = 999999999 - TRANS-TID                 
065600     MOVE 9                       TO LOGG-IDSEKVNR                        
065700     MOVE INFIL-IDDC              TO LOGG-IDDC                            
065800     MOVE 'EXCH'                  TO LOGG-IDHUVTYP                        
065900     MOVE 'OBJ'                   TO LOGG-IDSUBTYP                        
066000     MOVE IDPGM                   TO LOGG-IDPGM                           
066100     MOVE SPACE                   TO LOGG-IDTRANS                         
066200     MOVE IDPGM                   TO LOGG-IDUSER                          
066300     MOVE SPACE                   TO LOGG-REF                             
066400     MOVE INFIL-IDDISTR           TO LOGG-IDDISTR                         
066500     MOVE INFIL-IDKUNDNR          TO LOGG-IDKUNDNR                        
066600     MOVE INFIL-IDKUNDRF          TO LOGG-IDKUNDRF                        
066700     MOVE '+'                     TO LOGG-IDTECKEN-KVLS                   
066800     MOVE SPACE                   TO LOGG-IDTECKEN-KVAKS-PAV              
066900     MOVE SPACE                   TO LOGG-IDTECKEN-KVAKS                  
067000     MOVE SPACE                   TO LOGG-IDTECKEN-KVEFRS                 
067100     MOVE INFIL-KVRETUR-GODK      TO LOGG-KVART-SALDO                     
067200     MOVE WDK7-KVAKS-SDC          TO LOGG-KVAKS                           
067300     MOVE WDK7-KVAKS-PAV          TO LOGG-KVAKS-PAV                       
067400     MOVE WDK7-KVEFRS             TO LOGG-KVEFRS                          
067500     MOVE WDK7-KVLS               TO LOGG-KVLS                            
067600     MOVE ZERO                    TO LOGG-DAREGDAT-LADD                   
067700     .                                                                    
067800     EJECT                                                                
067900                                                                          
068000 BAC-FLYTTA-LOGG SECTION.                                                 
068100* LÄGGER UPP LOGG I WDL9                                                  
068200     MOVE W-IDARTNR               TO LOGG-IDARTNR                         
068300     MOVE FUNCTION CURRENT-DATE (1:8)                                     
068400                                  TO LOG-DAGENS-DATUM                     
068500     COMPUTE LOGG-DAREGDAT-9KOMPL = 99999999 - LOG-DAGENS-DATUM           
068600     ACCEPT TRANS-TID FROM TIME                                           
068700     COMPUTE LOGG-TIKLOCK-9KOMPL  = 999999999 - TRANS-TID                 
068800     MOVE 9                       TO LOGG-IDSEKVNR                        
068900     MOVE INFIL-IDDC              TO LOGG-IDDC                            
069000     MOVE 'EXCH'                  TO LOGG-IDHUVTYP                        
069100     MOVE 'OBJ'                   TO LOGG-IDSUBTYP                        
069200     MOVE IDPGM                   TO LOGG-IDPGM                           
069300     MOVE SPACE                   TO LOGG-IDTRANS                         
069400     MOVE IDPGM                   TO LOGG-IDUSER                          
069500     MOVE SPACE                   TO LOGG-REF                             
069600     MOVE INFIL-IDDISTR           TO LOGG-IDDISTR                         
069700     MOVE INFIL-IDKUNDNR          TO LOGG-IDKUNDNR                        
069800     MOVE INFIL-IDKUNDRF          TO LOGG-IDKUNDRF                        
069900     MOVE '+'                     TO LOGG-IDTECKEN-KVLS                   
070000     MOVE SPACE                   TO LOGG-IDTECKEN-KVAKS-PAV              
070100     MOVE SPACE                   TO LOGG-IDTECKEN-KVAKS                  
070200     MOVE SPACE                   TO LOGG-IDTECKEN-KVEFRS                 
070300     MOVE INFIL-KVRETUR-GODK      TO LOGG-KVART-SALDO                     
070400     MOVE SLAG-KVAKS-SDC          TO LOGG-KVAKS                           
070500     MOVE SLAG-KVAKS-PAV          TO LOGG-KVAKS-PAV                       
070600     MOVE SLAG-KVEFRS             TO LOGG-KVEFRS                          
070700     MOVE SLAG-KVLS               TO LOGG-KVLS                            
070800     MOVE ZERO                    TO LOGG-DAREGDAT-LADD                   
070900     .                                                                    
071000     EJECT                                                                
071100                                                                          
071200 BB-BEARBETA-W510-SALDON SECTION.                                         
071300     IF DCS-NDC-PF                                                        
071400        CONTINUE                                                          
071500     ELSE                                                                 
071600        PERFORM BBA-SOK-SAMLINGSNR-TABNR                                  
071700     END-IF                                                               
071800                                                                          
071900     IF DCS-CDC                                                           
072000        PERFORM IMS-GHU-ART6-SALDO                                        
072100        IF SEGMENT-FINNS                                                  
072200           ADD INFIL-KVRETUR-GODK TO ARTC-CLAG-KVLS                       
072300           PERFORM IMS-REPL-ART6-SALDO                                    
072400           PERFORM BBD-SKAPA-SALDOLOGG-WDK6                               
072410           IF INFIL-FLSKROT = JA                                          
072420             PERFORM S21-SKAPA-TRANSAR-SKROTNING                          
072430           END-IF                                                         
072500        END-IF                                                            
072600     ELSE                                                                 
072700                                                                          
072800        IF (DCS-SDC AND DCS-HOLLAND) OR DCS-AUSTRALIA                     
072810        OR  DCS-LAND-NON-VCC-OWNED OR DCS-JAPAN                           
072900           CONTINUE                                                       
073000        ELSE                                                              
073100           DISPLAY 'FEL LAGERBET. PÅ TRANSAKTIONEN = ' INFIL-IDDC         
073200           CALL ABEND                                                     
073300        END-IF                                                            
073400                                                                          
073500        PERFORM IMS-GHU-WDK711                                            
073600        IF SEGMENT-FINNS                                                  
073700          IF DCS-LAND-NON-VCC-OWNED                                       
073800            IF SLAG-PRAVCOST = ZERO                                       
073900              IF ARTC-CLAG-PRARTSTD = ZERO                                
074000                MOVE 10                 TO WS-SLAG-PRAVCOST               
074100              ELSE                                                        
074200                MOVE ARTC-CLAG-PRARTSTD TO WS-SLAG-PRAVCOST               
074300              END-IF                                                      
074310              PERFORM BBF-GET-CURRENCY-RATE                               
074320              COMPUTE SLAG-PRAVCOST ROUNDED =                             
074330              WS-SLAG-PRAVCOST / W-PRKURS                                 
074400            END-IF                                                        
074500          END-IF                                                          
074600          ADD INFIL-KVRETUR-GODK TO SLAG-KVLS                             
074700          PERFORM IMS-REPL-WDK7-SALDO                                     
074800          PERFORM BBE-SKAPA-SALDOLOGG-WDK7                                
074900        ELSE                                                              
075000          PERFORM BBB-NYA-SDC91-SEGMENT-WDK7                              
075100        END-IF                                                            
075200        IF INFIL-FLSKROT = JA                                             
075300          PERFORM S21-SKAPA-TRANSAR-SKROTNING                             
075400        END-IF                                                            
075500                                                                          
075600     END-IF                                                               
075700     .                                                                    
075800     EJECT                                                                
075900                                                                          
076000 BBA-SOK-SAMLINGSNR-TABNR SECTION.                                        
076100     MOVE NEJ                       TO SOK-SW                             
076200                                                                          
076300     SET KVITT-IX                   TO +1                                 
076400     SEARCH KVITT-TAB                                                     
076500       WHEN TAB-IDARTNR(KVITT-IX) = INFIL-IDARTNR-OBJ                     
076600         MOVE TAB-IDTABNR(KVITT-IX) TO WS-IDTABNR                         
076700         MOVE JA                    TO SOK-SW                             
076800     END-SEARCH                                                           
076900                                                                          
077000     IF SOK-OK                                                            
077100       SEARCH ALL SAMLINGSTABELL                                          
077200         WHEN TAB1-KVITT(TAB1RAD) = WS-IDTABNR                            
077300            MOVE TAB1-IDARTNR-OBJ(TAB1RAD)                                
077400                                    TO INFIL-IDARTNR-OBJ                  
077500       END-SEARCH                                                         
077600     END-IF                                                               
077700     .                                                                    
077800     EJECT                                                                
077900                                                                          
078000 BBB-NYA-SDC91-SEGMENT-WDK7 SECTION.                                      
078100                                                                          
078200     MOVE ALL '+'            TO WDK7-W005WDK7                             
078300     IF DCS-LAND-NON-VCC-OWNED                                            
078400       IF ARTC-CLAG-PRARTSTD = ZERO                                       
078500         MOVE 10                 TO WDK7-PRAVCOST                         
078600       ELSE                                                               
078700         MOVE ARTC-CLAG-PRARTSTD TO WDK7-PRAVCOST                         
078800       END-IF                                                             
078810       PERFORM BBF-GET-CURRENCY-RATE                                      
078820       COMPUTE WDK7-PRAVCOST ROUNDED =                                    
078830       WDK7-PRAVCOST / W-PRKURS                                           
078900     END-IF                                                               
079000     MOVE 'WDK711'           TO WDK7-IDSEGM                               
079100     MOVE INFIL-IDARTNR-OBJ  TO WDK7-IDARTNR-KFB                          
079200     MOVE INFIL-IDDC         TO WDK7-IDDC-KFB                             
079300                                WDK7-IDDC                                 
079400     MOVE INFIL-KVRETUR-GODK TO WDK7-KVLS                                 
079500     MOVE 'N'                TO WDK7-FLREFILL                             
079600                                                                          
079700     CALL W005WDK7 USING WDK7-W005WDK7 WDB6-PCB ARTC-PCB WDK7-PCB         
079800                                                                          
079900     PERFORM BBBA-SKAPA-SALDOLOGG-WDK7                                    
080000     .                                                                    
080100     EJECT                                                                
080200                                                                          
080300 BBBA-SKAPA-SALDOLOGG-WDK7 SECTION.                                       
080400     MOVE W-IDARTNR                 TO LOGG-IDARTNR                       
080500     MOVE 9                         TO LOGG-IDSEKVNR                      
080600     MOVE W-IDDC                    TO LOGG-IDDC                          
080700     MOVE 'EXCH'                    TO LOGG-IDHUVTYP                      
080800     MOVE 'OBJ'                     TO LOGG-IDSUBTYP                      
080900     MOVE 'W3712300'                TO LOGG-IDPGM                         
081000     MOVE '0000'                    TO LOGG-IDTRANS                       
081100     MOVE 'UNKNOWN'                 TO LOGG-IDUSER                        
081200     MOVE SPACE                     TO LOGG-REF                           
081300     MOVE INFIL-IDDISTR             TO LOGG-IDDISTR                       
081400     MOVE INFIL-IDKUNDNR            TO LOGG-IDKUNDNR                      
081500     MOVE INFIL-IDKUNDRF            TO LOGG-IDKUNDRF                      
081600     MOVE SPACE                     TO LOGG-IDTECKEN-KVAKS                
081700     MOVE SPACE                     TO LOGG-IDTECKEN-KVAKS-PAV            
081800     MOVE SPACE                     TO LOGG-IDTECKEN-KVEFRS               
081900     MOVE '+'                       TO LOGG-IDTECKEN-KVLS                 
082000     MOVE INFIL-KVRETUR-GODK        TO LOGG-KVART-SALDO                   
082100     MOVE WDK7-KVAKS-SDC            TO LOGG-KVAKS                         
082200     MOVE WDK7-KVAKS-PAV            TO LOGG-KVAKS-PAV                     
082300     MOVE WDK7-KVEFRS               TO LOGG-KVEFRS                        
082400     MOVE WDK7-KVLS                 TO LOGG-KVLS                          
082500     MOVE ZERO                      TO LOGG-DAREGDAT-LADD                 
082600     MOVE FUNCTION CURRENT-DATE(1:8) TO LOGG-DATUM                        
082700     COMPUTE LOGG-DAREGDAT-9KOMPL = 999999999 - LOGG-DATUM                
082800     ACCEPT WLOGG-TID FROM TIME                                           
082900     COMPUTE LOGG-TIKLOCK-9KOMPL = 999999999 - WLOGG-TID                  
083000                                                                          
083100     PERFORM IMS-ISRT-WDL901                                              
083200     IF SEGMENT-FINNS-REDAN                                               
083300       PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                              
083400          ADD -1 TO LOGG-IDSEKVNR                                         
083500          PERFORM IMS-ISRT-WDL901                                         
083600       END-PERFORM                                                        
083700     END-IF                                                               
083800     .                                                                    
083900     EJECT                                                                
084000                                                                          
084100 BBD-SKAPA-SALDOLOGG-WDK6 SECTION.                                        
084200     MOVE W-IDARTNR                 TO LOGG-IDARTNR                       
084300     MOVE 9                         TO LOGG-IDSEKVNR                      
084400     MOVE WS-CDC-11                 TO LOGG-IDDC                          
084500     MOVE 'EXCH'                    TO LOGG-IDHUVTYP                      
084600     MOVE 'OBJ'                     TO LOGG-IDSUBTYP                      
084700     MOVE 'W3712300'                TO LOGG-IDPGM                         
084800     MOVE '0000'                    TO LOGG-IDTRANS                       
084900     MOVE 'UNKNOWN'                 TO LOGG-IDUSER                        
085000     MOVE SPACE                     TO LOGG-REF                           
085100     MOVE INFIL-IDDISTR             TO LOGG-IDDISTR                       
085200     MOVE INFIL-IDKUNDNR            TO LOGG-IDKUNDNR                      
085300     MOVE INFIL-IDKUNDRF            TO LOGG-IDKUNDRF                      
085400     MOVE SPACE                     TO LOGG-IDTECKEN-KVAKS                
085500     MOVE SPACE                     TO LOGG-IDTECKEN-KVAKS-PAV            
085600     MOVE SPACE                     TO LOGG-IDTECKEN-KVEFRS               
085700     MOVE '+'                       TO LOGG-IDTECKEN-KVLS                 
085800     MOVE INFIL-KVRETUR-GODK        TO LOGG-KVART-SALDO                   
085900                                                                          
086000     COMPUTE LOGG-KVAKS = ARTC-CLAG-KVAKS-CDC +                           
086100                          ARTC-CLAG-KVAKS-T                               
086200     MOVE ARTC-CLAG-KVAKS-PAV       TO LOGG-KVAKS-PAV                     
086300     MOVE ARTC-CLAG-KVEFRS          TO LOGG-KVEFRS                        
086400     MOVE ARTC-CLAG-KVLS            TO LOGG-KVLS                          
086500     MOVE ZERO                      TO LOGG-DAREGDAT-LADD                 
086600     MOVE FUNCTION CURRENT-DATE(1:8) TO LOGG-DATUM                        
086700     COMPUTE LOGG-DAREGDAT-9KOMPL = 999999999 - LOGG-DATUM                
086800     ACCEPT WLOGG-TID FROM TIME                                           
086900     COMPUTE LOGG-TIKLOCK-9KOMPL = 999999999 - WLOGG-TID                  
087000                                                                          
087100     PERFORM IMS-ISRT-WDL901                                              
087200     IF SEGMENT-FINNS-REDAN                                               
087300       PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                              
087400          ADD -1 TO LOGG-IDSEKVNR                                         
087500          PERFORM IMS-ISRT-WDL901                                         
087600       END-PERFORM                                                        
087700     END-IF                                                               
087800     .                                                                    
087900     EJECT                                                                
088000                                                                          
088100 BBE-SKAPA-SALDOLOGG-WDK7 SECTION.                                        
088200     MOVE W-IDARTNR                 TO LOGG-IDARTNR                       
088300     MOVE 9                         TO LOGG-IDSEKVNR                      
088400     MOVE W-IDDC                    TO LOGG-IDDC                          
088500     MOVE 'EXCH'                    TO LOGG-IDHUVTYP                      
088600     MOVE 'OBJ'                     TO LOGG-IDSUBTYP                      
088700     MOVE 'W3712300'                TO LOGG-IDPGM                         
088800     MOVE '0000'                    TO LOGG-IDTRANS                       
088900     MOVE 'UNKNOWN'                 TO LOGG-IDUSER                        
089000     MOVE SPACE                     TO LOGG-REF                           
089100     MOVE INFIL-IDDISTR             TO LOGG-IDDISTR                       
089200     MOVE INFIL-IDKUNDNR            TO LOGG-IDKUNDNR                      
089300     MOVE INFIL-IDKUNDRF            TO LOGG-IDKUNDRF                      
089400     MOVE SPACE                     TO LOGG-IDTECKEN-KVAKS                
089500     MOVE SPACE                     TO LOGG-IDTECKEN-KVAKS-PAV            
089600     MOVE SPACE                     TO LOGG-IDTECKEN-KVEFRS               
089700     MOVE '+'                       TO LOGG-IDTECKEN-KVLS                 
089800     MOVE INFIL-KVRETUR-GODK        TO LOGG-KVART-SALDO                   
089900     MOVE SLAG-KVAKS-SDC            TO LOGG-KVAKS                         
090000     MOVE SLAG-KVAKS-PAV            TO LOGG-KVAKS-PAV                     
090100     MOVE SLAG-KVEFRS               TO LOGG-KVEFRS                        
090200     MOVE SLAG-KVLS                 TO LOGG-KVLS                          
090300     MOVE ZERO                      TO LOGG-DAREGDAT-LADD                 
090400     MOVE FUNCTION CURRENT-DATE(1:8) TO LOGG-DATUM                        
090500     COMPUTE LOGG-DAREGDAT-9KOMPL = 999999999 - LOGG-DATUM                
090600     ACCEPT WLOGG-TID FROM TIME                                           
090700     COMPUTE LOGG-TIKLOCK-9KOMPL = 999999999 - WLOGG-TID                  
090800                                                                          
090900     PERFORM IMS-ISRT-WDL901                                              
091000     IF SEGMENT-FINNS-REDAN                                               
091100       PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                              
091200          ADD -1 TO LOGG-IDSEKVNR                                         
091300          PERFORM IMS-ISRT-WDL901                                         
091400       END-PERFORM                                                        
091500     END-IF                                                               
091600     .                                                                    
091700     EJECT                                                                
091710 BBF-GET-CURRENCY-RATE  SECTION.                                          
091711     MOVE DCS-IDPARTNR        TO W-WDB1-IDPARTNR                          
091712     MOVE DCS-IDFTG           TO W-WDB1-IDFTG                             
091713     PERFORM IMS-GU-WDB101                                                
091714     IF SEGMENT-FINNS                                                     
091720       MOVE DAT-TIAAMMDD(1:4)        TO W-DATE-AAMM                       
091730       MOVE W-DATE-AAMM              TO CURR-TIAAMM                       
091750       MOVE BET-KDVALISO             TO CURR-KDVALISO-ROW                 
091770                                                                          
091797       MOVE WS-KDVALISO-HUV          TO CURR-KDVALISO-HUV                 
091799       MOVE 'M'                      TO CURR-KDVALTYP                     
091800       CALL W510CURR USING CURR-W510CURR 9305-PCB                         
091801       IF CURR-KDSVAR = ' '                                               
091802         MOVE CURR-PRKURS-NEW        TO W-PRKURS                          
091804       ELSE                                                               
091805         MOVE 1                      TO W-PRKURS                          
091807       END-IF                                                             
091808     ELSE                                                                 
091809       MOVE 1                        TO W-PRKURS                          
091810     END-IF                                                               
091811     .                                                                    
091812     EJECT                                                                
091820                                                                          
091900 BC-BEARBETA-W510-PEDAL SECTION.                                          
092000     IF DCS-NDC-PF                                                        
092100       CONTINUE                                                           
092200     ELSE                                                                 
092300       PERFORM BCA-SOK-SAMLINGSNR-TABNR                                   
092400     END-IF                                                               
092500                                                                          
092600     PERFORM IMS-GET-ARTC-WLARTC01                                        
092700     IF SEGMENT-FINNS                                                     
092800       PERFORM IMS-GET-ARTC-WLARTC11                                      
092900     END-IF                                                               
093000     IF SEGMENT-FINNS                                                     
093100        MOVE ARTC-CLAG-IDLKTO     TO WS-IDLKTO                            
093200        MOVE ARTC-CLAG-PRARTSTD   TO WS-PRARTSTD                          
093300     ELSE                                                                 
093400        MOVE ZERO                 TO WS-IDLKTO                            
093500        MOVE ZERO                 TO WS-PRARTSTD                          
093600     END-IF                                                               
093700                                                                          
093800     PERFORM BCB-EKONOMITRANS-WDR901                                      
093900     .                                                                    
094000     EJECT                                                                
094100                                                                          
094200 BCA-SOK-SAMLINGSNR-TABNR SECTION.                                        
094300     MOVE NEJ                              TO SOK-SW                      
094400                                                                          
094500     SET KVITT-IX TO +1                                                   
094600     SEARCH KVITT-TAB                                                     
094700       WHEN TAB-IDARTNR(KVITT-IX) = INFIL-IDARTNR-OBJ                     
094800         MOVE TAB-IDTABNR(KVITT-IX)        TO WS-IDTABNR                  
094900         MOVE JA                           TO SOK-SW                      
095000     END-SEARCH                                                           
095100                                                                          
095200     IF SOK-OK                                                            
095300       SEARCH ALL SAMLINGSTABELL                                          
095400         WHEN TAB1-KVITT(TAB1RAD) = WS-IDTABNR                            
095500            MOVE TAB1-IDARTNR-OBJ(TAB1RAD) TO INFIL-IDARTNR-OBJ           
095600       END-SEARCH                                                         
095700     END-IF                                                               
095800     .                                                                    
095900     EJECT                                                                
096000                                                                          
096100 BCB-EKONOMITRANS-WDR901 SECTION.                                         
096200     MOVE '302'                  TO EKH-KDEKHHT                           
096300     MOVE '301'                  TO EKH-KDEKSHT                           
096400     MOVE 'DET'                  TO EKH-KDEKNIVA                          
096500     MOVE INFIL-IDDC             TO EKH-IDDC-SEND                         
096600     MOVE '  '                   TO EKH-IDDC-REC                          
096700     MOVE INFIL-IDDISTR          TO EKH-IDDISTR                           
096800     MOVE INFIL-IDKUNDNR         TO EKH-IDKUNDNR                          
096900                                                                          
097000     MOVE 'VO'                   TO CIA-IDARTPRE-IN                       
097100     MOVE INFIL-IDARTNR-OBJ      TO CIA-IDARTBET-IN                       
097200     CALL W009CIA USING             CIA-W009CIA                           
097300     MOVE CIA-IDARTBET-UT        TO EKH-IDVERGL                           
097400                                                                          
097500     MOVE FUNCTION CURRENT-DATE (1:8)                                     
097600                                 TO EKH-DAVERDAT                          
097700     MOVE ARTC-ART-KDPRODSL      TO EKH-KDPRODSL                          
097800     MOVE ARTC-CLAG-KDPSLLOC     TO EKH-KDPSLLOC                          
097900     MOVE ARTC-ART-KDSORT        TO EKH-KDSORT                            
098000     MOVE INFIL-IDARTNR-OBJ      TO EKH-IDARTNR                           
098100     MOVE SPACE                  TO EKH-FLLSBOK                           
098200     MOVE 'SEK'                  TO EKH-KDVALISO                          
098300     MOVE  1.00                  TO EKH-PRKURS                            
098400     MOVE ZERO                   TO EKH-PRARTNTO                          
098500     MOVE ARTC-CLAG-PRARTSJK     TO EKH-PRARTSJK                          
098600     MOVE ARTC-CLAG-PRHEMTAG     TO EKH-PRHEMTAG                          
098700     MOVE ARTC-CLAG-PRARTSTD     TO EKH-PRARTSTD                          
098800     MOVE ZERO                   TO EKH-PRLANDCO                          
098900     MOVE ZERO                   TO EKH-PRINK                             
099000     MOVE ZERO                   TO EKH-PRDIRLON                          
099100     MOVE ZERO                   TO EKH-PRDMTRL                           
099200     MOVE ZERO                   TO EKH-PROVRPAL                          
099300     MOVE INFIL-KVRETUR-GODK     TO EKH-KVANTAL                           
099400     MOVE ZERO                   TO EKH-SUBEL                             
099500     MOVE '    '                 TO EKH-IDTRANS                           
099600     MOVE ZERO                   TO EKH-BEVAT                             
099700                                    EKH-IDANALYS                          
099800                                    EKH-IDKONTO                           
099900                                    EKH-KDANMORS                          
100000                                    EKH-KDFRAKT                           
100100                                    EKH-SUVAT                             
100200                                    EKH-DAAVIDAT                          
100300                                    EKH-IDAVINR                           
100400                                    EKH-KDAVVTYP                          
100500                                    EKH-KDRT                              
100600                                    EKH-KVANTMOT                          
100700                                    EKH-KVAVIS                            
100800     MOVE SPACE                  TO EKH-KDTRADP                           
100900                                    EKH-IDLEVNR                           
101000                                    EKH-IDKST                             
101100     MOVE SPACE                  TO EKH-FLDCET                            
101200     MOVE SPACE                  TO EKH-IDKUNDRF                          
101300     MOVE SPACE                  TO EKH-IDFAKT-EXP                        
101400                                                                          
101500     PERFORM S11-ISRT-WDR9                                                
101600     .                                                                    
101700     EJECT                                                                
101800                                                                          
101900 BE-BEARBETA-W570-PEDAL2 SECTION.                                         
102000     IF DCS-CHINA                                                         
102300       PERFORM BEA-SOK-SAMLINGSNR-TABNR                                   
102400     END-IF                                                               
102500                                                                          
102600     PERFORM BEB-EKONOMITRANS-WDR801                                      
102700     .                                                                    
102800     EJECT                                                                
102900                                                                          
103000 BEA-SOK-SAMLINGSNR-TABNR SECTION.                                        
103100     MOVE NEJ                              TO SOK-SW                      
103200                                                                          
103300     SET KVITT-IX TO +1                                                   
103400     SEARCH KVITT-TAB                                                     
103500       WHEN TAB-IDARTNR(KVITT-IX) = INFIL-IDARTNR-OBJ                     
103600         MOVE TAB-IDTABNR(KVITT-IX)        TO WS-IDTABNR                  
103700         MOVE JA                           TO SOK-SW                      
103800     END-SEARCH                                                           
103900                                                                          
104000     IF SOK-OK                                                            
104100       SEARCH ALL SAMLINGSTABELL                                          
104200         WHEN TAB1-KVITT(TAB1RAD) = WS-IDTABNR                            
104300            MOVE TAB1-IDARTNR-OBJ(TAB1RAD) TO INFIL-IDARTNR-OBJ           
104400       END-SEARCH                                                         
104500     END-IF                                                               
104600     .                                                                    
104700     EJECT                                                                
104800                                                                          
104900 BEB-EKONOMITRANS-WDR801 SECTION.                                         
105000     MOVE '302'                  TO EKO-EKH-KDEKHHT                       
105100     MOVE '301'                  TO EKO-EKH-KDEKSHT                       
105200     MOVE 'DET'                  TO EKO-EKH-KDEKNIVA                      
105300     MOVE INFIL-IDDC             TO EKO-EKH-IDDC-SEND                     
105400     MOVE '  '                   TO EKO-EKH-IDDC-REC                      
105500     MOVE INFIL-IDDISTR          TO EKO-EKH-IDDISTR                       
105600     MOVE INFIL-IDKUNDNR         TO EKO-EKH-IDKUNDNR                      
105700                                                                          
105800     MOVE 'VO'                   TO CIA-IDARTPRE-IN                       
105900     MOVE INFIL-IDARTNR-OBJ      TO CIA-IDARTBET-IN                       
106000     CALL W009CIA USING             CIA-W009CIA                           
106100     MOVE CIA-IDARTBET-UT        TO EKO-EKH-IDVERGL                       
106200                                                                          
106300     MOVE FUNCTION CURRENT-DATE (1:8)                                     
106400                                 TO EKO-EKH-DAVERDAT                      
106500     MOVE ARTC-ART-KDPRODSL      TO EKO-EKH-KDPRODSL                      
106600     MOVE ARTC-CLAG-KDPSLLOC     TO EKO-EKH-KDPSLLOC                      
106700     MOVE ARTC-ART-KDSORT        TO EKO-EKH-KDSORT                        
106800     MOVE INFIL-IDARTNR-OBJ      TO EKO-EKH-IDARTNR                       
106900     MOVE SPACE                  TO EKO-EKH-FLLSBOK                       
107030     MOVE DCS-KDVALISO           TO EKO-EKH-KDVALISO                      
107040     MOVE DCS-KDTRADP            TO EKO-EKH-KDTRADP                       
107050     IF DCS-NDC-CN                                                        
107060       MOVE 'W570'               TO EKO-FIL-IDCPYTXT                      
107070                                 IN EKO-FIL-WDR801(1:4)                   
107080     ELSE                                                                 
107094       MOVE DCS-KDTRADP          TO EKO-FIL-IDCPYTXT                      
107095                                 IN EKO-FIL-WDR801(1:4)                   
107097     END-IF                                                               
107098     MOVE 'EKHA'                 TO EKO-FIL-IDCPYTXT                      
107099                                 IN EKO-FIL-WDR801(5:4)                   
107110     MOVE  1.00                  TO EKO-EKH-PRKURS                        
107200     MOVE ZERO                   TO EKO-EKH-PRARTNTO                      
107300     MOVE ZERO                   TO EKO-EKH-PRARTSJK                      
107400     MOVE ZERO                   TO EKO-EKH-PRHEMTAG                      
107500     MOVE ZERO                   TO EKO-EKH-PRARTSTD                      
107600                                                                          
107700     PERFORM IMS-GHU-WDK711                                               
107800     IF SEGMENT-FINNS                                                     
107900        MOVE SLAG-PRAVCOST       TO EKO-EKH-PRARTSTD                      
108000     END-IF                                                               
108100                                                                          
108200     MOVE ZERO                   TO EKO-EKH-PRLANDCO                      
108300     MOVE ZERO                   TO EKO-EKH-PRINK                         
108400     MOVE ZERO                   TO EKO-EKH-PRDIRLON                      
108500     MOVE ZERO                   TO EKO-EKH-PRDMTRL                       
108600     MOVE ZERO                   TO EKO-EKH-PROVRPAL                      
108700     MOVE INFIL-KVRETUR-GODK     TO EKO-EKH-KVANTAL                       
108800     MOVE ZERO                   TO EKO-EKH-SUBEL                         
108900     MOVE '    '                 TO EKO-EKH-IDTRANS                       
109000     MOVE ZERO                   TO EKO-EKH-BEVAT                         
109100                                    EKO-EKH-IDANALYS                      
109200                                    EKO-EKH-IDKONTO                       
109300                                    EKO-EKH-KDANMORS                      
109400                                    EKO-EKH-KDFRAKT                       
109500                                    EKO-EKH-SUVAT                         
109600                                    EKO-EKH-DAAVIDAT                      
109700                                    EKO-EKH-IDAVINR                       
109800                                    EKO-EKH-KDAVVTYP                      
109900                                    EKO-EKH-KDRT                          
110000                                    EKO-EKH-KVANTMOT                      
110100                                    EKO-EKH-KVAVIS                        
110300     MOVE SPACE                  TO EKO-EKH-IDLEVNR                       
110400                                    EKO-EKH-IDKST                         
110500     MOVE SPACE                  TO EKO-EKH-FLDCET                        
110600     MOVE SPACE                  TO EKO-EKH-IDKUNDRF                      
110700     MOVE SPACE                  TO EKO-EKH-IDFAKT-EXP                    
110800                                                                          
110900     PERFORM S09-ISRT-WDR8                                                
111000     .                                                                    
111100     EJECT                                                                
111200                                                                          
111300 Z-FINIT SECTION.                                                         
111400                                                                          
111500     PERFORM S22-AVSLUTA-ORDERRADTRANS                                    
111600     PERFORM IMS-GET-XXCU-ROT                                             
111700     PERFORM IMS-GET-XXCU-SEG                                             
111800     MOVE ZERO TO XXCU-3144-KVPOST                                        
111900     PERFORM IMS-REPL-XXCU-SEG                                            
112000                                                                          
112100     CLOSE W37104                                                         
112200                                                                          
112300     MOVE 'S' TO POSTSUM-OPKOD                                            
112400     CALL POSTSUM USING POSTSUM-PARM                                      
112500     .                                                                    
112600     EJECT                                                                
112700                                                                          
112800 S01-LAES-W37104  SECTION.                                                
112900     READ W37104          INTO INFIL-AREA                                 
113000     AT END                                                               
113100        SET END-OF-W37104 TO   TRUE                                       
113200     NOT AT END                                                           
113300        MOVE 'W37104'     TO   POSTSUM-FDNAMN                             
113400        MOVE 'W37123D1'   TO   POSTSUM-DDNAMN2                            
113500        MOVE INFIL-IDPTYP TO   POSTSUM-TRANSTYP                           
113600        CALL POSTSUM USING POSTSUM-PARM                                   
113700     END-READ                                                             
113800     .                                                                    
113900     EJECT                                                                
114000 S02-LAS-W37181 SECTION.                                                  
114100     READ W37181 INTO KVITT-AREA                                          
114200          AT END MOVE JA TO W37181-EOF-SW                                 
114300     END-READ                                                             
114400                                                                          
114500     IF W37181-EOF-SW = NEJ                                               
114600       MOVE 'W37181'         TO POSTSUM-FDNAMN                            
114700       MOVE 'W37123D2'       TO POSTSUM-DDNAMN2                           
114800       MOVE 'KVITT'          TO POSTSUM-TRANSTYP                          
114900       CALL POSTSUM USING POSTSUM-PARM                                    
115000     END-IF                                                               
115100     .                                                                    
115200     EJECT                                                                
115300                                                                          
115400 S06-ANROP-MSGI SECTION.                                                  
115500     MOVE ALL '+'           TO MSGI-WMSGINIT                              
115600     MOVE '013'             TO MSGI-KDCALL                                
115700     MOVE INFIL-IDDC        TO W-IDDC-MSGI                                
115800     MOVE W-IDDCTEXT-MSGI   TO MSGI-IDUSER                                
115900     MOVE 'W371'            TO MSGI-IDTRANS                               
116000     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
116100     MOVE MSGI-TILOKDAT     TO W-TILLOKDAT                                
116200     MOVE W-TILLOKDAT       TO DATE-TILLOKDAT                             
116300     IF W-TILLOKDAT(1:2) > 60                                             
116400        MOVE 19             TO SEKEL-TILLOKDAT                            
116500     ELSE                                                                 
116600        MOVE 20             TO SEKEL-TILLOKDAT                            
116700     END-IF                                                               
116800     MOVE WS-TILLOKDAT      TO A14-DAINLINL                               
116900     .                                                                    
117000     EJECT                                                                
117100                                                                          
117200 S08-TAG-CHECKPOINT   SECTION.                                            
117300     ADD CHKP-ANT      TO CHKP-TOT                                        
117400     PERFORM IMS-GET-XXCU-ROT                                             
117500     PERFORM IMS-GET-XXCU-SEG                                             
117600     MOVE CHKP-TOT     TO XXCU-3144-KVPOST                                
117700                                                                          
117800     DISPLAY ' KVPOST ORDER RAPPORT TIKLOCK '                             
117900     XXCU-3144-KVPOST '/' W-IDORDNR-X '/' SPAR-IDBYTRAP '/'               
118000     W-TIKLOCK                                                            
118100                                                                          
118200     PERFORM IMS-REPL-XXCU-SEG                                            
118300                                                                          
118400     PERFORM IMS-CHECKPOINT                                               
118500     MOVE ZERO         TO CHKP-ANT                                        
118600     .                                                                    
118700     EJECT                                                                
118800                                                                          
118900 S09-ISRT-WDR8 SECTION.                                                   
119000     MOVE IDPGM                TO EKO-FIL-IDPGM                           
119100     MOVE DAGENS-DATUM         TO EKO-FIL-TIREGDAT                        
119200     ADD +1                    TO DAGENS-KLOCKSLAG                        
119300     MOVE DAGENS-KLOCKSLAG     TO EKO-FIL-TIKLOCK                         
119400     MOVE +1                   TO EKO-FIL-IDSEKVNR                        
119600                                                                          
119700     PERFORM IMS-ISRT-WDR801                                              
119800     PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                                
119900         ADD +1 TO EKO-FIL-IDSEKVNR                                       
120000         PERFORM IMS-ISRT-WDR801                                          
120100     END-PERFORM                                                          
120200     .                                                                    
120300     EJECT                                                                
120400                                                                          
120500 S10-ISRT-WDR8-LAB SECTION.                                               
120600     MOVE IDPGM                TO EKO-FIL-IDPGM                           
120700     MOVE DAGENS-DATUM         TO EKO-FIL-TIREGDAT                        
120800     ADD +1                    TO DAGENS-KLOCKSLAG                        
120900     MOVE DAGENS-KLOCKSLAG     TO EKO-FIL-TIKLOCK                         
121000     MOVE +1                   TO EKO-FIL-IDSEKVNR                        
121100     MOVE 'W510A14 '           TO EKO-FIL-IDCPYTXT                        
121200     MOVE A14-W510A14          TO EKO-FIL-WDR801-DATA                     
121300                                                                          
121400     PERFORM IMS-ISRT-WDR801                                              
121500     PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                                
121600         ADD +1 TO EKO-FIL-IDSEKVNR                                       
121700         PERFORM IMS-ISRT-WDR801                                          
121800     END-PERFORM                                                          
121900     .                                                                    
122000     EJECT                                                                
122100                                                                          
122200 S11-ISRT-WDR9  SECTION.                                                  
122300     MOVE IDPGM            TO FIL-IDPGM    IN FIL-WDR901                  
122400     MOVE FUNCTION CURRENT-DATE (1:8)                                     
122500                           TO FIL-DAREGDAT IN FIL-WDR901                  
122600     MOVE FUNCTION CURRENT-DATE (9:8)                                     
122700                           TO FIL-TIKLOCK  IN FIL-WDR901                  
122800     ADD  +1               TO FIL-TIKLOCK  IN FIL-WDR901                  
122900     MOVE +1               TO FIL-IDSEKVNR IN FIL-WDR901                  
123000     MOVE 'W510EKHA'       TO FIL-IDCPYTXT IN FIL-WDR901                  
123100     MOVE IDPGM            TO FIL-IDUSER   IN FIL-WDR901                  
123200                                                                          
123300     PERFORM IMS-ISRT-WDR901                                              
123400     PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                                
123500       ADD +1 TO FIL-IDSEKVNR IN FIL-WDR901                               
123600       PERFORM IMS-ISRT-WDR901                                            
123700     END-PERFORM                                                          
123800     .                                                                    
123900     EJECT                                                                
124000                                                                          
124100 S21-SKAPA-TRANSAR-SKROTNING SECTION.                                     
124200                                                                          
124300                                                                          
124400*    UNDERSÖK OM NY ORDER                                                 
124500     IF INFIL-IDDISTR = SPAR-IDDISTR AND                                  
124600        INFIL-IDKUNDNR = SPAR-IDKUNDNR AND                                
124700        INFIL-IDBYTRAP = SPAR-IDBYTRAP                                    
124800          CONTINUE                                                        
124900     ELSE                                                                 
125000        IF SKROTORDER-SKAPAD                                              
125100          PERFORM S22-AVSLUTA-ORDERRADTRANS                               
125200        END-IF                                                            
125300        PERFORM S23-SPARA-ORDERIDENT                                      
125400        PERFORM S24-SKAPA-MSG-KOM-AREA                                    
125500        PERFORM S25-SKAPA-ORDERHUVUDTRANS                                 
125600     END-IF                                                               
125700                                                                          
125800     PERFORM S28-SKAPA-ORDERRADTRANS                                      
125900     .                                                                    
126000     EJECT                                                                
126100                                                                          
126200 S22-AVSLUTA-ORDERRADTRANS SECTION.                                       
126300     SKIP2                                                                
126400                                                                          
126500     IF ORAD-IX > 0                                                       
126600        IF ORAD-IX NOT > ORAD-IX-MAX                                      
126700           MOVE 'J'              TO 4252-MID-FLSLUT                       
126800                                                                          
126900        END-IF                                                            
127000                                                                          
127100        CALL W006KOM USING MSG-PCB                                        
127200                           DISP-PCB                                       
127300                           KOMA-PCB                                       
127400                           MSG-KOM-WMSGKOM                                
127500                           MSG-IO-AREA                                    
127600        IF MSG-KOM-IDMFSMED NOT = SPACE                                   
127700*          FELAKTIG UPPDATERING PÅ KOMMUNIKATIONS DB                      
127800*          DUBBLETT ELLER DATUM,TID EJ NUM - FÅR EJ INTRÄFFA              
127900           MOVE ' FELAKTIG DATUM,TID PÅ INPUTFIL W37104 '                 
128000                         TO FELTEXT                                       
128100           DISPLAY ' FELAKTIG DATUM,TID INPUTFIL W37104 '                 
128200           CALL ABEND USING RKOD-ABEND-UTAN-DUMP                          
128300        END-IF                                                            
128400                                                                          
128500        IF 4252-MID-FLSLUT = 'J'                                          
128600          PERFORM S08-TAG-CHECKPOINT                                      
128700        END-IF                                                            
128800                                                                          
128900     END-IF                                                               
129000                                                                          
129100     MOVE ZERO     TO ORAD-IX                                             
129200     MOVE SPACE    TO 4252-MID-W4I25201                                   
129300     .                                                                    
129400     EJECT                                                                
129500                                                                          
129600 S23-SPARA-ORDERIDENT SECTION.                                            
129700                                                                          
129800     MOVE INFIL-IDDISTR           TO SPAR-IDDISTR                         
129900     MOVE INFIL-IDKUNDNR          TO SPAR-IDKUNDNR                        
130000     MOVE INFIL-IDBYTRAP          TO SPAR-IDBYTRAP                        
130100                                                                          
130200     MOVE JA TO SKROT-SW                                                  
130300     .                                                                    
130400     EJECT                                                                
130500                                                                          
130600 S24-SKAPA-MSG-KOM-AREA SECTION.                                          
130700                                                                          
130800* GÖRS 1 GNG FÖR VARJE NY ORDER * LÄGG I EGEN SECTION?                    
130900     MOVE SPACE                  TO MSG-KOM-WMSGKOM                       
131000     MOVE +54                    TO MSG-KOM-KVLL                          
131100     MOVE LOW-VALUE              TO MSG-KOM-KDZ1                          
131200     MOVE LOW-VALUE              TO MSG-KOM-KDZ2                          
131300     MOVE SPACE                  TO MSG-KOM-KDTRANS                       
131400     MOVE 'W4I25101'             TO MSG-KOM-IDCPYTXT                      
131500     MOVE 'BYTES   '             TO MSG-KOM-IDSNDNOD                      
131600     MOVE 'W3712300'             TO MSG-KOM-IDSNDJOB                      
131700     MOVE DAGENS-DATUM           TO MSG-KOM-TIREGDAT                      
131800     ADD +1                      TO W-TIKLOCK                             
131900     MOVE W-TIKLOCK              TO MSG-KOM-TIKLOCK                       
132000     MOVE SPACE                  TO MSG-KOM-IDMFSMED                      
132100                                    MSG-KOM-KDSVAR                        
132200     .                                                                    
132300     EJECT                                                                
132400                                                                          
132500 S25-SKAPA-ORDERHUVUDTRANS SECTION.                                       
132600                                                                          
132700     COMPUTE MSG-KVLL = LENGTH OF 4251-MID-W4I25101 + 17                  
132800                                                                          
132900     MOVE LOW-VALUE              TO MSG-KDZ1                              
133000     MOVE LOW-VALUE              TO MSG-KDZ2                              
133100     MOVE 'W4T251X '             TO MSG-KDTRANS-1                         
133200     MOVE '4251'                 TO MSG-IDTRANS-1                         
133300     MOVE '1'                    TO MSG-KDMFSFOR-1                        
133400** FYLL I FÄLTEN TILL MIDEN                                               
133500     MOVE SPACE                  TO 4251-MID-W4I25101                     
133600     MOVE 'W371'                 TO 4251-MID-IDSYSTEM                     
133610     MOVE '57'                   TO 4251-MID-IDFTG                        
133700     IF DCS-CDC OR DCS-SDC OR DCS-AUSTRALIA OR DCS-JAPAN                  
133800       MOVE '68'                 TO 4251-MID-KDFRAKT                      
133900       MOVE '158600000316'       TO 4251-MID-IDANALYS                     
134000       MOVE '0000482311'         TO 4251-MID-IDKONTO                      
134100       MOVE '0082'               TO 4251-MID-IDDISTR                      
134200       MOVE '000000'             TO 4251-MID-IDKUNDNR                     
134300       MOVE '1'                  TO 4251-MID-KDORDKL                      
134400       MOVE INFIL-IDDC           TO 4251-MID-IDDC                         
134500     ELSE                                                                 
134600       IF DCS-NDC-NA                                                      
134700         MOVE '11'               TO 4251-MID-KDFRAKT                      
134800         MOVE '8480'             TO 4251-MID-IDDISTR                      
134900         MOVE INFIL-IDDC         TO W-IDKUNDNR-2                          
135000         MOVE W-IDKUNDNR-X       TO 4251-MID-IDKUNDNR                     
135100         MOVE '1'                TO 4251-MID-KDORDKL                      
135200         MOVE INFIL-IDDC         TO 4251-MID-IDDC                         
135300       ELSE                                                               
135400          IF DCS-LAND-NON-VCC-OWNED                                       
135410            MOVE DCS-IDFTG      TO 4251-MID-IDFTG                         
135500            MOVE '68'           TO 4251-MID-KDFRAKT                       
135600            MOVE '8480'         TO 4251-MID-IDDISTR                       
135700            MOVE INFIL-IDDC     TO W-IDKUNDNR-2                           
135800            MOVE W-IDKUNDNR-X   TO 4251-MID-IDKUNDNR                      
135900            MOVE '1'            TO 4251-MID-KDORDKL                       
136000            MOVE INFIL-IDDC     TO 4251-MID-IDDC                          
136100          END-IF                                                          
136200       END-IF                                                             
136300     END-IF                                                               
136400                                                                          
136500     PERFORM S26-LAES-HOEGSTA-IDORDNR                                     
136600     MOVE W-IDORDNR-X             TO 4251-MID-IDORDNR                     
136700     MOVE SPACE                   TO 4251-MID-TIRFS                       
136800                                     4251-MID-BEKUNDRF                    
136900     MOVE 'N'                     TO 4251-MID-KDFAKTYP                    
137000     MOVE NEJ                     TO 4251-MID-FLRESTN                     
137100     MOVE SPACE                   TO 4251-MID-KDTPOTYP                    
137200                                     4251-MID-TITPO                       
137300                                     4251-MID-BELAGINS                    
137400                                     4251-MID-BEGMT                       
137500                                     4251-MID-ADGMT-GATA                  
137600                                     4251-MID-ADGMT-PADR                  
137700                                     4251-MID-KDROPACK                    
137800                                     4251-MID-BEVARREF                    
137900                                     4251-MID-KDTULLVE                    
138000                                     4251-MID-KDNOTES                     
138100     MOVE JA                      TO 4251-MID-FLAUTFAK                    
138200     MOVE JA                      TO 4251-MID-FLAUTPAC                    
138300     MOVE NEJ                     TO 4251-MID-FLEMBORD                    
138400     MOVE NEJ                     TO 4251-MID-FLOVRLEV                    
138600     MOVE SPACE                   TO 4251-MID-IDKAMPRF                    
138700                                     4251-MID-ADBET                       
138800                                     4251-MID-BEBET                       
138900                                     4251-MID-IDSKYLT                     
139000                                     4251-MID-FLLSBOK                     
139100                                     4251-MID-IDBILREG                    
139200                                     4251-MID-IDVIN                       
139300                                     4251-MID-IDCISNR                     
139400                                     4251-MID-IDKST                       
139500     MOVE SPACE                   TO 4251-MID-KDORDTYP-LDC                
139600     MOVE ZERO                    TO 4251-MID-TIREPDAT                    
139800                                     4251-MID-IDGROSS                     
139900     MOVE NEJ                     TO 4251-MID-FLFORBI                     
140000                                     4251-MID-FLORDTIL                    
140100                                                                          
140200     PERFORM S27-ANROPA-W006KOM                                           
140300                                                                          
140400*    MOVE SPACE    TO 4251-MID-W4I25101                                   
140500                                                                          
140600     .                                                                    
140700 S26-LAES-HOEGSTA-IDORDNR  SECTION.                                       
140800                                                                          
140900     MOVE 'NL1'  TO W-IDRT-4111                                           
141000                                                                          
141100     PERFORM IMS-GHU-WDGX4112                                             
141200     MOVE 4112-IDORDNR7 TO W-IDORDNR-X                                    
141300     IF W-IDORDNR-VV = WS-DAT-TIVV-ORDER                                  
141400       ADD  +1                    TO W-IDORDNR-LLL                        
141500     ELSE                                                                 
141600       MOVE WS-DAT-TIVV-ORDER     TO W-IDORDNR-VV                         
141700       MOVE +1                    TO W-IDORDNR-LLL                        
141800     END-IF                                                               
141900     MOVE W-IDORDNR-X  TO 4112-IDORDNR7                                   
142000     PERFORM IMS-REPL-WDGX4112                                            
142100     .                                                                    
142200     EJECT                                                                
142300                                                                          
142400 S27-ANROPA-W006KOM SECTION.                                              
142500                                                                          
142600     CALL W006KOM USING MSG-PCB                                           
142700                        DISP-PCB                                          
142800                        KOMA-PCB                                          
142900                        MSG-KOM-WMSGKOM                                   
143000                        MSG-IO-AREA                                       
143100                                                                          
143200     IF MSG-KOM-IDMFSMED NOT = SPACE                                      
143300*       FELAKTIG UPPDATERING PÅ KOMMUNIKATIONS DB                         
143400*       DUBBLETT ELLER DATUM,TID EJ NUM - FÅR EJ INTRÄFFA                 
143500        MOVE ' FELAKTIG DATUM,TID PÅ INPUTFIL W37104 '                    
143600                      TO FELTEXT-STR                                      
143700        DISPLAY ' FELAKTIG DATUM,TID INPUTFIL W37104 '                    
143800        CALL ABEND USING RKOD-ABEND-MED-DUMP                              
143900     END-IF                                                               
144000                                                                          
144100*= 4251 O 4252 AREAN   MOVE SPACE    TO MSGKOM-AREA                       
144200                                                                          
144300     .                                                                    
144400     EJECT                                                                
144500 S28-SKAPA-ORDERRADTRANS SECTION.                                         
144600                                                                          
144700     ADD +1         TO ORAD-IX                                            
144800     IF ORAD-IX > ORAD-IX-MAX                                             
144900        PERFORM S22-AVSLUTA-ORDERRADTRANS                                 
145000        ADD +1      TO ORAD-IX                                            
145100     END-IF                                                               
145200     IF ORAD-IX = 1                                                       
145300        MOVE SPACE               TO 4252-MID-W4I25201                     
145400     END-IF                                                               
145500     COMPUTE MSG-KVLL = LENGTH OF 4252-MID-W4I25201 + 17                  
145600                                                                          
145700     MOVE LOW-VALUE              TO MSG-KDZ1                              
145800     MOVE LOW-VALUE              TO MSG-KDZ2                              
145900     MOVE 'W4T252X '             TO MSG-KDTRANS-1                         
146000     MOVE '4252'                 TO MSG-IDTRANS-1                         
146100     MOVE '1'                    TO MSG-KDMFSFOR-1                        
146200                                                                          
146300     MOVE 'W371'                 TO 4252-MID-IDSYSTEM                     
146400     IF DCS-CDC OR DCS-SDC OR DCS-JAPAN OR DCS-AUSTRALIA                  
146500       MOVE '0082'               TO 4252-MID-IDDISTR                      
146600       MOVE '000000'             TO 4252-MID-IDKUNDNR                     
146700     ELSE                                                                 
146800       IF DCS-NDC-NA OR DCS-LAND-NON-VCC-OWNED                            
146900         MOVE '8480'             TO 4252-MID-IDDISTR                      
147000         MOVE INFIL-IDDC         TO W-IDKUNDNR-2                          
147100         MOVE W-IDKUNDNR-X       TO 4252-MID-IDKUNDNR                     
147200       END-IF                                                             
147300     END-IF                                                               
147400                                                                          
147500     MOVE W-IDORDNR-X            TO 4252-MID-IDORDNR                      
147600     MOVE SPACE                  TO 4252-MID-BEVOLREF                     
147700     MOVE 'N'                    TO 4252-MID-FLSLUT                       
147800                                                                          
147900     MOVE INFIL-IDARTNR-OBJ      TO 4252-MID-IDARTNR    (ORAD-IX)         
148000                                    REK-IDARTNR                           
148100     MOVE 9                      TO REK-LNGD                              
148200     MOVE 0                      TO REK-REKSIFFR                          
148300                                                                          
148400     CALL W009KSIF               USING REK-IDARTNR                        
148500                                       REK-LNGD                           
148600                                       REK-REKSIFFR                       
148700                                                                          
148800     MOVE REK-REKSIFFR           TO 4252-MID-REKSIFFR   (ORAD-IX)         
148900     MOVE INFIL-KVRETUR-GODK     TO W-KVRETUR-GODK-6                      
149000     MOVE W-KVRETUR-GODK-6       TO 4252-MID-KVBEART    (ORAD-IX)         
149100     MOVE SPACE                  TO 4252-MID-PRARTNTO   (ORAD-IX)         
149200                                    4252-MID-TITPO      (ORAD-IX)         
149300                                    4252-MID-FLRESTN    (ORAD-IX)         
149400                                    4252-MID-KDKVBRYT   (ORAD-IX)         
149500                                    4252-MID-FLINVEST   (ORAD-IX)         
149600     MOVE ZERO                   TO 4252-MID-KDVRINFO   (ORAD-IX)         
149700     MOVE ZERO                   TO 4252-MID-IDKONTO    (ORAD-IX)         
149800     MOVE SPACE                  TO 4252-MID-BERADREF   (ORAD-IX)         
149900                                    4252-MID-IDBIL      (ORAD-IX)         
150000                                    4252-MID-IDKST      (ORAD-IX)         
150100     MOVE 2                      TO 4252-MID-KDDSP      (ORAD-IX)         
150200     MOVE NEJ                    TO 4252-MID-FLSLATT    (ORAD-IX)         
150300     MOVE SPACE                TO 4252-MID-PRARTNTO-LOC (ORAD-IX)         
150400     MOVE SPACE                TO 4252-MID-PRARTBTO-LOC (ORAD-IX)         
150500     MOVE SPACE                  TO 4252-MID-KDVALISO   (ORAD-IX)         
150600     MOVE SPACE                  TO 4252-MID-KDVAT      (ORAD-IX)         
150700     MOVE 0                      TO 4252-MID-RERAB      (ORAD-IX)         
150800     MOVE SPACE                  TO 4252-MID-KDRAB      (ORAD-IX)         
150900     MOVE SPACE                TO 4252-MID-BEART-VIPS   (ORAD-IX)         
151000     MOVE ZERO                 TO 4252-MID-ADLAGOMR-CD  (ORAD-IX)         
151100                                  4252-MID-ADGANG-CD    (ORAD-IX)         
151200                                  4252-MID-ADPLATS-CD   (ORAD-IX)         
151300     MOVE SPACE                TO 4252-MID-IDKUNDRF-WIP (ORAD-IX)         
151400                                                                          
151500     .                                                                    
151600     EJECT                                                                
151700                                                                          
151800* --- IMS SEKTIONER ---                                                   
151900                                                                          
152000 IMS-GET-XXCU-ROT SECTION.                                                
152100     STRING 'WLXXCU01(WDGXKEY  =' W-WDGXKEY-X ')'                         
152200          DELIMITED BY SIZE INTO SSA1                                     
152300     MOVE '  GE' TO GODK-STATUSKODER                                      
152400     CALL CBLTDLI USING GU XXCU-PCB DLI-IO-CHKP SSA1                      
152500     MOVE XXCU-STATUS-CODE TO STATUS-WS                                   
152600     PERFORM IMS-STATUSKONTROLL                                           
152700     .                                                                    
152800                                                                          
152900 IMS-GET-XXCU-SEG SECTION.                                                
153000     MOVE  'WLXXCU11 ' TO SSA2                                            
153100     MOVE '  GE' TO GODK-STATUSKODER                                      
153200     CALL CBLTDLI USING GHNP XXCU-PCB DLI-IO-CHKP SSA1 SSA2               
153300     MOVE XXCU-STATUS-CODE TO STATUS-WS                                   
153400     PERFORM IMS-STATUSKONTROLL                                           
153500     .                                                                    
153600                                                                          
153700 IMS-REPL-XXCU-SEG SECTION.                                               
153800     MOVE '  ' TO GODK-STATUSKODER                                        
153900     CALL CBLTDLI USING REPL XXCU-PCB DLI-IO-CHKP                         
154000     MOVE XXCU-STATUS-CODE TO STATUS-WS                                   
154100     PERFORM IMS-STATUSKONTROLL                                           
154200     .                                                                    
154300                                                                          
154400 IMS-RESTART SECTION.                                                     
154500     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
154600     MOVE '  ' TO GODK-STATUSKODER                                        
154700     CALL CBLTDLI USING XRST MSG-PCB                                      
154800                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
154900                        CHKP-AREA-LENGTH CHKP-AREA                        
155000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
155100     PERFORM IMS-STATUSKONTROLL                                           
155200     .                                                                    
155300                                                                          
155400 IMS-CHECKPOINT SECTION.                                                  
155500     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
155600     MOVE '  XD' TO GODK-STATUSKODER                                      
155700     CALL CBLTDLI USING CHKP MSG-PCB                                      
155800                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
155900                        CHKP-AREA-LENGTH CHKP-AREA                        
156000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
156100     PERFORM IMS-STATUSKONTROLL                                           
156200                                                                          
156300     IF IMS-EJ-OK                                                         
156400       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
156500       DISPLAY FELTEXT                                                    
156600       CALL FELLOG                                                        
156700     END-IF                                                               
156800     .                                                                    
156900     EJECT                                                                
157000                                                                          
157100 IMS-GET-ARTC-WLARTC01 SECTION.                                           
157200     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
157300          DELIMITED BY SIZE INTO SSA1                                     
157400     MOVE '  GE' TO GODK-STATUSKODER                                      
157500     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-K601 SSA1                      
157600     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
157700     PERFORM IMS-STATUSKONTROLL                                           
157800     .                                                                    
157900     EJECT                                                                
158000                                                                          
158100 IMS-GET-ARTC-WLARTC11 SECTION.                                           
158200     STRING 'WLARTC11(KDSEGKEY =1)'                                       
158300                     DELIMITED BY SIZE INTO SSA1                          
158400     MOVE '  GE' TO GODK-STATUSKODER                                      
158500     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-K611 SSA1                     
158600     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
158700     PERFORM IMS-STATUSKONTROLL                                           
158800     .                                                                    
158900     EJECT                                                                
159000                                                                          
159100 IMS-GHU-ART6-SALDO SECTION.                                              
159200     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
159300           DELIMITED BY SIZE INTO SSA1                                    
159400     MOVE 'WLARTC11 '          TO SSA2                                    
159500     MOVE '  '                 TO GODK-STATUSKODER                        
159600     CALL CBLTDLI USING GHU ARTC-PCB DLI-IO-K611 SSA1 SSA2                
159700     MOVE ARTC-STATUS-CODE     TO STATUS-WS                               
159800     PERFORM IMS-STATUSKONTROLL                                           
159900     .                                                                    
160000                                                                          
160100 IMS-REPL-ART6-SALDO SECTION.                                             
160200     MOVE '  '                  TO GODK-STATUSKODER                       
160300     CALL CBLTDLI USING REPL ARTC-PCB DLI-IO-K611                         
160400     MOVE ARTC-STATUS-CODE      TO STATUS-WS                              
160500     PERFORM IMS-STATUSKONTROLL                                           
160600     .                                                                    
160700     EJECT                                                                
160800                                                                          
160900 IMS-GHU-WDK711 SECTION.                                                  
161000     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
161100           DELIMITED BY SIZE INTO SSA1                                    
161200     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
161300           DELIMITED BY SIZE INTO SSA2                                    
161400     MOVE '  GE' TO GODK-STATUSKODER                                      
161500     CALL CBLTDLI USING GHU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2              
161600     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
161700     PERFORM IMS-STATUSKONTROLL                                           
161800     .                                                                    
161900                                                                          
162000 IMS-REPL-WDK7-SALDO SECTION.                                             
162100     MOVE '  ' TO GODK-STATUSKODER                                        
162200     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-WDK711                       
162300     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
162400     PERFORM IMS-STATUSKONTROLL                                           
162500     .                                                                    
162600                                                                          
162700 IMS-ISRT-WDL901 SECTION.                                                 
162800     MOVE 'WLLOGA01 ' TO SSA1                                             
162900     MOVE '  II' TO GODK-STATUSKODER                                      
163000     CALL CBLTDLI USING ISRT LOGA-PCB WLLOGA01 SSA1                       
163100     MOVE LOGA-STATUS-CODE TO STATUS-WS                                   
163200     PERFORM IMS-STATUSKONTROLL                                           
163300     .                                                                    
163400     EJECT                                                                
163500                                                                          
163600 IMS-ISRT-WDR801   SECTION.                                               
163700     MOVE 'WDR801   '      TO SSA1                                        
163800     MOVE '  II'           TO GODK-STATUSKODER                            
163900     CALL CBLTDLI USING ISRT WDR8-PCB DLI-IO-WDR801 SSA1                  
164000     MOVE WDR8-STATUS-CODE TO STATUS-WS                                   
164100     PERFORM IMS-STATUSKONTROLL                                           
164200     .                                                                    
164300     EJECT                                                                
164400                                                                          
164500 IMS-ISRT-WDR901 SECTION.                                                 
164600     MOVE 'WDR901   '      TO SSA1                                        
164700     MOVE '  II'           TO GODK-STATUSKODER                            
164800     CALL CBLTDLI USING ISRT WDR9-PCB DLI-IO-WDR901 SSA1                  
164900     MOVE WDR9-STATUS-CODE TO STATUS-WS                                   
165000     PERFORM IMS-STATUSKONTROLL                                           
165100     .                                                                    
165200     EJECT                                                                
165300                                                                          
165400 IMS-GU-WDB601    SECTION.                                                
165500     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
165600          DELIMITED BY SIZE INTO SSA1                                     
165700     MOVE '  GE' TO GODK-STATUSKODER                                      
165800     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
165900     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
166000     PERFORM IMS-STATUSKONTROLL                                           
166100     IF SEGMENT-SAKNAS                                                    
166200         MOVE SPACE TO DCS-KDDC                                           
166300     END-IF                                                               
166400     .                                                                    
166500                                                                          
166600 IMS-GHU-WDGX4112    SECTION.                                             
166700                                                                          
166800     STRING 'WDR101  (WDGXKEY  =' W-WDGXKEY-4111-X ')'                    
166900            DELIMITED BY SIZE INTO SSA1                                   
167000     MOVE 'WDGX4112 ' TO SSA2                                             
167100     MOVE '  ' TO GODK-STATUSKODER                                        
167200     CALL CBLTDLI USING GHU 4111-PCB DLI-IO-WDGX4112 SSA1 SSA2            
167300     MOVE 4111-STATUS-CODE TO STATUS-WS                                   
167400     PERFORM IMS-STATUSKONTROLL                                           
167500     .                                                                    
167600     EJECT                                                                
167700 IMS-REPL-WDGX4112    SECTION.                                            
167800                                                                          
167900     MOVE '  ' TO GODK-STATUSKODER                                        
168000     CALL CBLTDLI USING REPL 4111-PCB DLI-IO-WDGX4112                     
168100     MOVE 4111-STATUS-CODE TO STATUS-WS                                   
168200     PERFORM IMS-STATUSKONTROLL                                           
168300     .                                                                    
168400     EJECT                                                                
168495                                                                          
168496 IMS-GU-WDB101 SECTION.                                                   
168497     STRING 'WLBETC01(WDB101KY =' W-WDB101KY-X ')'                        
168498          DELIMITED BY SIZE INTO SSA1                                     
168499     MOVE '  GE'               TO GODK-STATUSKODER                        
168500     CALL CBLTDLI USING GU BETC-PCB DLI-IO-WLBETC01 SSA1                  
168501     MOVE BETC-STATUS-CODE     TO STATUS-WS                               
168502     PERFORM IMS-STATUSKONTROLL                                           
168503     .                                                                    
168504     EJECT                                                                
168505                                                                          
168510 IMS-STATUSKONTROLL SECTION.                                              
168600     SET STATUS-IX TO 1                                                   
168700     SEARCH GODK-STATUS                                                   
168800       AT END                                                             
168900         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
169000           DELIMITED BY SIZE INTO FELTEXT                                 
169100         DISPLAY FELTEXT                                                  
169200         CALL FELLOG                                                      
169300       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
169400         CONTINUE                                                         
169500     END-SEARCH                                                           
169600     .                                                                    
