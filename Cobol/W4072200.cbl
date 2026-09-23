000100*                                                                         
000200******************************************************************        
000300*     THIS PROGRAM ALSO HAS A WEB-LDC VERSION CALLED WL0161      *        
000400******************************************************************        
000500*                                                                         
000600 ID DIVISION.                                                             
000700 PROGRAM-ID.     W4072200.                                                
000800 AUTHOR.         LARS THELL.                                              
000900 DATE-WRITTEN.   95/07/14.                                                
001000 DATE-COMPILED.                                                           
001100                                                                          
001200*    FUNKTION:                                                            
001300*        VISAR DETALJUPPGIFTER FÖR EN LEVERANANSMÄRKNINGSRAD              
001400*        GÖR KONTROLLER.ANROPAR KONTROLL PGM W418KTL3 RETURMATRIX         
001500*        UPPDATERING AV BEHANDLIGNSKOD.                                   
001600*                                                                         
001700*        PROGRAMMET UPPDATERAR WLKREE                                     
001800*        PROGRAMMET UPPDATERAR WL4109  WDR1 ANALYSNUMMER                  
001900*        PROGRAM 418ANSV LÄSER WL4113  WDR1 ADM-ANSVARIG                  
002000*                              WL4115  WDR1 RET-ANSVARIG                  
002100*                              WL4117  WDR1 REM-ANSVARIG                  
002200*        PROGRAMMET LÄSER      WDL5                                       
002300*        PROGRAMMET LÄSER      WLARTC                                     
002400*        PROGRAMMET LÄSER      WLBENA                                     
002500*        PROGRAMMET LÄSER      WDR5 (WDGX6328)                            
002600*        PROGRAMMET LÄSER/UPPDATERAR WDR5 (WDGX4103)                      
002700*                                                                         
002800*    E-TRACKER 1572353  DATUM 20050519                                    
002900*              2913019  DATUM 20060130                                    
003000*              1658417  DATUM 20060306                                    
003100*              850114   DATUM 20070404                                    
003200*              5674920  DATUM 20071018                                    
003300*              5798675  DATUM 20071101                                    
003400*              5838822  DATUM 20071108                                    
003500*                                                                         
003600*    E-TRACKER 8635407  DATE  2009-10-21 RETURN CODES MATRIX              
003700*    E-TRACKER 8687963  DATE  2010-03-18 REFERRALS PICKING AREA           
003800*    E-TRACKER 9822116  DATE  2010-10-14 DISCR/RETURNS HAZ.MAT.           
003900*    E-TRACKER 10133176 DATE  2011-03-17 DISCR/RETURNS HAZ.MAT.           
004000*    E-TRACKER 10143271 DATE 2011-10-19 CHINA WAREHOUSE PROJECT-1         
004100*                                                                         
004200*    STORY 1639344 / POSSIBILITY TO CHANGE DISCREPENCY CODE FROM          
004300*                     42-62 AND FROM 62-42                                
004400*    INDATA.                                                              
004500*        TRANSAKTION: W4T722                                              
004600*        MID:         W4I72201                                            
004700*                                                                         
004800*    UTDATA.                                                              
004900*        MOD:         W4O72201                                            
005000                                                                          
005100                                                                          
005200 ENVIRONMENT DIVISION.                                                    
005300                                                                          
005400 DATA DIVISION.                                                           
005500                                                                          
005600     EJECT                                                                
005700 WORKING-STORAGE SECTION.                                                 
005800*    -- CHECKED BY WY2000                                                 
005900                                                                          
006000 77  IDPGM                       PIC X(08)   VALUE 'W4072200'.            
006100                                                                          
006200*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
006300 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
006400                                                                          
006500 77  JA                          PIC X       VALUE 'J'.                   
006600 77  YES                         PIC X       VALUE 'Y'.                   
006700 77  NEJ                         PIC X       VALUE 'N'.                   
006800                                                                          
006900 77  INDX                        PIC S9(4)   VALUE +0  COMP SYNC.         
007000 77  MEAN-IX                     PIC S9(4)   VALUE +0  COMP SYNC.         
007100                                                                          
007200 77  VKTARA                      PIC S9(6)V9 VALUE +0  COMP-3.            
007300 77  WS-VKTARA                   PIC S9(6)V9 VALUE +0  COMP-3.            
007400 77  VKORDBTO-KOLLI              PIC S9(9)V9 VALUE +0  COMP-3.            
007500 77  VKORDNTO-KOLLI              PIC S9(9)V9 VALUE +0  COMP-3.            
007600 77  W-KVPB-TOT                  PIC S9(6)V9 VALUE ZERO COMP-3.           
007700 77  WS-VKORDNTO-DIFF            PIC S9(9)V9 VALUE +0  COMP-3.            
007800 77  WS-VKORDNTO-DIFF-POSITIV    PIC  9(9)V9 VALUE 0   COMP-3.            
007900 77  WS-VKORDNTO-KOLLI-TOT       PIC S9(9)V9 VALUE +0  COMP-3.            
008000 77  WS-VLORDNTO-KOLLI-TOT       PIC S9(8)V9(3) VALUE +0 COMP-3.          
008100 77  WS-VLORDNTO-KOLLI           PIC S9(4)V9(3) VALUE +0 COMP-3.          
008200 77  WS-VLORDBTO                 PIC S9(4)V9(3) VALUE ZERO COMP-3.        
008300 77  WS-VKORDBTO                 PIC S9(6)V9(1) VALUE ZERO COMP-3.        
008400 77  WS-VKORDNTO                 PIC S9(6)V9(1) VALUE ZERO COMP-3.        
008500 77  WS-KVLEVART                 PIC S9(5)   VALUE +0  COMP-3.            
008600 77  WS-KVLS                     PIC S9(7)   VALUE +0  COMP-3.            
008700 77  WS-KVOKS-TOT-CDC            PIC S9(9)   VALUE +0  COMP-3.            
008800 77  WS-KVDISP                   PIC S9(9)   VALUE +0  COMP-3.            
008900 77  WS-KDANMORS-UPPD            PIC X(2).                                
009000 77  WS-IDKOLLI                  PIC S9(5)   VALUE ZERO COMP-3.           
009100 77  WS-IDKUNDRF                 PIC X(10).                               
009200 77  WS-KVLEVANM-TOT             PIC S9(7)   VALUE ZERO COMP-3.           
009300 77  WS-DATUM                    PIC 9(6).                                
009400 77  WS-DATUM-Y2K                PIC 9(8).                                
009500 77  WS-RADPRIS                  PIC S9(7)V9(2) VALUE ZERO COMP-3.        
009600 77  WS-NYTT-RADPRIS             PIC S9(7)V9(2) VALUE ZERO COMP-3.        
009700 77  WS-PRKURS                   PIC S9(2)V9(5) VALUE +0   COMP-3.        
009800 77  WS-PRARTBTO-CONV            PIC S9(7)V9(2) VALUE ZERO COMP-3.        
009900 77  SPAR-KDANMORS               PIC X(2)   VALUE SPACE.                  
010000 77  SPAR-IDDC-RET               PIC X(2)   VALUE SPACE.                  
010100 77  SPAR-KDKRENOT-4104          PIC X(2)   VALUE SPACE.                  
010200 77  WS-KDVALISO-SPAR            PIC X(3)   VALUE SPACE.                  
010300                                                                          
010400 77  SPAR-ANM-IDDC-RET           PIC X(2)   VALUE SPACE.                  
010500 77  SPAR-ANM-IXDCCLEAR          PIC 9      VALUE ZERO.                   
010600 77  WS-CDC-SE                   PIC X(2)   VALUE '11'.                   
010700 77  WC-KDANMORS                 PIC X(8)   VALUE 'KDANMORS'.             
010800 77  WC-IDFKNGRP                 PIC X(8)   VALUE 'IDFKNGRP'.             
010900 77  WC-IDARTNR                  PIC X(8)   VALUE 'IDARTNR '.             
011000 77  WC-IDDC-EXCP                PIC X(8)   VALUE 'IDDC    '.             
011100 77  WS-ANM-IDDC-RET             PIC X(2)   VALUE SPACE.                  
011200 77  WS-ANM-IXDCCLEAR            PIC 9      VALUE ZERO.                   
011300                                                                          
011400 77  WS-IDANALYS-UPPD            PIC X(12).                               
011500 77  WS-IDKONTO-UPPD             PIC 9(11)   VALUE ZERO COMP-3.           
011600 77  WS-IDKST-UPPD               PIC X(10)   VALUE SPACE.                 
011700                                                                          
011800 77  WS-IDANSK                   PIC  9(3)      VALUE ZERO.               
011900 77  WS-VKART                    PIC S9(7)      VALUE ZERO.               
012000 77  WS-VLARTNTO                 PIC S9(8)V9(1) VALUE ZERO.               
012100                                                                          
012200 77  W-RR                        PIC X(3)    VALUE 'RR '.                 
012300 77  W-QR                        PIC X(3)    VALUE 'QR '.                 
012400 77  W-PR                        PIC X(3)    VALUE 'PR '.                 
012500                                                                          
012600 77  W-DATE-AAMM                 PIC  9(4)   VALUE ZERO.                  
012700 77  DUMMY-IDARTNR               PIC S9(9)   VALUE +100 COMP-3.           
012800 77  SW-KDKREBEH-Q               PIC X       VALUE 'N'.                   
012900 77  SW-KDKREBEH-P               PIC X       VALUE 'N'.                   
013000 77  SW-KDKREBEH-Y               PIC X       VALUE 'N'.                   
013300 77  KDANMORS-UPPD               PIC X       VALUE 'N'.                   
013400 77  SW-4721-HOPP                PIC X       VALUE 'N'.                   
013500 77  TRAEFF-SW                   PIC X       VALUE 'N'.                   
013600 77  SW-KDKREBEH-AENDRAD         PIC X       VALUE 'N'.                   
013700     88  KDKREBEH-AENDRAD                    VALUE 'J'.                   
013800                                                                          
013900 77  RETURRADER-KVAR-SW          PIC X       VALUE 'N'.                   
014000     88 RETURRADER-KVAR                      VALUE 'J'.                   
014100                                                                          
014200 77  IXDCCLEAR-2-SW              PIC X       VALUE 'N'.                   
014300     88  IXDCCLEAR-2-RAD-FINNS               VALUE 'J'.                   
014400                                                                          
014500 77  IXDCCLEAR-3-SW              PIC X       VALUE 'N'.                   
014600     88  IXDCCLEAR-3-RAD-FINNS               VALUE 'J'.                   
014700                                                                          
014800 77  ANGRA-NEKAD-RETUR-RAD-SW    PIC X      VALUE 'N'.                    
014900     88  ANGRA-NEKAD-RETUR-RAD              VALUE 'J'.                    
015000                                                                          
015100 77  ANGRA-NEKAD-RAD-SW          PIC X      VALUE 'N'.                    
015200     88  ANGRA-NEKAD-RAD                    VALUE 'J'.                    
015300                                                                          
015301 77  WS-FLAUTREM-FLAG            PIC X.                                   
015302     88  WS-FLAUTREM-Y                       VALUE 'J'.                   
015400     88  WS-FLAUTREM-N                       VALUE 'N'.                   
015410                                                                          
015500 77  GODK-KOD-SW                 PIC X.                                   
015600     88  GODK-KOD                            VALUE 'J'.                   
015700     88  EJ-GODK-KOD                         VALUE 'N'.                   
015800     EJECT                                                                
015900                                                                          
016000 77  GODK-ARTIKEL-SW             PIC X.                                   
016100     88  GODK-ARTIKEL                        VALUE 'J'.                   
016200     88  EJ-GODK-ARTIKEL                     VALUE 'N'.                   
016300     EJECT                                                                
016400                                                                          
016500 77  GODK-IDFKNGRP-SW            PIC X.                                   
016600     88  GODK-IDFKNGRP                       VALUE 'J'.                   
016700     88  EJ-GODK-IDFKNGRP                    VALUE 'N'.                   
016800     EJECT                                                                
016900                                                                          
017000 77  GODK-DC-ARTIKEL-SW          PIC X.                                   
017100     88  GODK-DC-ARTIKEL                     VALUE 'J'.                   
017200     88  EJ-GODK-DC-ARTIKEL                  VALUE 'N'.                   
017300     EJECT                                                                
017400                                                                          
017500 77  GODK-DC-LEV-SW              PIC X.                                   
017600     88  GODK-DC-LEV                         VALUE 'J'.                   
017700     88  EJ-GODK-DC-LEV                      VALUE 'N'.                   
017800     EJECT                                                                
017900                                                                          
018000 77  WDR501-UPD-SW                   PIC X.                               
018100   88  WDR501-FINNS                         VALUE 'J'.                    
018200   88  WDR501-SAKNAS                        VALUE 'N'.                    
018300                                                                          
018400 77  KNOTA-RAD-FINNS-SW          PIC X      VALUE 'N'.                    
018500     88  KNOTA-RAD-FINNS                    VALUE 'J'.                    
018600                                                                          
018700 77  OBEH-RADER-FINNS-SW         PIC X.                                   
018800   88  OBEH-RADER-FINNS                     VALUE 'J'.                    
018900                                                                          
019000 77  OBEH-REMISS-FINNS-SW        PIC X.                                   
019100   88  OBEH-REMISS-FINNS                    VALUE 'J'.                    
019200                                                                          
019300 77  BEH-RADER-FINNS-SW          PIC X.                                   
019400   88  BEH-RADER-FINNS                      VALUE 'J'.                    
019500                                                                          
019600 77  AVVISADE-RADER-SW           PIC X.                                   
019700   88  AVVISADE-RADER-FINNS                 VALUE 'J'.                    
019800                                                                          
019900 77  ANNULERADE-RADER-SW         PIC X.                                   
020000   88  ANNULERADE-RADER-FINNS               VALUE 'J'.                    
020100                                                                          
020200 77  GODKAENDA-RADER-SW          PIC X.                                   
020300   88  GODKAENDA-RADER-FINNS                VALUE 'J'.                    
020400                                                                          
020500 77  KDKREBEH-SW                 PIC X(3)   VALUE SPACE.                  
020600   88  OK-KOD                                                             
020700         VALUE 'N70' 'N71' 'N72' 'N73' 'N74' 'N75'                        
020800               'N76' 'N77' 'N78' 'N79' 'N80' 'J  '                        
020900               'Y  ' 'RR ' 'QR '.                                         
021000   88  OK-FELKOD                                                          
021100         VALUE 'N70' 'N71' 'N72' 'N73' 'N74' 'N75'                        
021200               'N76' 'N77' 'N78' 'N79' 'N80'.                             
021300   88  OK-GODKAEND                                                        
021400         VALUE 'J  ' 'Y  '.                                               
021500   88  OK-REMISS                                                          
021600         VALUE 'RR ' 'QR '.                                               
021700                                                                          
021800 77  WS-KDANMORS                 PIC X(2).                                
021900   88  WS-ANTAL-SKALL-FINNAS                 VALUE '00'                   
022000                                                   '20'                   
022100                                                   '25'                   
022200                                                   '30'                   
022300                                                   '31'                   
022400                                                   '42'                   
022500                                                   '43'                   
022600                                                   '72'                   
022700                                                   '73'                   
022800                                                   '84'.                  
022900                                                                          
023000 77  INDATA-SW                   PIC X       VALUE 'J'.                   
023100     88  INDATA-OK                           VALUE 'J'.                   
023200     88  INDATA-FEL                          VALUE 'N'.                   
023300                                                                          
023400 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
023500     88  NYCKLAR-OK                          VALUE 'J'.                   
023600     88  NYCKLAR-FEL                         VALUE 'N'.                   
023700                                                                          
023800 77  WL410901-SW                 PIC X       VALUE 'J'.                   
023900     88  WL410901-FINNS                      VALUE 'J'.                   
024000     88  WL410901-SAKNAS                     VALUE 'N'.                   
024100                                                                          
024200 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
024300     88  EGEN-MID                            VALUE '4722'.                
024400     88  HELP-MID                            VALUE '0551'.                
024500                                                                          
024600 01  TEST-KDKREBEH.                                                       
024700     03  KDKREBEH-1              PIC X(1)   VALUE SPACE.                  
024800     03  KDKREBEH-2              PIC X(1)   VALUE SPACE.                  
024900     03  KDKREBEH-3              PIC X(1)   VALUE SPACE.                  
025000                                                                          
025100 77  WDB6-SW                     PIC X       VALUE 'J'.                   
025200     88  WDB6-FINNS                          VALUE 'J'.                   
025300     88  WDB6-SAKNAS                         VALUE 'N'.                   
025400                                                                          
025500     EJECT                                                                
025600*    --- VALID IDDC CODES                                                 
025700*                                                                         
025800*01    -COPY WWDC99                                                       
025900*01    -COPY WWDCLAND                                                     
026000       EJECT                                                              
026100                                                                          
026200 01  TEST-IDDISTR                PIC 9(5)   VALUE ZERO COMP-3.            
026300*01  FILLER  -COPY WWDIST79      -RED TEST-IDDISTR.                       
026400     EJECT                                                                
026500*01  FILLER  -COPY WWDIST07      -RED TEST-IDDISTR.                       
026600     EJECT                                                                
026700*01  FILLER  -COPY WWDIST35      -RED TEST-IDDISTR.                       
026800     EJECT                                                                
026900*01  FILLER  -COPY WWDIST34      -RED TEST-IDDISTR.                       
027000     EJECT                                                                
027100*                                                                         
027200                                                                          
027300*01  -COPY WWIDFTG                                                        
027400                                                                          
027500*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
027600 01  GENERELLA-SUBPROGRAM.                                                
027700     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
027800     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
027900     03  W006PRT                 PIC X(8)    VALUE 'W006PRT '.            
028000     03  W418ANSV                PIC X(8)    VALUE 'W418ANSV'.            
028100     03  W418MERE                PIC X(8)    VALUE 'W418MERE'.            
028200     03  W418MEAN                PIC X(8)    VALUE 'W418MEAN'.            
028300     03  W418UDET                PIC X(8)    VALUE 'W418UDET'.            
028400     03  W418OKOD                PIC X(8)    VALUE 'W418OKOD'.            
028500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
028600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
028700     03  W418KTL3                PIC X(8)    VALUE 'W418KTL3'.            
028800     03  W335CURR                PIC X(8)    VALUE 'W335CURR'.            
028900     03  W510CURR                PIC X(8)    VALUE 'W510CURR'.            
029000     EJECT                                                                
029100 01  FILLER                      PIC X(16)   VALUE 'W006PRT '.            
029200*01  -COPY W006PRT                                                        
029300     EJECT                                                                
029400*    --- PARAMETRAR TILL SUBPROGRAM W418ANSV                              
029500 01  FILLER                      PIC X(16)   VALUE 'W418ANSV'.            
029600*01 -COPY W418ANSV                                                        
029700     EJECT                                                                
029800*    --- PARAMETRAR TILL SUBPROGRAM W418MERE                              
029900 01  FILLER                      PIC X(16)   VALUE 'W418MERE'.            
030000*01 -COPY W418MERE                                                        
030100     EJECT                                                                
030200*    --- PARAMETRAR TILL SUBPROGRAM W418MEAN                              
030300 01  FILLER                      PIC X(16)   VALUE 'W418MEAN'.            
030400*01 -COPY W418MEAN                                                        
030500     EJECT                                                                
030600*    --- PARAMETRAR TILL SUBPROGRAM W418UDET                              
030700 01  FILLER                      PIC X(16)   VALUE 'W418UDET'.            
030800*01 -COPY W418UDET                                                        
030900     EJECT                                                                
031000*    ---  LÄNKAREA TILL W418OKOD                                          
031100 01  FILLER                      PIC X(16)   VALUE 'W418OKOD'.            
031200*01 -COPY W418OKOD           -PRE OKOD-.                                  
031300     EJECT                                                                
031400*    ---  LÄNKAREA TILL W418KTL3                                          
031500 01  FILLER                      PIC X(16)   VALUE 'W418KTL3'.            
031600*01 -COPY W418KTL3                                                        
031700     EJECT                                                                
031800*    --- PARAMETRAR TILL SUBPROGRAM W335CURR                              
031900 01  FILLER                      PIC X(16)   VALUE 'W335CURR'.            
032000*01 -COPY W335CURR                                                        
032100     EJECT                                                                
032200*    --- PARAMETRAR TILL SUBPROGRAM W510CURR                              
032300 01  FILLER                      PIC X(16)   VALUE 'W510CURR'.            
032400*01 -COPY W510CURR                                                        
032500     EJECT                                                                
032600*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
032700 01  FILLER                      PIC X(16)   VALUE 'WMEDKONV'.            
032800*01 -COPY WMEDAREA                                                        
032900     EJECT                                                                
033000*    --- PARAMETRAR TILL COPYTEXT   WWOMVAND                              
033100*01 -COPY WWOMVAND                                                        
033200     SKIP3                                                                
033300 01  MESSAGE-CODES.                                                       
033400     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
033500     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
033600     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
033700     03  INF-PRESS-PF23          PIC X(3)    VALUE '206'.                 
033800     03  INF-PRESS-PF4           PIC X(3)    VALUE '081'.                 
033900     03  ERR-INFO-MISSING        PIC X(3)    VALUE '005'.                 
034000     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
034100     03  ERR-UPDATE-NOT-OK       PIC X(3)    VALUE '007'.                 
034200     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
034300     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
034400     03  INF-PRINT-BEG           PIC X(3)    VALUE '118'.                 
034500     03  INF-MATRIS-KONFLIKT     PIC X(3)    VALUE '291'.                 
034600     03  INF-MATRIS-KONFLIKT-PSN PIC X(3)    VALUE '292'.                 
034700     03  ERR-WRONG-PRINTER       PIC X(3)    VALUE '772'.                 
034800     03  ERR-PRIS-MISSING        PIC X(3)    VALUE '301'.                 
034900     03  ERR-EJ-BEHORIG-GODK-LA  PIC X(3)    VALUE '604'.                 
035000     03  ERR-FLERA-FUNKTIONER    PIC X(3)    VALUE '097'.                 
035100     03  ERR-RETUR-OK-SAKNAS     PIC X(3)    VALUE '345'.                 
035200     EJECT                                                                
035300*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
035400*                                                                         
035500 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
035600                                                                          
035700*01 -COPY WMSGINIT                                                        
035800                                                                          
035900*                                                                         
036000*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
036100*                                                                         
036200 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
036300                                                                          
036400*01  MID -COPY W4I72201                                                   
036500     EJECT                                                                
036600 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
036700                                                                          
036800*01  -COPY WMSGAREA                                                       
036900     EJECT                                                                
037000     03  MOD REDEFINES MSG-AREA.                                          
037100*      05  -COPY W4O72201    -PRE MOD-                                    
037200     EJECT                                                                
037300 01  FILLER                      PIC X(16)   VALUE 'KOM-IO-AREA'.         
037400                                                                          
037500 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
037600                                                                          
037700*01  -COPY WMFSAREA                                                       
037800     EJECT                                                                
037900 01  ALT-MSG-IO-AREA.                                                     
038000   03 ALT-LL                     PIC S9(4)  VALUE +17  COMP SYNC.         
038100   03 ALT-Z1                     PIC X.                                   
038200   03 ALT-Z2                     PIC X.                                   
038300   03 ALT-TRANSKOD               PIC X(8)   VALUE 'W4T721  '.             
038400   03 ALT-IDTRANS                PIC X(4)   VALUE '4722'.                 
038500   03 ALT-SPRAK                  PIC X.                                   
038600     EJECT                                                                
038700                                                                          
038800                                                                          
038900*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
039000*                                                                         
039100     EJECT                                                                
039200 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
039300                                                                          
039400                                                                          
039500 01  NYCKLAR-TILL-DLI.                                                    
039600                                                                          
039700     03  W-IDLEVANM-X.                                                    
039800         05  W-IDDISTR-A2        PIC S9(5)   COMP-3 VALUE ZERO.           
039900         05  W-IDKUNDNR-A2       PIC S9(7)   COMP-3 VALUE ZERO.           
040000         05  W-IDRAPPNR-A2       PIC  X(7)          VALUE ZERO.           
040100                                                                          
040200     03  W-WDA211KY-X.                                                    
040300         05  W-IDARTNR-A2        PIC S9(9)   COMP-3 VALUE ZERO.           
040400         05  W-IDRADNR-A2        PIC S9(5)   COMP-3 VALUE ZERO.           
040500                                                                          
040600     03  W-WDA2DSEQ-MIN-X.                                                
040700         05  W-IDFTG-DSEQ-FOM    PIC  X(2)   VALUE SPACE.                 
040800         05  W-IDARTNR-DSEQ-FOM  PIC S9(9)   COMP-3 VALUE ZERO.           
040900         05  W-IDDISTR-DSEQ-FOM  PIC S9(5)   COMP-3 VALUE ZERO.           
041000         05  W-IDKUNDNR-DSEQ-FOM PIC S9(7)   COMP-3 VALUE ZERO.           
041100         05  FILLER              PIC X(10)     VALUE LOW-VALUE.           
041200                                                                          
041300     03  W-WDA2DSEQ-MAX-X.                                                
041400         05  W-IDFTG-DSEQ-TOM    PIC  X(2)   VALUE SPACE.                 
041500         05  W-IDARTNR-DSEQ-TOM  PIC S9(9)   COMP-3 VALUE ZERO.           
041600         05  W-IDDISTR-DSEQ-TOM  PIC S9(5)   COMP-3 VALUE ZERO.           
041700         05  W-IDKUNDNR-DSEQ-TOM PIC S9(7)   COMP-3 VALUE ZERO.           
041800         05  FILLER              PIC X(10)    VALUE HIGH-VALUE.           
041900                                                                          
042000     03  W-IDFAKT-X.                                                      
042100         05 W-IDFAKT-L5          PIC S9(7)   COMP-3 VALUE ZERO.           
042200                                                                          
042300     03  W-IDGMTREF-X.                                                    
042400         05 W-IDDISTR-L5         PIC S9(5)   COMP-3 VALUE ZERO.           
042500         05 W-IDKUNDNR-L5        PIC S9(7)   COMP-3 VALUE ZERO.           
042600         05 W-IDKUNDRF-L5        PIC X(10).                               
042700                                                                          
042800     03  W-IDARTNR-L5-X.                                                  
042900         05 W-IDARTNR-L5         PIC S9(9)   VALUE ZERO  COMP-3.          
043000                                                                          
043100     03  W-WDL511KY-X.                                                    
043200         05  W-IDPRODNR-L5       PIC S9(7)   VALUE ZERO  COMP-3.          
043300         05  W-IDKOLLI-L5-X.                                              
043400           07 W-IDKOLLI-L5       PIC S9(5)   VALUE ZERO  COMP-3.          
043500                                                                          
043600     03  W-IDARTNR-X.                                                     
043700         05  W-IDARTNR           PIC S9(9)   COMP-3 VALUE ZERO.           
043800                                                                          
043900     03  W-IDDC-X.                                                        
044000         05  W-IDDC              PIC X(2).                                
044100                                                                          
044200     03  W-IDLAND-X.                                                      
044300         05  W-IDLAND            PIC X(2).                                
044400                                                                          
044500     03  W-IDDC-B6-X.                                                     
044600         05 W-IDDC-B6            PIC X(2)    VALUE SPACE.                 
044700                                                                          
044800     03  W-WDB611KY-X.                                                    
044900         05  W-URV-TEELMT            PIC X(16)  VALUE SPACE.              
045000         05  W-URV-FILLER            PIC X(20)  VALUE SPACE.              
045100         05  W-URV-IDARTNR-EXCP-FILLER REDEFINES W-URV-FILLER.            
045200           07  W-URV-IDARTNR-EXCP    PIC 9(9).                            
045300           07  FILLER                PIC X(11).                           
045400         05  W-URV-IDFKNGRP-EXCP-FILLER REDEFINES W-URV-FILLER.           
045500           07  W-URV-IDFKNGRP-EXCP   PIC 9(4).                            
045600           07  FILLER                PIC X(16).                           
045700         05  W-URV-KDANMORS-RET-FILLER REDEFINES W-URV-FILLER.            
045800           07  W-URV-KDANMORS-RET    PIC X(2).                            
045900           07  FILLER                PIC X(18).                           
046000         05  W-URV-IDDC-EXCP-FILLER REDEFINES W-URV-FILLER.               
046100           07  W-URV-IDDC-EXCP       PIC X(2).                            
046200           07  FILLER                PIC X(18).                           
046300                                                                          
046400     03  W-IDKOLLI-X.                                                     
046500         05  W-IDKOLLI           PIC S9(5)   COMP-3 VALUE ZERO.           
046600                                                                          
046700     03  W-KDSEGKEY-X.                                                    
046800         05  W-KDSEGKEY          PIC  X(1)          VALUE '1'.            
046900                                                                          
047000     03  W-KDKOLLI-X.                                                     
047100         05  W-KDKOLLI           PIC  X(8).                               
047200                                                                          
047300     03  W-IDSKYLT-X.                                                     
047400         05  W-IDSKYLT           PIC  X(3)   VALUE SPACE.                 
047500                                                                          
047600     03  W-KDARBTYP-X.                                                    
047700         05  W-KDARBTYP          PIC  X(8).                               
047800                                                                          
047900     03 W-4109-X.                                                         
048000         05 FILLER               PIC  X(4)   VALUE '4109'.                
048100         05 W-IDFTG              PIC  X(2)   VALUE SPACE.                 
048200         05 FILLER               PIC  X(24)  VALUE LOW-VALUE.             
048300                                                                          
048400     03 W-WDGXKEY-MIN-X.                                                  
048500         05 W-IDARTNR-4110-MIN   PIC S9(9)   VALUE ZERO COMP-3.           
048600         05 W-KDANMORS-4110-MIN  PIC X(2)    VALUE SPACE.                 
048700         05 FILLER               PIC X(8)    VALUE LOW-VALUE.             
048800                                                                          
048900     03 W-WDGXKEY-MAX-X.                                                  
049000         05 W-IDARTNR-4110-MAX   PIC S9(9)   VALUE ZERO COMP-3.           
049100         05 W-KDANMORS-4110-MAX  PIC X(2)    VALUE SPACE.                 
049200         05 FILLER               PIC X(8)    VALUE HIGH-VALUE.            
049300                                                                          
049400     03  W-IDPERSON-X.                                                    
049500         05  W-IDPERSON          PIC S9(3)   COMP-3 VALUE ZERO.           
049600                                                                          
049700*    -NYCKLAR TIL WDB201                                                  
049800     03  W-IDGMT-X.                                                       
049900       05  W-IDDISTR-WDB2        PIC S9(5) VALUE ZERO COMP-3.             
050000       05  W-IDKUNDNR-WDB2       PIC S9(7) VALUE ZERO COMP-3.             
050100     03  W-IDGMT-MIN-X.                                                   
050200       05  W-IDDISTR-WDB2-MIN    PIC S9(5) VALUE ZERO COMP-3.             
050300       05  W-IDKUNDNR-WDB2-MIN   PIC S9(7) VALUE ZERO COMP-3.             
050400     03  W-IDGMT-MAX-X.                                                   
050500       05  W-IDDISTR-WDB2-MAX    PIC S9(5) VALUE ZERO COMP-3.             
050600       05  W-IDKUNDNR-WDB2-MAX   PIC S9(7) VALUE ZERO COMP-3.             
050700                                                                          
050800* TILL WDB101                                                             
050900     03  W-WDB101KY-X.                                                    
051000       05  W-WDB1-IDPARTNR       PIC X(9)  VALUE SPACE.                   
051100       05  W-WDB1-IDFTG          PIC 9(2)  VALUE ZERO.                    
051200                                                                          
051300* TILL WDR5 ATTESTANSVARIGTABELL KREDITNOTOR                              
051400     03  W-WDGXKEY-6327-X.                                                
051500         05  W-IDHTYP-6327       PIC X(4)    VALUE '6327'.                
051600         05  W-KDARBTYP-6327     PIC X(8)    VALUE 'DISC    '.            
051700         05  W-IDDC-6327         PIC X(2)    VALUE SPACE.                 
051800         05  FILLER              PIC X(16)   VALUE LOW-VALUE.             
051900                                                                          
052000     03  W-KY6328-MIN-X.                                                  
052100         05  W-SUBEL-6328-MIN    PIC 9(7)    VALUE ZERO.                  
052200         05  W-IDUSER-6328-MIN   PIC X(8)    VALUE LOW-VALUE.             
052300                                                                          
052400     03  W-KY6328-MAX-X.                                                  
052500         05  W-SUBEL-6328-MAX    PIC 9(7)    VALUE 9999999.               
052600         05  W-IDUSER-6328-MAX   PIC X(8)    VALUE HIGH-VALUE.            
052700                                                                          
052800     03  W-IDUSER-6328-X.                                                 
052900         05  W-IDUSER-GODK-6328  PIC X(8)    VALUE SPACE.                 
053000                                                                          
053100* TILL WDR5 ATTEST AV KREDITNOTOR                                         
053200     03  W-WDGXKEY-4103-X.                                                
053300         05  W-IDHTYP-4103       PIC X(4)    VALUE '4103'.                
053400         05  W-IDDISTR-4103      PIC S9(5)   VALUE ZERO COMP-3.           
053500         05  W-IDKUNDNR-4103     PIC S9(7)   VALUE ZERO COMP-3.           
053600         05  W-IDRAPPNR-4103     PIC  9(7)   VALUE ZERO.                  
053700         05  FILLER              PIC X(12)   VALUE LOW-VALUE.             
053800                                                                          
053900     03  W-WDGXKEY-4104-X.                                                
054000         05  W-IDDC-4104         PIC X(2)    VALUE SPACE.                 
054100         05  W-KDKRENOT-4104     PIC X(2)    VALUE SPACE.                 
054200                                                                          
054300     EJECT                                                                
054400*    --- STATUS-KOD FRÅN IMS                                              
054500 01  STATUS-WS                   PIC XX.                                  
054600     88  STATUS-OK                           VALUE '  '.                  
054700     88  SEGMENT-FINNS                       VALUE '  '.                  
054800     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
054900     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
055000                                                                          
055100 01  GODK-STATUSKODER.                                                    
055200     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
055300                                                                          
055400 01  SSA1                        PIC X(192).                              
055500 01  SSA2                        PIC X(64).                               
055600 01  SSA3                        PIC X(64).                               
055700     EJECT                                                                
055800*    --- IMS FUNKTIONSKODER                                               
055900*01  -COPY W0003                                                          
056000     EJECT                                                                
056100*    ---  DLI INPUT-OUTPUT AREA                                           
056200 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
056300                                                                          
056400 01  DLI-IO-AREA.                                                         
056500     03  IO-AREA                 PIC X(300)  VALUE SPACE.                 
056600     03  WLKREE01 REDEFINES IO-AREA.                                      
056700*        05  -COPY WDA201                                                 
056800     03  WLKREE11 REDEFINES IO-AREA.                                      
056900*        05  -COPY WDA211                                                 
057000     EJECT                                                                
057100 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA2'.        
057200 01  DLI-IO-AREA2.                                                        
057300     03  IO-AREA2                PIC X(1200) VALUE SPACE.                 
057400     03  WLKREE21 REDEFINES IO-AREA2.                                     
057500*        05  -COPY WDA221                                                 
057600     EJECT                                                                
057700     03  WLKREI01 REDEFINES IO-AREA2.                                     
057800*        05  -COPY WDA2D1                                                 
057900     EJECT                                                                
058000     03  WLARTC01 REDEFINES IO-AREA2.                                     
058100*        05  -COPY WDK601                                                 
058200     EJECT                                                                
058300     03  WLEMBB01 REDEFINES IO-AREA2.                                     
058400*        05  -COPY WDK501                                                 
058500     EJECT                                                                
058600     03  WLARTC11 REDEFINES IO-AREA2.                                     
058700*        05  -COPY WDK611                                                 
058800     EJECT                                                                
058900     03  WLARTM01 REDEFINES IO-AREA2.                                     
059000*        05  -COPY WDK901                                                 
059100     EJECT                                                                
059200     03  WLBENA11 REDEFINES IO-AREA2.                                     
059300*        05  -COPY WDD311                                                 
059400     EJECT                                                                
059500 01  FILLER                      PIC X(20) VALUE 'DLI-IO-WDL501'.         
059600                                                                          
059700 01  DLI-IO-WDL501.                                                       
059800*    03  -COPY WDL501                                                     
059900     EJECT                                                                
060000 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDL511'.         
060100 01  DLI-IO-WDL511.                                                       
060200*    03  -COPY WDL511                                                     
060300     EJECT                                                                
060400 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDL521'.         
060500 01  DLI-IO-WDL521.                                                       
060600*    03  -COPY WDL521                                                     
060700     EJECT                                                                
060800 01  FILLER                      PIC X(20) VALUE                          
060900                                          'DLI-IO-AREA-WDK711'.           
061000 01  DLI-IO-AREA-K711.                                                    
061100     03  WDK711.                                                          
061200*        05  -COPY WDK711                                                 
061300     EJECT                                                                
061400 01  FILLER                      PIC X(20) VALUE                          
061500                                          'DLI-IO-AREA-WDK712'.           
061600 01  DLI-IO-AREA-K712.                                                    
061700     03  WDK712.                                                          
061800*        05  -COPY WDK712                                                 
061900     EJECT                                                                
062000 01  FILLER                      PIC X(20) VALUE                          
062100                                          'DLI-IO-AREA-WDK722'.           
062200 01  DLI-IO-AREA-K722.                                                    
062300     03  WDK722.                                                          
062400*        05  -COPY WDK722                                                 
062500     EJECT                                                                
062600 01  FILLER                      PIC X(20) VALUE                          
062700                                          'DLI-IO-AREA-4109'.             
062800 01  DLI-IO-AREA-4109.                                                    
062900     03  WL410901.                                                        
063000*        05  -COPY WDGX4109                                               
063100     EJECT                                                                
063200 01  FILLER                      PIC X(20)  VALUE                         
063300                                          'DLI-IO-AREA-4110'.             
063400 01  DLI-IO-AREA-4110.                                                    
063500     03  WL410911.                                                        
063600*        05  -COPY WDGX4110                                               
063700     EJECT                                                                
063800 01  FILLER                PIC X(20) VALUE 'DLI-IO-AREA-4113'.            
063900                                                                          
064000 01  DLI-IO-AREA-4113.                                                    
064100     03  WL411301.                                                        
064200*        05  -COPY WDGX4113                                               
064300     EJECT                                                                
064400*                                                                         
064500 01  FILLER                PIC X(20) VALUE 'DLI-IO-AREA-4114'.            
064600                                                                          
064700 01  DLI-IO-AREA-4114.                                                    
064800     03  WL411311.                                                        
064900*        05  -COPY WDGX4114                                               
065000     EJECT                                                                
065100*                                                                         
065200 01  FILLER                PIC X(20) VALUE 'DLI-IO-AREA-4115'.            
065300                                                                          
065400 01  DLI-IO-AREA-4115.                                                    
065500     03  WL411501.                                                        
065600*        05  -COPY WDGX4115                                               
065700     EJECT                                                                
065800*                                                                         
065900 01  FILLER                PIC X(20) VALUE 'DLI-IO-AREA-4116'.            
066000                                                                          
066100 01  DLI-IO-AREA-4116.                                                    
066200     03  WL411511.                                                        
066300*        05  -COPY WDGX4116                                               
066400     EJECT                                                                
066500*                                                                         
066600 01  FILLER                PIC X(20) VALUE 'DLI-IO-AREA-P311'.            
066700                                                                          
066800 01  DLI-IO-AREA-P311.                                                    
066900*    03  -COPY WDP311                                                     
067000                                                                          
067100 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDB201'.           
067200 01  DLI-IO-WDB201.                                                       
067300*     03  -COPY WDB201.                                                   
067400     EJECT                                                                
067500 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDB101'.           
067600 01  DLI-IO-WDB101.                                                       
067700*     03  -COPY WDB101.                                                   
067800                                                                          
067900 01  FILLER                    PIC X(16)   VALUE 'WDB601 AREA'.           
068000 01   DLI-IO-AREA-B601.                                                   
068100*     03  -COPY WDB601                                                    
068200     EJECT                                                                
068300                                                                          
068400 01  FILLER                    PIC X(16)   VALUE 'DLI-IO-WDB611'.         
068500 01  DLI-IO-WDB611.                                                       
068600*    03  -COPY WDB611                                                     
068700     EJECT                                                                
068800*                                                                         
068900 01  FILLER                PIC X(16) VALUE 'DLI-IO-WDGX6327'.             
069000 01  DLI-IO-WDGX6327.                                                     
069100*    03  -COPY WDGX6327                                                   
069200     EJECT                                                                
069300*                                                                         
069400 01  FILLER                PIC X(16) VALUE 'DLI-IO-WDGX6328'.             
069500 01  DLI-IO-WDGX6328.                                                     
069600*    03  -COPY WDGX6328                                                   
069700     EJECT                                                                
069800*                                                                         
069900 01  FILLER                PIC X(16) VALUE 'DLI-IO-WDGX4103'.             
070000 01  DLI-IO-WDGX4103.                                                     
070100*    03  -COPY WDGX4103                                                   
070200     EJECT                                                                
070300*                                                                         
070400 01  FILLER                PIC X(16) VALUE 'DLI-IO-WDGX4104'.             
070500 01  DLI-IO-WDGX4104.                                                     
070600*    03  -COPY WDGX4104                                                   
070700     EJECT                                                                
070800 LINKAGE SECTION.                                                         
070900*01  -COPY W0009   -PRE MSG-                                              
071000*01  -COPY W0009   -PRE ALT-                                              
071100     EJECT                                                                
071200*01  -COPY W0009   -PRE MAIL-                                             
071300     EJECT                                                                
071400*01  -COPY W0009   -PRE 4721-                                             
071500*01  -COPY W0008   -PRE USEA-                                             
071600     05  FILLER                  PIC X.                                   
071700     EJECT                                                                
071800*01  -COPY W0008   -PRE KREE-                                             
071900     05  FILLER                  PIC X.                                   
072000     EJECT                                                                
072100*01  -COPY W0008   -PRE WDL5-                                             
072200     05  FILLER                  PIC X.                                   
072300     EJECT                                                                
072400*01  -COPY W0008   -PRE ARTC-                                             
072500     05  FILLER                  PIC X.                                   
072600     EJECT                                                                
072700*01  -COPY W0008   -PRE ARTM-                                             
072800     05  FILLER                  PIC X.                                   
072900     EJECT                                                                
073000*01  -COPY W0008   -PRE WDK7-                                             
073100     05  FILLER                  PIC X.                                   
073200     EJECT                                                                
073300*01  -COPY W0008   -PRE BENA-                                             
073400     05  FILLER                  PIC X.                                   
073500     EJECT                                                                
073600*01  -COPY W0008   -PRE 4113-                                             
073700     05  FILLER                  PIC X.                                   
073800     EJECT                                                                
073900*01  -COPY W0008   -PRE WDP3-                                             
074000     05  FILLER                  PIC X.                                   
074100     EJECT                                                                
074200*01  -COPY W0008   -PRE WDG2-                                             
074300     05  FILLER                  PIC X.                                   
074400     EJECT                                                                
074500*01  -COPY W0008   -PRE KREI-                                             
074600     05  FILLER                  PIC X.                                   
074700     EJECT                                                                
074800*01  -COPY W0008   -PRE EMBB-                                             
074900     05  FILLER                  PIC X.                                   
075000     EJECT                                                                
075100*01  -COPY W0008   -PRE 4109-                                             
075200     05  FILLER                  PIC X.                                   
075300     EJECT                                                                
075400*01  -COPY W0008   -PRE 4115-                                             
075500     05  FILLER                  PIC X.                                   
075600     EJECT                                                                
075700*01  -COPY W0008   -PRE 4117-                                             
075800     05  FILLER                  PIC X.                                   
075900     EJECT                                                                
076000*01  -COPY W0008      -PRE WDB1-                                          
076100     05  FILLER                  PIC X.                                   
076200     EJECT                                                                
076300*01  -COPY W0008      -PRE WDB2-                                          
076400     05  FILLER                  PIC X.                                   
076500                                                                          
076600*01  -COPY W0008      -PRE WDB6-                                          
076700     05  FILLER                  PIC X.                                   
076800     EJECT                                                                
076900*01  -COPY W0008      -PRE 6327-                                          
077000     05  FILLER                  PIC X.                                   
077100     EJECT                                                                
077200*01  -COPY W0008      -PRE 4103-                                          
077300     05  FILLER                  PIC X.                                   
077400     EJECT                                                                
077500 01  KTL3-WDA8-PCB               PIC X.                                   
077600 01  KTL3-WDB2-PCB               PIC X.                                   
077700 01  KTL3-WDK6-PCB               PIC X.                                   
077800 01  KTL3-WDK7-PCB               PIC X.                                   
077900 01  KTL3-WDB6-PCB               PIC X.                                   
078000 01  KTL3-1165-PCB               PIC X.                                   
078100     EJECT                                                                
078200 PROCEDURE DIVISION  USING MSG-PCB  ALT-PCB  MAIL-PCB                     
078300                           4721-PCB                                       
078400                           USEA-PCB                                       
078500                           KREE-PCB WDL5-PCB ARTC-PCB                     
078600                           ARTM-PCB                                       
078700                           WDK7-PCB                                       
078800                           BENA-PCB                                       
078900                           4113-PCB                                       
079000                           WDP3-PCB WDG2-PCB                              
079100                           KREI-PCB                                       
079200                           EMBB-PCB                                       
079300                           4109-PCB                                       
079400                           4115-PCB                                       
079500                           4117-PCB                                       
079600                           WDB1-PCB                                       
079700                           WDB2-PCB                                       
079800                           WDB6-PCB                                       
079900                           6327-PCB                                       
080000                           4103-PCB                                       
080100                      KTL3-WDA8-PCB                                       
080200                      KTL3-WDB2-PCB                                       
080300                      KTL3-WDK6-PCB                                       
080400                      KTL3-WDK7-PCB                                       
080500                      KTL3-WDB6-PCB                                       
080600                      KTL3-1165-PCB.                                      
080700 MAIN SECTION.                                                            
080800     ENTRY 'DLITCBL' USING MSG-PCB  ALT-PCB  MAIL-PCB                     
080900                           4721-PCB                                       
081000                           USEA-PCB                                       
081100                           KREE-PCB WDL5-PCB ARTC-PCB                     
081200                           ARTM-PCB                                       
081300                           WDK7-PCB                                       
081400                           BENA-PCB                                       
081500                           4113-PCB                                       
081600                           WDP3-PCB WDG2-PCB                              
081700                           KREI-PCB                                       
081800                           EMBB-PCB                                       
081900                           4109-PCB                                       
082000                           4115-PCB                                       
082100                           4117-PCB                                       
082200                           WDB1-PCB                                       
082300                           WDB2-PCB                                       
082400                           WDB6-PCB                                       
082500                           6327-PCB                                       
082600                           4103-PCB                                       
082700                      KTL3-WDA8-PCB                                       
082800                      KTL3-WDB2-PCB                                       
082900                      KTL3-WDK6-PCB                                       
083000                      KTL3-WDK7-PCB                                       
083100                      KTL3-WDB6-PCB                                       
083200                      KTL3-1165-PCB.                                      
083300                                                                          
083400     PERFORM IMS-GET-MSG                                                  
083500     IF SEGMENT-FINNS                                                     
083600       PERFORM A-INIT                                                     
083700       PERFORM B-KOLLA-NYCKLAR                                            
083800       IF NYCKLAR-OK                                                      
083900         IF MFS-UPDATE OR MFS-UPD-V                                       
084000           MOVE MSGI-IDFTG  TO WS-IDFTG                                   
084100           IF IDFTG-PV OR IDFTG-NON-VCC                                   
084200             PERFORM C-GODK-ADM-KONTROLL                                  
084300           ELSE                                                           
084400             MOVE JA TO INDATA-SW                                         
084500           END-IF                                                         
084600           IF INDATA-OK                                                   
084700             PERFORM G-KOLLA-INPUT                                        
084800             IF INDATA-OK                                                 
084900                PERFORM H-UPPDATERA                                       
085000             END-IF                                                       
085100           END-IF                                                         
085200         ELSE                                                             
085300            IF MFS-PRINT                                                  
085400               PERFORM I-SKRIV-DETALJLISTA                                
085500            ELSE                                                          
085600              IF EGEN-MID OR HELP-MID                                     
085700                CONTINUE                                                  
085800              ELSE                                                        
085900                PERFORM MFS-RENSA-FAELT-UT                                
086000              END-IF                                                      
086100            END-IF                                                        
086200         END-IF                                                           
086300         IF SW-4721-HOPP        = JA                                      
086400           PERFORM J-4721-HOPP                                            
086500         ELSE                                                             
086600           PERFORM F-LAES-VISA-INFO                                       
086700         END-IF                                                           
086800       END-IF                                                             
086900       IF SW-4721-HOPP          = NEJ                                     
087000         COMPUTE MSG-KVLL = LENGTH OF MOD-W4O72201 + 4                    
087100         PERFORM IMS-INSERT-MSG                                           
087200       END-IF                                                             
087300     END-IF                                                               
087400     MOVE ZERO                  TO RETURN-CODE                            
087500     GOBACK                                                               
087600     .                                                                    
087700     EJECT                                                                
087800 A-INIT                         SECTION.                                  
087900                                                                          
088000     IF MSG-DUBBLA-TRANSKODER                                             
088100       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I72201                 
088200       MOVE MSG-IDTRANS-2                 TO MFS-IDTRANS                  
088300       MOVE MSG-KDMFSFOR-2                TO MFS-KDMFSFOR                 
088400     ELSE                                                                 
088500       MOVE MSG-INDATA-MINUS-1-TRANSKOD   TO MID-W4I72201                 
088600       MOVE MSG-IDTRANS-1                 TO MFS-IDTRANS                  
088700       MOVE MSG-KDMFSFOR-1                TO MFS-KDMFSFOR                 
088800     END-IF                                                               
088900                                                                          
089000     MOVE MSG-KDTRTYP           TO MFS-KDTRTYP                            
089100     MOVE MSG-IDPFK             TO MFS-IDPFK                              
089200     MOVE MFS-IDTRANS           TO W-IDTRANS                              
089300                                                                          
089400     MOVE LOW-VALUE             TO MSG-AREA                               
089500     MOVE 'W4O72201'            TO MFS-IDMOD                              
089600     MOVE '4722'                TO MOD-IDTRANS                            
089700     MOVE MFS-RENSA-FAELT       TO MOD-TEMFSFEL MOD-TEMFSINF              
089800                                                                          
089900     ACCEPT WS-DATUM            FROM DATE                                 
090000     MOVE FUNCTION CURRENT-DATE (1:8) TO WS-DATUM-Y2K                     
090100                                                                          
090200     MOVE JA                    TO INDATA-SW                              
090300                                                                          
090400     IF EGEN-MID OR HELP-MID                                              
090500       CONTINUE                                                           
090600     ELSE                                                                 
090700       MOVE SPACE               TO MFS-KDTRTYP                            
090800       MOVE '7'                 TO MFS-IDPFK                              
090900     END-IF                                                               
091000                                                                          
091100     MOVE  LOW-VALUE         TO W-IDGMT-MIN-X                             
091200                                                                          
091300     MOVE HIGH-VALUE         TO W-IDGMT-MAX-X                             
091400                                                                          
091500     MOVE +1                    TO INDX                                   
091600     PERFORM UNTIL INDX      >  13                                        
091700        MOVE ZERO               TO MEAN-IDDISTR  (INDX)                   
091800                                   MEAN-IDKUNDNR (INDX)                   
091900                                   MEAN-IDRAPPNR (INDX)                   
092000                                   MEAN-IDARTNR  (INDX)                   
092100                                   MEAN-IDRADNR  (INDX)                   
092200        MOVE SPACE              TO MEAN-KDKREBEH (INDX)                   
092300        MOVE +1                 TO MEAN-IX                                
092400        PERFORM UNTIL MEAN-IX > 3                                         
092500          MOVE SPACE         TO MEAN-TEANMNOT-ADM (INDX, MEAN-IX)         
092600                                MEAN-TEANMNOT-REM (INDX, MEAN-IX)         
092700          ADD +1                TO MEAN-IX                                
092800        END-PERFORM                                                       
092900        ADD +1                  TO INDX                                   
093000     END-PERFORM                                                          
093100                                                                          
093200     MOVE FUNCTION CURRENT-DATE (3:2) TO W-DATE-AAMM(1:2)                 
093300     MOVE FUNCTION CURRENT-DATE (5:2) TO W-DATE-AAMM(3:2)                 
093400     .                                                                    
093500     EJECT                                                                
093600 B-KOLLA-NYCKLAR                SECTION.                                  
093700                                                                          
093800     MOVE ALL '+'               TO MSGI-WMSGINIT                          
093900     MOVE '001'                 TO MSGI-KDCALL                            
094000     IF EGEN-MID                                                          
094100        MOVE MID-IDDISTR-IN     TO MSGI-IDDISTR                           
094200        MOVE MID-IDKUNDNR-IN    TO MSGI-IDKUNDNR                          
094300        MOVE MID-IDRAPPNR-IN    TO MSGI-IDRAPPNR                          
094400        MOVE MID-IDARTNR-IN     TO MSGI-IDARTNR                           
094500        MOVE MID-IDRADNR-IN     TO MSGI-IDRADNR                           
094600     END-IF                                                               
094700     MOVE MSG-SIGNON-USERID     TO MSGI-IDUSER                            
094800     MOVE '4722'            TO MSGI-IDTRANS                               
094900     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
095000     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
095100                                                                          
095200     IF MSGI-IDLAND-SPR = 'GB'                                            
095300       MOVE 'GB '           TO MED-IDSKYLT                                
095400                                 W-IDSKYLT                                
095500     ELSE                                                                 
095600       MOVE 'S  '           TO MED-IDSKYLT                                
095700                                 W-IDSKYLT                                
095800     END-IF                                                               
095900                                                                          
096000     MOVE JA                    TO NYCKLAR-SW                             
096100                                                                          
096200     PERFORM BA-KOLLA-IDDISTR                                             
096300     PERFORM BB-KOLLA-IDKUNDNR                                            
096400     PERFORM BC-KOLLA-IDRAPPNR                                            
096500     PERFORM BD-KOLLA-IDARTNR                                             
096600     PERFORM BE-KOLLA-IDRADNR                                             
096700                                                                          
096800     IF NYCKLAR-FEL                                                       
096900       MOVE ERR-WRONG-KEY       TO MED-IDMFSFEL                           
097000       CALL WMEDKONV USING MED-WMEDAREA                                   
097100       MOVE MED-MFSFEL          TO MOD-TEMFSFEL                           
097200       PERFORM MFS-RENSA-FAELT-UT                                         
097300     END-IF                                                               
097400     .                                                                    
097500     EJECT                                                                
097600 BA-KOLLA-IDDISTR               SECTION.                                  
097700                                                                          
097800     MOVE MFS-RENSA-FAELT       TO MOD-IDDISTR-IN                         
097900                                                                          
098000     IF MID-IDDISTR-IN          NOT = ALL '+'                             
098100       MOVE '7'                 TO MFS-IDPFK                              
098200       MOVE SPACE               TO MFS-KDTRTYP                            
098300     END-IF                                                               
098400                                                                          
098500     IF MSGI-IDDISTR NUMERIC AND MSGI-IDDISTR > ZERO                      
098600       MOVE MSGI-IDDISTR        TO W-IDDISTR-A2                           
098700                                   MOD-IDDISTR-UT                         
098800                                   W-IDDISTR-WDB2                         
098900                                   W-IDDISTR-4103                         
099000                                   TEST-IDDISTR                           
099100       INSPECT MOD-IDDISTR-UT  REPLACING LEADING ZERO BY SPACE            
099200     ELSE                                                                 
099300       MOVE NEJ                 TO NYCKLAR-SW                             
099400     END-IF                                                               
099500                                                                          
099600     .                                                                    
099700     EJECT                                                                
099800                                                                          
099900 BB-KOLLA-IDKUNDNR              SECTION.                                  
100000                                                                          
100100     MOVE MFS-RENSA-FAELT       TO MOD-IDKUNDNR-IN                        
100200                                                                          
100300     IF MID-IDKUNDNR-IN         NOT = ALL '+'                             
100400       MOVE '7'                 TO MFS-IDPFK                              
100500       MOVE SPACE               TO MFS-KDTRTYP                            
100600     END-IF                                                               
100700                                                                          
100800     IF MSGI-IDKUNDNR           NUMERIC                                   
100900       MOVE MSGI-IDKUNDNR       TO W-IDKUNDNR-A2                          
101000                                   MOD-IDKUNDNR-UT                        
101100                                   W-IDKUNDNR-WDB2                        
101200                                   W-IDKUNDNR-4103                        
101300       INSPECT MOD-IDKUNDNR-UT REPLACING LEADING ZERO BY SPACE            
101400     ELSE                                                                 
101500       MOVE NEJ                 TO NYCKLAR-SW                             
101600     END-IF                                                               
101700                                                                          
101800     .                                                                    
101900     EJECT                                                                
102000 BC-KOLLA-IDRAPPNR              SECTION.                                  
102100                                                                          
102200     MOVE MFS-RENSA-FAELT       TO MOD-IDRAPPNR-IN                        
102300                                                                          
102400     IF MID-IDRAPPNR-IN         NOT = ALL '+'                             
102500       MOVE '7'                 TO MFS-IDPFK                              
102600       MOVE SPACE               TO MFS-KDTRTYP                            
102700     END-IF                                                               
102800                                                                          
102900     IF MSGI-IDRAPPNR           =  SPACE                                  
103000       MOVE NEJ                 TO NYCKLAR-SW                             
103100     ELSE                                                                 
103200       MOVE MSGI-IDRAPPNR       TO W-IDRAPPNR-A2                          
103300                                   W-IDRAPPNR-4103                        
103400     END-IF                                                               
103500     MOVE MSGI-IDRAPPNR         TO MOD-IDRAPPNR-UT                        
103600     INSPECT MOD-IDRAPPNR-UT REPLACING LEADING ZERO BY SPACE              
103700                                                                          
103800     .                                                                    
103900     EJECT                                                                
104000 BD-KOLLA-IDARTNR               SECTION.                                  
104100                                                                          
104200     MOVE MFS-RENSA-FAELT       TO MOD-IDARTNR-IN                         
104300                                                                          
104400     IF MID-IDARTNR-IN          NOT = ALL '+'                             
104500       MOVE '7'                 TO MFS-IDPFK                              
104600       MOVE SPACE               TO MFS-KDTRTYP                            
104700     END-IF                                                               
104800                                                                          
104900     IF MSGI-IDARTNR            NUMERIC AND MSGI-IDARTNR > ZERO           
105000       MOVE MSGI-IDARTNR        TO W-IDARTNR                              
105100                                   W-IDARTNR-A2                           
105200                                   MOD-IDARTNR-UT                         
105300       INSPECT MOD-IDARTNR-UT  REPLACING LEADING ZERO BY SPACE            
105400     ELSE                                                                 
105500       MOVE NEJ                 TO NYCKLAR-SW                             
105600       MOVE MSGI-IDARTNR        TO MOD-IDARTNR-UT                         
105700       INSPECT MOD-IDARTNR-UT  REPLACING LEADING ZERO BY SPACE            
105800     END-IF                                                               
105900                                                                          
106000     .                                                                    
106100     EJECT                                                                
106200 BE-KOLLA-IDRADNR               SECTION.                                  
106300                                                                          
106400     MOVE MFS-RENSA-FAELT       TO MOD-IDRADNR-IN                         
106500                                                                          
106600     IF MID-IDRADNR-IN          NOT = ALL '+'                             
106700       MOVE '7'                 TO MFS-IDPFK                              
106800       MOVE SPACE               TO MFS-KDTRTYP                            
106900     END-IF                                                               
107000                                                                          
107100     IF MSGI-IDRADNR            NUMERIC                                   
107200       MOVE MSGI-IDRADNR        TO W-IDRADNR-A2                           
107300                                   MOD-IDRADNR-UT                         
107400       INSPECT MOD-IDRADNR-UT  REPLACING LEADING ZERO BY SPACE            
107500     ELSE                                                                 
107600       MOVE NEJ                 TO NYCKLAR-SW                             
107700     END-IF                                                               
107800                                                                          
107900     .                                                                    
108000     EJECT                                                                
108100 C-GODK-ADM-KONTROLL  SECTION.                                            
108200                                                                          
108300     IF MID-KDANMORS-UPPD = ALL '+'  AND                                  
108400        MID-KDKREBEH = ALL '+'                                            
108500       CONTINUE                                                           
108600     ELSE                                                                 
108700       MOVE JA                    TO INDATA-SW                            
108800                                                                          
108900       PERFORM IMS-GU-WLKREE11-KVAL                                       
109000       MOVE LEV-IDDC    TO WS-IDDC                                        
109100       IF NDC-PACIFIC OR                                                  
109200          NDC-NX OR                                                       
109300          NDC-NS                                                          
109400         CONTINUE                                                         
109500       ELSE                                                               
109600         IF SEGMENT-FINNS                                                 
109700           MOVE 'DISC'          TO W-KDARBTYP-6327                        
109800           MOVE LEV-IDDC        TO W-IDDC-6327                            
109900           MOVE LOW-VALUE       TO W-IDUSER-6328-MIN                      
110000           MOVE ZERO            TO W-SUBEL-6328-MIN                       
110100           MOVE HIGH-VALUE      TO W-IDUSER-6328-MAX                      
110200           MOVE 9999999         TO W-SUBEL-6328-MAX                       
110300           MOVE MSGI-IDUSER     TO W-IDUSER-GODK-6328                     
110400                                                                          
110500           PERFORM IMS-GU-WDGX6327                                        
110600           IF SEGMENT-FINNS                                               
110700             PERFORM IMS-GNP-WDGX6328                                     
110800             IF SEGMENT-SAKNAS                                            
110900               IF MID-KDKREBEH NOT = ALL '+'                              
111000                 MOVE MFS-ALFA-FAELT-FEL TO                               
111100                                    MOD-KDKREBEH-ATTR                     
111200                 MOVE MFS-ROER-EJ-FAELT TO MOD-KDKREBEH                   
111300               ELSE                                                       
111400                 MOVE MFS-ALFA-FAELT-FEL TO                               
111500                                    MOD-KDANMORS-UPPD-ATTR                
111600                 MOVE MFS-ROER-EJ-FAELT TO MOD-KDANMORS-UPPD              
111700               END-IF                                                     
111800               MOVE NEJ                    TO INDATA-SW                   
111900               MOVE ERR-EJ-BEHORIG-GODK-LA TO MED-IDMFSFEL                
112000               CALL WMEDKONV USING MED-WMEDAREA                           
112100               MOVE MED-MFSFEL         TO MOD-TEMFSFEL                    
112200             END-IF                                                       
112300           ELSE                                                           
112400             IF MID-KDKREBEH NOT = ALL '+'                                
112500               MOVE MFS-ALFA-FAELT-FEL TO                                 
112600                                  MOD-KDKREBEH-ATTR                       
112700               MOVE MFS-ROER-EJ-FAELT TO MOD-KDKREBEH                     
112800             ELSE                                                         
112900               MOVE MFS-ALFA-FAELT-FEL TO                                 
113000                                  MOD-KDANMORS-UPPD-ATTR                  
113100               MOVE MFS-ROER-EJ-FAELT TO MOD-KDANMORS-UPPD                
113200             END-IF                                                       
113300             MOVE NEJ                    TO INDATA-SW                     
113400             MOVE ERR-EJ-BEHORIG-GODK-LA TO MED-IDMFSFEL                  
113500             CALL WMEDKONV USING MED-WMEDAREA                             
113600             MOVE MED-MFSFEL         TO MOD-TEMFSFEL                      
113700           END-IF                                                         
113800         END-IF                                                           
113900       END-IF                                                             
114000     END-IF                                                               
114100     .                                                                    
114200     EJECT                                                                
114300 F-LAES-VISA-INFO               SECTION.                                  
114400                                                                          
114500     MOVE MSGI-IDFTG  TO WS-IDFTG                                         
114600     IF DIST79-DEALER-PRICE OR                                            
114700                               IDFTG-US OR IDFTG-CA OR                    
114800                               IDFTG-CN OR IDFTG-IN OR                    
114900                               IDFTG-KR OR IDFTG-TR OR                    
115000                               IDFTG-MX OR IDFTG-BR OR                    
115100                               IDFTG-MY OR IDFTG-TH OR                    
115200                               IDFTG-TW OR IDFTG-ZA                       
115300       PERFORM S10-HAMTA-KDVALISO                                         
115400     ELSE                                                                 
115500       IF DIST79-ECOM-PRICE                                               
115600         MOVE WS-KDVALISO-SPAR  TO MOD-KDVALISO                           
115700       ELSE                                                               
115800         MOVE 'SEK'             TO MOD-KDVALISO                           
115900       END-IF                                                             
116000     END-IF                                                               
116100                                                                          
116200     PERFORM IMS-GHU-WLKREE11                                             
116300     IF SEGMENT-SAKNAS                                                    
116400        MOVE ERR-INFO-MISSING   TO MED-IDMFSFEL                           
116500        CALL WMEDKONV USING MED-WMEDAREA                                  
116600        MOVE MED-MFSFEL         TO MOD-TEMFSFEL                           
116700        PERFORM MFS-RENSA-FAELT-UT                                        
116800        PERFORM MFS-STAENG-FAELT-IN                                       
116900      ELSE                                                                
117000        PERFORM FA-REDIGERA-BILD                                          
117100     END-IF                                                               
117200     .                                                                    
117300     EJECT                                                                
117400                                                                          
117500 FA-REDIGERA-BILD               SECTION.                                  
117600                                                                          
117700     PERFORM FAA-REDIGERA-ANM-INFO                                        
117800     PERFORM FAB-REDIGERA-FAKT-INFO                                       
117900     PERFORM FAC-REDIGERA-ART-INFO                                        
118000                                                                          
118100     .                                                                    
118200     EJECT                                                                
118300 FAA-REDIGERA-ANM-INFO          SECTION.                                  
118400                                                                          
118500     MOVE '-'                   TO MOD-STRECK-1                           
118600                                   MOD-STRECK-2                           
118700                                   MOD-STRECK-3                           
118800     IF INDATA-OK                                                         
118900       MOVE LEV-KDKREBEH        TO MOD-KDKREBEH                           
119000       IF MOD-KDKREBEH(1:1)        = 'C'                                  
119100          IF MOD-KDKREBEH(2:1)     = '1'                                  
119200             MOVE ZERO          TO MOD-KDKREBEH(2:1)                      
119300          END-IF                                                          
119400       END-IF                                                             
119500       IF MOD-KDKREBEH(1:1)        = 'Q' OR 'P'                           
119600         IF MED-IDMFSINF = '101'                                          
119700           CONTINUE                                                       
119800         ELSE                                                             
119900           IF MOD-KDKREBEH(1:1)    = 'P'                                  
120000             MOVE INF-MATRIS-KONFLIKT-PSN TO MED-IDMFSINF                 
120100             MOVE 'Q'                     TO MOD-KDKREBEH(1:1)            
120200           ELSE                                                           
120300             MOVE INF-MATRIS-KONFLIKT TO MED-IDMFSINF                     
120400           END-IF                                                         
120500           CALL WMEDKONV USING MED-WMEDAREA                               
120600           MOVE MED-MFSINF          TO MOD-TEMFSINF                       
120700         END-IF                                                           
120800       END-IF                                                             
120900       IF SWEDISH-TEXT                                                    
121000          IF MOD-KDKREBEH(1:1)   = 'Y'                                    
121100             MOVE 'J'           TO MOD-KDKREBEH(1:1)                      
121200          ELSE                                                            
121300             IF MOD-KDKREBEH(1:1)  = 'C'                                  
121400                MOVE 'Ä'        TO MOD-KDKREBEH(1:1)                      
121500             ELSE                                                         
121600                IF MOD-KDKREBEH = 'DEL'                                   
121700                   MOVE 'ANN' TO MOD-KDKREBEH                             
121800                END-IF                                                    
121900             END-IF                                                       
122000          END-IF                                                          
122100       ELSE                                                               
122200          IF MOD-KDKREBEH(1:1)     = 'J'                                  
122300             MOVE 'Y'           TO MOD-KDKREBEH(1:1)                      
122400          ELSE                                                            
122500             IF MOD-KDKREBEH       = 'ANN'                                
122600                MOVE 'DEL'      TO MOD-KDKREBEH                           
122700             END-IF                                                       
122800          END-IF                                                          
122900       END-IF                                                             
123000       MOVE LEV-FLSVAR          TO MOD-FLSVAR                             
123100       MOVE LEV-FLRETUR         TO MOD-FLRETUR                            
123200     END-IF                                                               
123300                                                                          
123400     MOVE LEV-IDDC              TO MOD-IDDC                               
123500                                   W-IDDC                                 
123600                                   W-IDDC-B6                              
123700                                   WS-IDDC                                
123800     MOVE LEV-IDARTNR           TO MOD-IDARTNR                            
123900                                   W-IDARTNR-DSEQ-FOM                     
124000                                   W-IDARTNR-DSEQ-TOM                     
124100     MOVE LEV-KDANMORS          TO MOD-KDANMORS                           
124200                                   WS-KDANMORS                            
124300                                   OKOD-KDANMORS                          
124400                                                                          
124500     MOVE JA                    TO WDB6-SW                                
124600     PERFORM IMS-GU-WDB601                                                
124700     IF SEGMENT-SAKNAS                                                    
124800       MOVE NEJ                 TO WDB6-SW                                
124900     END-IF                                                               
125000                                                                          
125100     IF LEV-FLANLYSF = JA                                                 
125200        MOVE MFS-ADD-LYS-UPP-FAELT                                        
125300                                TO MOD-KDANMORS-ATTR                      
125400     ELSE                                                                 
125500        MOVE MFS-FORMATETS-ATTR                                           
125600                                TO MOD-KDANMORS-ATTR                      
125700     END-IF                                                               
125800     MOVE LEV-KVLEVANM-BEKR     TO MOD-KVLEVANM-BEKR                      
125900**** SDC3A OCH LDC HAR INHYRD PERSONAL OCH SKALL EJ KUNNA                 
126000**** SE PRISER ENLIGT SUSSI                                               
126100     MOVE MSGI-IDFTG            TO WS-IDFTG                               
126200     IF (MSG-SIGNON-USERID (1:3) = 'PHL') AND IDFTG-PV                    
126300       MOVE ZERO                TO MOD-PRARTBTO                           
126400     ELSE                                                                 
126500       IF DIST79-DEALER-PRICE OR                                          
126600          DIST79-ECOM-PRICE                                               
126700         MOVE LEV-PRARTBTO-LOC  TO MOD-PRARTBTO                           
126800       ELSE                                                               
126900         MOVE LEV-PRARTBTO      TO MOD-PRARTBTO                           
127000       END-IF                                                             
127100     END-IF                                                               
127200     MOVE LEV-IDORDNR7          TO MOD-IDORDNR5                           
127300     MOVE LEV-IDKUNDRF          TO WS-IDKUNDRF                            
127400     MOVE LEV-IDKOLLI           TO MOD-IDKOLLI                            
127500                                   WS-IDKOLLI                             
127600     MOVE LEV-KDFAKTYP          TO MOD-KDFAKTYP                           
127700     MOVE LEV-IDFAKT            TO MOD-IDFAKT                             
127800     MOVE LEV-TIFAKT            TO MOD-TIFAKT                             
127900                                                                          
128000     MOVE LEV-IDFTG    TO WS-IDFTG                                        
128100     IF IDFTG-PV                                                          
128200        MOVE ZERO               TO MOD-IDFAKT-LOC                         
128300                                   MOD-TIFAKT-LOC                         
128400                                   MOD-PRARTBTO-LOC                       
128500                                   MOD-KDFRAKT                            
128600                                   MOD-PRFRAKT                            
128700        MOVE SPACE              TO MOD-KDFAKTYP-LOC                       
128800     ELSE                                                                 
128900        IF CDC-SE OR GOOD-DDC                                             
129000           MOVE LEV-IDFAKT-LOC  TO MOD-IDFAKT-LOC                         
129100           MOVE LEV-KDFAKTYP    TO MOD-KDFAKTYP-LOC                       
129200           MOVE LEV-TIFAKT-LOC  TO MOD-TIFAKT-LOC                         
129300           MOVE LEV-PRARTBTO-LOCINV TO MOD-PRARTBTO-LOC                   
129400           MOVE LEV-KDFRAKT     TO MOD-KDFRAKT                            
129500           MOVE LEV-PRFRAKT     TO MOD-PRFRAKT                            
129600        ELSE                                                              
129700           MOVE LEV-IDFAKT-LOC  TO MOD-IDFAKT                             
129800           MOVE LEV-TIFAKT-LOC  TO MOD-TIFAKT                             
129900           MOVE LEV-KDFAKTYP    TO MOD-KDFAKTYP                           
130000           MOVE LEV-PRARTBTO-LOCINV TO MOD-PRARTBTO                       
130100           MOVE LEV-KDFRAKT     TO MOD-KDFRAKT                            
130200           MOVE LEV-PRFRAKT     TO MOD-PRFRAKT                            
130300        END-IF                                                            
130400     END-IF                                                               
130500     IF LEV-FLTEXT               = JA                                     
130600       IF MSGI-IDLAND-SPR = 'GB'                                          
130700         MOVE YES               TO MOD-FLTEXT                             
130800       ELSE                                                               
130900         MOVE LEV-FLTEXT        TO MOD-FLTEXT                             
131000       END-IF                                                             
131100     ELSE                                                                 
131200       MOVE LEV-FLTEXT          TO MOD-FLTEXT                             
131300     END-IF                                                               
131400                                                                          
131500     CALL W418OKOD USING OKOD-W418OKOD                                    
131600                                                                          
131700     IF WS-ANTAL-SKALL-FINNAS                                             
131800       PERFORM FAAA-KONTROLLERA-KVLEVANM-TOT                              
131900       MOVE WS-KVLEVANM-TOT     TO MOD-SULEVANM                           
132000     END-IF                                                               
132100                                                                          
132200     .                                                                    
132300     EJECT                                                                
132400 FAAA-KONTROLLERA-KVLEVANM-TOT  SECTION.                                  
132500                                                                          
132600     MOVE +0                    TO WS-KVLEVANM-TOT                        
132700     MOVE MSGI-IDDISTR          TO W-IDDISTR-DSEQ-FOM                     
132800                                   W-IDDISTR-DSEQ-TOM                     
132900     MOVE MSGI-IDKUNDNR         TO W-IDKUNDNR-DSEQ-FOM                    
133000                                   W-IDKUNDNR-DSEQ-TOM                    
133100     MOVE MSGI-IDFTG            TO W-IDFTG-DSEQ-FOM                       
133200                                   W-IDFTG-DSEQ-TOM                       
133300                                   W-IDFTG                                
133400                                                                          
133500     PERFORM IMS-GN-WLKREI-SEQ                                            
133600     PERFORM UNTIL SEGMENT-SAKNAS                                         
133700       MOVE SEQD-IDDISTR        TO W-IDDISTR-A2                           
133800       MOVE SEQD-IDKUNDNR       TO W-IDKUNDNR-A2                          
133900       MOVE SEQD-IDRAPPNR       TO W-IDRAPPNR-A2                          
134000       MOVE SEQD-IDRADNR        TO W-IDRADNR-A2                           
134100       PERFORM IMS-GHU-WLKREE11                                           
134200       IF LEV-IDKOLLI = WS-IDKOLLI                                        
134300         IF LEV-IDKUNDRF = WS-IDKUNDRF                                    
134400           IF LEV-KDKREBEH(1:1) = 'N'                                     
134500             CONTINUE                                                     
134600           ELSE                                                           
134700             ADD LEV-KVLEVANM-BEKR TO WS-KVLEVANM-TOT                     
134800           END-IF                                                         
134900         END-IF                                                           
135000       END-IF                                                             
135100       PERFORM IMS-GN-WLKREI-SEQ                                          
135200     END-PERFORM                                                          
135300     .                                                                    
135400     EJECT                                                                
135500                                                                          
135600 FAB-REDIGERA-FAKT-INFO         SECTION.                                  
135700                                                                          
135800     MOVE LEV-IDFAKT             TO W-IDFAKT-L5                           
135900     MOVE MSGI-IDDISTR          TO W-IDDISTR-L5                           
136000     MOVE MSGI-IDKUNDNR         TO W-IDKUNDNR-L5                          
136100     MOVE WS-IDKUNDRF           TO W-IDKUNDRF-L5                          
136200     MOVE WS-IDKOLLI            TO W-IDKOLLI-L5                           
136300     MOVE MSGI-IDARTNR          TO W-IDARTNR-L5                           
136400                                                                          
136500     PERFORM IMS-GU-WDL501                                                
136600     IF SEGMENT-FINNS                                                     
136700        PERFORM IMS-GNP-WDL511                                            
136800        IF SEGMENT-FINNS                                                  
136900          MOVE FAKC-IDPRODNR TO W-IDPRODNR-L5                             
137000          PERFORM IMS-GNP-WDL521                                          
137100        END-IF                                                            
137200     END-IF                                                               
137300     IF SEGMENT-FINNS                                                     
137400        MOVE FAKL-IDBORD        TO MOD-IDBORD                             
137500        MOVE FAKL-KVBEART-Q     TO MOD-KVBEART-Q                          
137600        MOVE FAKL-KVLEVART      TO MOD-KVLEVART                           
137700        IF WS-KVLEVANM-TOT       > FAKL-KVLEVART                          
137800          MOVE MFS-ADD-HILIGHT-FIELD TO MOD-KVLEVANM-BEKR-ATTR            
137900        END-IF                                                            
138000                                                                          
138100        IF LEV-KDANMORS = '60'                                            
138200           IF LEV-KVLEVANM-BEKR  < FAKL-KVLEVART                          
138300             MOVE MFS-ADD-HILIGHT-FIELD TO MOD-KVLEVANM-BEKR-ATTR         
138400           END-IF                                                         
138500        END-IF                                                            
138600        ADD LEV-KVLEVANM-BEKR   TO WS-KVLEVANM-TOT                        
138700        IF MSGI-KDMATT = 'U'                                              
138800           COMPUTE WS-VKORDBTO = FAKC-VKORDBTO-KOLLI *                    
138900                                 CONV-KG-TO-LB                            
139000           COMPUTE WS-VKORDNTO = FAKC-VKORDNTO-KOLLI *                    
139100                                 CONV-KG-TO-LB                            
139200           MOVE WS-VKORDBTO     TO MOD-VKORDBTO-KOLLI                     
139300                                   VKORDBTO-KOLLI                         
139400           MOVE WS-VKORDNTO     TO VKORDNTO-KOLLI                         
139500                                                                          
139600        ELSE                                                              
139700           MOVE FAKC-VKORDBTO-KOLLI                                       
139800                                TO MOD-VKORDBTO-KOLLI                     
139900                                   VKORDBTO-KOLLI                         
140000           MOVE FAKC-VKORDNTO-KOLLI                                       
140100                                TO VKORDNTO-KOLLI                         
140200        END-IF                                                            
140300                                                                          
140400        MOVE FAKC-KDORDKL       TO MOD-KDORDKL                            
140500        MOVE FAKC-KDKOLLI       TO W-KDKOLLI                              
140600                                   MOD-KDKOLLI                            
140700        IF FAK-FLDIRLEV          = JA                                     
140800          IF MSGI-IDLAND-SPR = 'GB'                                       
140900            MOVE YES            TO MOD-FLDIRLEV                           
141000          ELSE                                                            
141100            MOVE FAK-FLDIRLEV   TO MOD-FLDIRLEV                           
141200          END-IF                                                          
141300        ELSE                                                              
141400          MOVE FAK-FLDIRLEV     TO MOD-FLDIRLEV                           
141500        END-IF                                                            
141600        MOVE FAKL-IDUSER-PACK   TO MOD-IDUSER-PACK                        
141700        INSPECT MOD-IDUSER-PACK REPLACING LEADING ZERO BY SPACE           
141800        MOVE FAKC-IDPRODNR      TO MOD-IDPRODNR                           
141900        MOVE FAKL-KVORDRAD      TO MOD-KVORDRAD                           
142000        MOVE FAKL-IDUSER-OREG   TO MOD-IDUSER-OREG                        
142100        MOVE FAKC-TIFAKT        TO MOD-TIREGDAT                           
142200        PERFORM FABA-BERAEKNA-VKORDNTO-DIFF                               
142300        COMPUTE WS-VKORDNTO-KOLLI-TOT =                                   
142400                WS-VKORDNTO-KOLLI-TOT / 1000                              
142500                                                                          
142600        PERFORM IMS-GET-EMBB01                                            
142700        IF SEGMENT-FINNS                                                  
142800          IF MSGI-KDMATT = 'U'                                            
142900            COMPUTE WS-VKTARA = EMB-VKTARA * CONV-KG-TO-LB                
143000            MOVE WS-VKTARA      TO MOD-VKTARA                             
143100                                   VKTARA                                 
143200          ELSE                                                            
143300            MOVE EMB-VKTARA     TO MOD-VKTARA                             
143400                                   VKTARA                                 
143500          END-IF                                                          
143600          COMPUTE VKORDNTO-KOLLI =                                        
143700                  VKORDBTO-KOLLI -                                        
143800                  VKTARA                                                  
143900          END-COMPUTE                                                     
144000        END-IF                                                            
144100                                                                          
144200        COMPUTE WS-VKORDNTO-DIFF =                                        
144300                VKORDNTO-KOLLI   -                                        
144400                WS-VKORDNTO-KOLLI-TOT                                     
144500        END-COMPUTE                                                       
144600        IF MSGI-KDMATT = 'U'                                              
144700             COMPUTE WS-VKORDNTO-DIFF = WS-VKORDNTO-DIFF *                
144800                                        CONV-KG-TO-LB                     
144900        END-IF                                                            
145000        MOVE WS-VKORDNTO-DIFF        TO MOD-VKORDBTO-DIFF                 
145100        COMPUTE WS-VLORDNTO-KOLLI =                                       
145200                WS-VLORDNTO-KOLLI-TOT / 1000                              
145300        IF MSGI-KDMATT = 'U'                                              
145400           COMPUTE WS-VLORDNTO-KOLLI = WS-VLORDNTO-KOLLI *                
145500                                       CONV-M3-TO-YD3                     
145600        END-IF                                                            
145700      MOVE WS-VLORDNTO-KOLLI       TO MOD-VLORDBTO-KOLLI                  
145800     ELSE                                                                 
145900        IF OKOD-FL-IDFAKT = JA                                            
146000          MOVE MFS-ADD-HILIGHT-FIELD TO MOD-IDFAKT-ATTR                   
146100        END-IF                                                            
146200        IF WS-ANTAL-SKALL-FINNAS                                          
146300          MOVE MFS-ADD-HILIGHT-FIELD TO MOD-KVLEVANM-BEKR-ATTR            
146400        END-IF                                                            
146500        MOVE MFS-RENSA-FAELT    TO MOD-KVBEART-Q                          
146600                                   MOD-KVLEVART                           
146700                                   MOD-VKORDBTO-KOLLI                     
146800                                   MOD-VKORDBTO-DIFF                      
146900                                   MOD-VKTARA                             
147000                                   MOD-KDKOLLI                            
147100                                   MOD-KDORDKL                            
147200                                   MOD-FLDIRLEV                           
147300                                   MOD-IDUSER-PACK                        
147400                                   MOD-IDPRODNR                           
147500                                   MOD-KVORDRAD                           
147600                                   MOD-IDUSER-OREG                        
147700                                   MOD-TIREGDAT                           
147800                                   MOD-IDBORD                             
147900                                                                          
148000     END-IF                                                               
148100     .                                                                    
148200     EJECT                                                                
148300                                                                          
148400 FABA-BERAEKNA-VKORDNTO-DIFF    SECTION.                                  
148500                                                                          
148600     MOVE ZERO                  TO WS-VKORDNTO-KOLLI-TOT                  
148700     PERFORM IMS-GNP-WDL521-KLI-1ST                                       
148800     PERFORM UNTIL SEGMENT-SAKNAS                                         
148900       MOVE FAKL-IDARTNR        TO W-IDARTNR                              
149000       MOVE FAKL-KVLEVART       TO WS-KVLEVART                            
149100       PERFORM IMS-GU-WLARTC01                                            
149200       MOVE FAK-IDDC  TO W-IDDC                                           
149300       PERFORM S20-READ-ARTC11-WDK712-22                                  
149400       COMPUTE WS-VKORDNTO-KOLLI-TOT =                                    
149500               WS-VKORDNTO-KOLLI-TOT +                                    
149600               (WS-KVLEVART * WS-VKART)                                   
149700       COMPUTE WS-VLORDNTO-KOLLI-TOT =                                    
149800               WS-VLORDNTO-KOLLI-TOT +                                    
149900               (WS-KVLEVART * WS-VLARTNTO)                                
150000       PERFORM IMS-GNP-WDL521-KLI                                         
150100     END-PERFORM                                                          
150200     .                                                                    
150300     EJECT                                                                
150400 FAC-REDIGERA-ART-INFO          SECTION.                                  
150500                                                                          
150600     MOVE +0                    TO WS-KVOKS-TOT-CDC                       
150700     MOVE MSGI-IDARTNR          TO W-IDARTNR                              
150800                                                                          
150900     PERFORM IMS-GET-ARTM-WDK9                                            
151000     IF SEGMENT-FINNS                                                     
151100       COMPUTE WS-KVOKS-TOT-CDC = ART-KVOKS-BULK +                        
151200                                  ART-KVOKS-DAG  +                        
151300                                  ART-KVOKS-VOR                           
151400     END-IF                                                               
151500                                                                          
151600     PERFORM IMS-GU-WLARTC01                                              
151700                                                                          
151800     MOVE ART-REKSIFFR          TO MOD-REKSIFFR                           
151900     MOVE ART-IDFKNGRP          TO MOD-IDFKNGRP                           
152000     MOVE ART-KDPRODSL          TO MOD-KDPRODSL                           
152100                                                                          
152200     MOVE FAK-IDDC    TO W-IDDC                                           
152300     PERFORM S20-READ-ARTC11-WDK712-22                                    
152400                                                                          
152500     COMPUTE W-KVPB-TOT       =  CLAG-KVPB-SEP +                          
152600                                 CLAG-KVPB-SATS +                         
152700                                 CLAG-KVPB-TPO                            
152800                                                                          
152900     MOVE W-KVPB-TOT            TO MOD-KVPB-TOT                           
153000     MOVE CLAG-KDERS            TO MOD-KDERS                              
153100     MOVE WS-VKART              TO MOD-VKART                              
153200                                                                          
153300     MOVE MSGI-IDFTG            TO WS-IDFTG                               
153400     IF (MSG-SIGNON-USERID (1:3) = 'PHL') AND IDFTG-PV                    
153500       MOVE ZERO                TO MOD-PRARTBTO-EXP                       
153600     ELSE                                                                 
153700       MOVE CLAG-PRARTBTO-EXP   TO MOD-PRARTBTO-EXP                       
153800     END-IF                                                               
153900     IF NDC-NA OR NDC-CN OR NDC-IN OR NDC-KR OR                           
154000        NDC-TR OR NDC-MY OR NDC-TH OR NDC-TW OR                           
154100        NDC-MX OR NDC-BR OR NDC-ZA                                        
154200       MOVE LEV-IDDC            TO W-IDDC                                 
154300       PERFORM IMS-GU-WDK711                                              
154400       IF SEGMENT-FINNS                                                   
154500          MOVE SLAG-IDPERSON-BUY                                          
154600                               TO MOD-IDANSK                              
154700       END-IF                                                             
154800       MOVE FAK-IDDC            TO W-IDDC                                 
154900     ELSE                                                                 
155000       MOVE WS-IDANSK           TO MOD-IDANSK                             
155100     END-IF                                                               
155200     MOVE CLAG-KDPSLLOC         TO MOD-KDPSLLOC                           
155300                                                                          
155400     IF CDC-SE OR GOOD-DDC                                                
155500       MOVE CLAG-ADLAGOMR       TO MOD-ADLAGOMR                           
155600       MOVE CLAG-ADGANG         TO MOD-ADGANG                             
155700       MOVE CLAG-ADPLATS        TO MOD-ADPLATS                            
155800       MOVE CLAG-PRINK          TO MOD-PRINK                              
155900       MOVE CLAG-TIINVDAT       TO MOD-TIINVDAT                           
156000       MOVE CLAG-KVINVS         TO MOD-KVINVS                             
156100       COMPUTE WS-KVDISP         = CLAG-KVLS   -                          
156200                                   CLAG-KVRESS -                          
156300                                   CLAG-KVUTRS -                          
156400                                   WS-KVOKS-TOT-CDC                       
156500       MOVE WS-KVDISP           TO MOD-KVLS                               
156600     ELSE                                                                 
156700       IF SDC OR NDC-PACIFIC                                              
156800          MOVE CLAG-PRINK       TO MOD-PRINK                              
156900       END-IF                                                             
157000       IF  WDB6-FINNS                                                     
157100       AND DCS-FLPRISSPR = JA                                             
157200         MOVE MSGI-IDFTG        TO WS-IDFTG                               
157300         IF (MSG-SIGNON-USERID (1:3) = 'PHL') AND IDFTG-PV                
157400           MOVE ZERO             TO MOD-PRINK                             
157500         ELSE                                                             
157600           MOVE CLAG-PRINK       TO MOD-PRINK                             
157700         END-IF                                                           
157800       END-IF                                                             
157900       PERFORM IMS-GU-WDK711                                              
158000       IF SEGMENT-FINNS                                                   
158100          MOVE SLAG-ADLAGOMR    TO MOD-ADLAGOMR                           
158200          MOVE SLAG-ADGANG      TO MOD-ADGANG                             
158300          MOVE SLAG-ADPLATS     TO MOD-ADPLATS                            
158400          MOVE SLAG-TIINVDAT    TO MOD-TIINVDAT                           
158500          MOVE SLAG-KVINVS      TO MOD-KVINVS                             
158600          COMPUTE WS-KVDISP      = SLAG-KVLS   -                          
158700                                   SLAG-KVUTRS                            
158800          MOVE WS-KVDISP        TO MOD-KVLS                               
158900          IF NDC-NA OR NDC-CN OR NDC-IN OR NDC-KR OR                      
159000             NDC-TR OR NDC-MY OR NDC-TH OR NDC-TW OR                      
159100             NDC-MX OR NDC-BR OR NDC-ZA                                   
159200             MOVE SLAG-KVPB-REF TO MOD-KVPB-TOT                           
159300             MOVE SLAG-PRAVCOST TO MOD-PRINK                              
159400             MOVE SLAG-IDPERSON-BUY                                       
159500                                TO MOD-IDANSK                             
159600          END-IF                                                          
159700       ELSE                                                               
159800          MOVE MFS-RENSA-FAELT  TO MOD-ADLAGOMR                           
159900                                   MOD-ADGANG                             
160000                                   MOD-ADPLATS                            
160100                                   MOD-TIINVDAT                           
160200                                   MOD-KVINVS                             
160300                                   MOD-KVLS                               
160400       END-IF                                                             
160500     END-IF                                                               
160600                                                                          
160700     PERFORM IMS-GU-WLBENA11                                              
160800     IF SEGMENT-FINNS                                                     
160900        MOVE TEXT-BEART         TO MOD-BEART                              
161000     ELSE                                                                 
161100        MOVE MFS-RENSA-FAELT    TO MOD-BEART                              
161200     END-IF                                                               
161300                                                                          
161400     .                                                                    
161500     EJECT                                                                
161600                                                                          
161700 G-KOLLA-INPUT                  SECTION.                                  
161800                                                                          
161900     MOVE JA                    TO INDATA-SW                              
162000     PERFORM IMS-GET-WLKREE01-KVAL                                        
162100     IF SEGMENT-FINNS                                                     
162200       MOVE ANM-KDVALISO  TO WS-KDVALISO-SPAR                             
162300       MOVE ANM-IDDC-RET  TO SPAR-ANM-IDDC-RET                            
162400       MOVE ANM-IXDCCLEAR TO SPAR-ANM-IXDCCLEAR                           
162500                                                                          
162600       IF ANM-KDLEVANM > '0' AND < '4'                                    
162700         PERFORM IMS-GHU-WLKREE11                                         
162800                                                                          
162900         IF SEGMENT-FINNS                                                 
163000           PERFORM GF-KOLLA-DUBBEL-INPUT                                  
163100           IF INDATA-OK                                                   
163200             PERFORM GA-KOLLA-KDKREBEH                                    
163300             PERFORM GB-KOLLA-FLSVAR                                      
163400             PERFORM GC-KOLLA-IDANSV                                      
163500             PERFORM GD-KOLLA-KDANMORS                                    
163600             PERFORM GE-KOLLA-PRISSATT                                    
163700             PERFORM GG-KOLLA-FLRETUR                                     
163800             PERFORM GH-KOLLA-FLAUTREM                                    
163900           END-IF                                                         
164000         ELSE                                                             
164100           MOVE NEJ             TO INDATA-SW                              
164200           MOVE ERR-WRONG-KEY   TO MED-IDMFSFEL                           
164300           CALL WMEDKONV USING MED-WMEDAREA                               
164400           MOVE MED-MFSFEL      TO MOD-TEMFSFEL                           
164500         END-IF                                                           
164600       ELSE                                                               
164700         IF MID-KDKREBEH NOT = ALL '+'                                    
164800           MOVE MFS-ALFA-FAELT-FEL TO MOD-KDKREBEH-ATTR                   
164900           MOVE MFS-ROER-EJ-FAELT  TO MOD-KDKREBEH                        
165000         END-IF                                                           
165100         IF MID-FLSVAR NOT = ALL '+'                                      
165200           MOVE MFS-ALFA-FAELT-FEL TO MOD-FLSVAR-ATTR                     
165300           MOVE MFS-ROER-EJ-FAELT  TO MOD-FLSVAR                          
165400         END-IF                                                           
165500         IF MID-FLRETUR NOT = ALL '+'                                     
165600           MOVE MFS-ALFA-FAELT-FEL TO MOD-FLRETUR-ATTR                    
165700           MOVE MFS-ROER-EJ-FAELT  TO MOD-FLRETUR                         
165800         END-IF                                                           
165900         IF MID-IDANSV NOT = ALL '+'                                      
166000           MOVE MFS-ALFA-FAELT-FEL TO MOD-IDANSV-ATTR                     
166100           MOVE MFS-ROER-EJ-FAELT TO MOD-IDANSV                           
166200         END-IF                                                           
166300         MOVE NEJ               TO INDATA-SW                              
166400         MOVE ERR-UPDATE-NOT-OK TO MED-IDMFSFEL                           
166500         CALL WMEDKONV USING MED-WMEDAREA                                 
166600         MOVE MED-MFSFEL        TO MOD-TEMFSFEL                           
166700       END-IF                                                             
166800     ELSE                                                                 
166900       MOVE NEJ                 TO INDATA-SW                              
167000       MOVE ERR-WRONG-KEY       TO MED-IDMFSFEL                           
167100       CALL WMEDKONV USING MED-WMEDAREA                                   
167200       MOVE MED-MFSFEL          TO MOD-TEMFSFEL                           
167300     END-IF                                                               
167400                                                                          
167500     .                                                                    
167600     EJECT                                                                
167700 GA-KOLLA-KDKREBEH              SECTION.                                  
167800                                                                          
167900     IF MID-KDKREBEH NOT = ALL '+'                                        
168000     AND MFS-UPD-V                                                        
168100       MOVE MID-KDKREBEH        TO TEST-KDKREBEH                          
168200                                   KDKREBEH-SW                            
168300       IF OK-KOD                                                          
168400         IF MID-KDKREBEH = 'RR ' OR 'QR '                                 
168500           MOVE +2              TO ANSV-KDCALL                            
168600           MOVE MSGI-IDDISTR    TO ANSV-IDDISTR                           
168700           MOVE LEV-IDDC        TO ANSV-IDDC                              
168800           MOVE MSGI-IDFTG      TO ANSV-IDFTG                             
168900           MOVE MSGI-IDKUNDNR   TO ANSV-IDKUNDNR                          
169000           MOVE LEV-KDANMORS    TO ANSV-KDANMORS                          
169100                                                                          
169200           MOVE LEV-IDFAKT      TO W-IDFAKT-L5                            
169300           MOVE MSGI-IDDISTR    TO W-IDDISTR-L5                           
169400           MOVE MSGI-IDKUNDNR   TO W-IDKUNDNR-L5                          
169500           MOVE LEV-IDKUNDRF    TO W-IDKUNDRF-L5                          
169600           MOVE LEV-IDKOLLI     TO W-IDKOLLI-L5                           
169700           MOVE LEV-IDARTNR     TO W-IDARTNR-L5                           
169800                                                                          
169900           MOVE LEV-KDORDKL     TO ANSV-KDORDKL                           
170000           MOVE LEV-ADLAGOMR    TO ANSV-ADLAGOMR                          
170100           PERFORM IMS-GU-WDL501                                          
170200           IF SEGMENT-FINNS                                               
170300              PERFORM IMS-GNP-WDL511                                      
170400              IF SEGMENT-FINNS                                            
170500                MOVE FAKC-KDORDKL    TO ANSV-KDORDKL                      
170600                MOVE FAKC-IDPRODNR   TO W-IDPRODNR-L5                     
170700                PERFORM IMS-GNP-WDL521                                    
170800                IF SEGMENT-FINNS                                          
170900                  MOVE FAKL-ADLAGOMR TO ANSV-ADLAGOMR                     
171000                END-IF                                                    
171100              END-IF                                                      
171200           END-IF                                                         
171300                                                                          
171400           CALL W418ANSV USING ANSV-W418ANSV 4113-PCB                     
171500                                             4115-PCB                     
171600                                             4117-PCB                     
171700                                                                          
171800           IF ANSV-OK                                                     
171900             MOVE ANSV-KDARBTYP TO LEV-KDARBTYP                           
172000                                   LEV-KDARBTYP-REM                       
172100             MOVE ANSV-IDPERSON TO LEV-IDPERSON                           
172200                                   LEV-IDPERSON-REM                       
172300             MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDKREBEH-ATTR               
172400           ELSE                                                           
172500             MOVE MFS-ALFA-FAELT-FEL TO MOD-KDKREBEH-ATTR                 
172600             MOVE MFS-ROER-EJ-FAELT TO MOD-KDKREBEH                       
172700             MOVE NEJ           TO INDATA-SW                              
172800             MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                    
172900             CALL WMEDKONV USING MED-WMEDAREA                             
173000             MOVE MED-MFSFEL TO MOD-TEMFSFEL                              
173100           END-IF                                                         
173200         ELSE                                                             
173300           MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDKREBEH-ATTR                 
173400         END-IF                                                           
173500                                                                          
173600         IF MID-KDKREBEH = 'J  ' OR 'Y  '                                 
173700           IF LEV-KDKREBEH = 'Q  ' OR 'P  '                               
173800             IF LEV-FLRETUR = 'J'  OR                                     
173900                MID-FLRETUR = 'J'  OR                                     
174000                MID-FLRETUR = 'Y'                                         
174100                                                                          
174200               CONTINUE                                                   
174300             ELSE                                                         
174400               MOVE NEJ                  TO INDATA-SW                     
174500               MOVE MFS-ALFA-FAELT-FEL   TO MOD-KDKREBEH-ATTR             
174600               MOVE MFS-ROER-EJ-FAELT    TO MOD-KDKREBEH                  
174700               MOVE ERR-RETUR-OK-SAKNAS  TO MED-IDMFSFEL                  
174800               CALL WMEDKONV USING MED-WMEDAREA                           
174900               MOVE MED-MFSFEL        TO MOD-TEMFSFEL                     
175000             END-IF                                                       
175100           END-IF                                                         
175200         END-IF                                                           
175300       ELSE                                                               
175400         MOVE NEJ               TO INDATA-SW                              
175500         MOVE MFS-ALFA-FAELT-FEL TO MOD-KDKREBEH-ATTR                     
175600         MOVE MFS-ROER-EJ-FAELT TO MOD-KDKREBEH                           
175700         MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                        
175800         CALL WMEDKONV USING MED-WMEDAREA                                 
175900         MOVE MED-MFSFEL        TO MOD-TEMFSFEL                           
176000       END-IF                                                             
176100     ELSE                                                                 
176200       IF MID-KDKREBEH = ALL '+'                                          
177200         SET WS-FLAUTREM-Y     TO TRUE                                    
177201         CONTINUE                                                         
177202       ELSE                                                               
177203         MOVE NEJ               TO INDATA-SW                              
177204         MOVE MFS-ALFA-FAELT-FEL TO MOD-KDKREBEH-ATTR                     
177205         MOVE MFS-ROER-EJ-FAELT TO MOD-KDKREBEH                           
177206         MOVE INF-PRESS-PF23             TO MED-IDMFSFEL                  
177207         CALL WMEDKONV USING MED-WMEDAREA                                 
177208         MOVE MED-MFSFEL        TO MOD-TEMFSFEL                           
177209       END-IF                                                             
177300     END-IF                                                               
177400     .                                                                    
177500     EJECT                                                                
177600 GB-KOLLA-FLSVAR                SECTION.                                  
177700                                                                          
177800     IF MID-FLSVAR NOT = ALL '+'                                          
177900       IF LEV-KDKREBEH = 'RR ' OR 'QR ' OR 'PR'                           
178000         IF MID-FLSVAR = 'J' OR 'Y' OR 'N' OR ' '                         
178100                                                                          
178200           MOVE +1              TO ANSV-KDCALL                            
178300           MOVE MSGI-IDDISTR    TO ANSV-IDDISTR                           
178400           MOVE MSGI-IDFTG      TO ANSV-IDFTG                             
178500           MOVE MSGI-IDKUNDNR   TO ANSV-IDKUNDNR                          
178600           MOVE LEV-KDANMORS    TO ANSV-KDANMORS                          
178700           MOVE ZERO            TO ANSV-KDORDKL                           
178800                                   ANSV-ADLAGOMR                          
178900                                                                          
179000           CALL W418ANSV USING ANSV-W418ANSV 4113-PCB                     
179100                                             4115-PCB                     
179200                                             4117-PCB                     
179300                                                                          
179400           IF ANSV-OK                                                     
179500             MOVE ANSV-KDARBTYP TO LEV-KDARBTYP                           
179600             MOVE ANSV-IDPERSON TO LEV-IDPERSON                           
179700             MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLSVAR-ATTR                 
179800           ELSE                                                           
179900             IF ANSV-KDSVAR = 'S'                                         
180000               MOVE HIGH-VALUE           TO ANSV-KDANMORS                 
180100               CALL W418ANSV USING ANSV-W418ANSV 4113-PCB                 
180200               IF ANSV-KDSVAR = SPACE                                     
180300                 MOVE ANSV-KDARBTYP      TO LEV-KDARBTYP                  
180400                 MOVE ANSV-IDPERSON      TO LEV-IDPERSON                  
180500                 MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLSVAR-ATTR             
180700                 SET  WS-FLAUTREM-Y        TO TRUE                        
180710               ELSE                                                       
180800                 PERFORM S02-FELMEDDELANDE                                
180900               END-IF                                                     
181000             ELSE                                                         
181100               PERFORM S02-FELMEDDELANDE                                  
181200             END-IF                                                       
181300           END-IF                                                         
181400         ELSE                                                             
181500           PERFORM S02-FELMEDDELANDE                                      
181600         END-IF                                                           
181700       ELSE                                                               
181800         PERFORM S02-FELMEDDELANDE                                        
181900       END-IF                                                             
182000     END-IF                                                               
182100     .                                                                    
182200     EJECT                                                                
182300 GC-KOLLA-IDANSV                SECTION.                                  
182400                                                                          
182500     IF MID-IDANSV NOT = ALL '+'                                          
182600       IF MID-IDANSV(4:3) NUMERIC                                         
182700         MOVE MID-IDANSV(1:3) TO W-KDARBTYP                               
182800         MOVE MID-IDANSV(4:3) TO W-IDPERSON                               
182900         PERFORM IMS-GET-WDP311                                           
183000         IF SEGMENT-FINNS                                                 
183100           MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDANSV-ATTR                   
183200         ELSE                                                             
183300           MOVE MFS-ALFA-FAELT-FEL TO MOD-IDANSV-ATTR                     
183400           MOVE MFS-ROER-EJ-FAELT TO MOD-IDANSV                           
183500           MOVE NEJ               TO INDATA-SW                            
183600           MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                      
183700           CALL WMEDKONV USING MED-WMEDAREA                               
183800           MOVE MED-MFSFEL        TO MOD-TEMFSFEL                         
183900         END-IF                                                           
184000       ELSE                                                               
184100         MOVE MFS-ALFA-FAELT-FEL TO MOD-IDANSV-ATTR                       
184200         MOVE MFS-ROER-EJ-FAELT TO MOD-IDANSV                             
184300         MOVE NEJ               TO INDATA-SW                              
184400         MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                        
184500         CALL WMEDKONV USING MED-WMEDAREA                                 
184600         MOVE MED-MFSFEL        TO MOD-TEMFSFEL                           
184700       END-IF                                                             
184800     END-IF                                                               
184900     .                                                                    
185000     EJECT                                                                
185100 GD-KOLLA-KDANMORS              SECTION.                                  
185200                                                                          
185300     IF MID-KDANMORS-UPPD NOT = ALL '+'                                   
185400        AND MFS-UPD-V                                                     
185500       MOVE MID-KDANMORS-UPPD   TO OKOD-KDANMORS                          
185600       CALL W418OKOD USING OKOD-W418OKOD                                  
185700       IF OKOD-FL-GODK-KOD = JA                                           
185800         PERFORM GDA-KOLLA-KDANMORS                                       
185900         IF OKOD-FL-RETILL = JA                                           
186000           PERFORM GDB-KOLLA-RETMATRIX                                    
186100         END-IF                                                           
186200         IF KDANMORS-UPPD = NEJ                                           
186300           MOVE NEJ             TO INDATA-SW                              
186400           MOVE MFS-ALFA-FAELT-FEL TO MOD-KDANMORS-UPPD-ATTR              
186500           MOVE MFS-ROER-EJ-FAELT TO MOD-KDANMORS-UPPD                    
186600           MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                      
186700           CALL WMEDKONV USING MED-WMEDAREA                               
186800           MOVE MED-MFSFEL      TO MOD-TEMFSFEL                           
186900         ELSE                                                             
187000           MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDANMORS-UPPD-ATTR           
187100         END-IF                                                           
187200       ELSE                                                               
187300         MOVE NEJ               TO INDATA-SW                              
187400         MOVE MFS-ALFA-FAELT-FEL TO MOD-KDANMORS-UPPD-ATTR                
187500         MOVE MFS-ROER-EJ-FAELT TO MOD-KDANMORS-UPPD                      
187600         MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                        
187700         CALL WMEDKONV USING MED-WMEDAREA                                 
187800         MOVE MED-MFSFEL    TO MOD-TEMFSFEL                               
187900       END-IF                                                             
188000     ELSE                                                                 
188100       IF MID-KDANMORS-UPPD = ALL '+'                                     
188200         CONTINUE                                                         
188300       ELSE                                                               
188400         MOVE NEJ               TO INDATA-SW                              
188500         MOVE MFS-ALFA-FAELT-FEL TO MOD-KDANMORS-UPPD-ATTR                
188600         MOVE MFS-ROER-EJ-FAELT TO MOD-KDANMORS-UPPD                      
188700         MOVE INF-PRESS-PF23             TO MED-IDMFSFEL                  
188800         CALL WMEDKONV USING MED-WMEDAREA                                 
188900         MOVE MED-MFSFEL        TO MOD-TEMFSFEL                           
189000       END-IF                                                             
189100     END-IF                                                               
189200     .                                                                    
189300     EJECT                                                                
189400                                                                          
189500 GDA-KOLLA-KDANMORS             SECTION.                                  
189600                                                                          
189700     MOVE JA                    TO KDANMORS-UPPD                          
189800     MOVE MID-KDANMORS-UPPD     TO OKOD-KDANMORS                          
189900                                   W-KDANMORS-4110-MIN                    
190000                                   W-KDANMORS-4110-MAX                    
190100     CALL W418OKOD USING OKOD-W418OKOD                                    
190200                                                                          
190300     IF OKOD-FL-PRIS-ZERO = JA                                            
190400       IF DIST79-DEALER-PRICE OR                                          
190500          DIST79-ECOM-PRICE                                               
190600         IF LEV-PRARTBTO-LOC  = ZERO                                      
190700           CONTINUE                                                       
190800         ELSE                                                             
190900           MOVE NEJ             TO KDANMORS-UPPD                          
191000         END-IF                                                           
191100       ELSE                                                               
191200         IF LEV-PRARTBTO = ZERO                                           
191300           CONTINUE                                                       
191400         ELSE                                                             
191500           MOVE NEJ             TO KDANMORS-UPPD                          
191600         END-IF                                                           
191700       END-IF                                                             
191800     END-IF                                                               
191900                                                                          
192000     IF OKOD-FL-GODK-PRIS-ZERO = NEJ                                      
192100       IF DIST79-DEALER-PRICE OR                                          
192200          DIST79-ECOM-PRICE                                               
192300         IF LEV-PRARTBTO-LOC    = ZERO                                    
192400           MOVE NEJ             TO KDANMORS-UPPD                          
192500         END-IF                                                           
192600       ELSE                                                               
192700         IF LEV-PRARTBTO        = ZERO                                    
192800           MOVE NEJ             TO KDANMORS-UPPD                          
192900         END-IF                                                           
193000       END-IF                                                             
193100     END-IF                                                               
193200                                                                          
193300     IF OKOD-FL-IDFAKT = JA                                               
193400         IF  LEV-IDFAKT         = ZERO                                    
193500           MOVE NEJ               TO KDANMORS-UPPD                        
193600         END-IF                                                           
193700     END-IF                                                               
193800                                                                          
193900     IF OKOD-FL-KDFAKTYP-R = JA                                           
194000       IF  LEV-KDFAKTYP       = SPACE                                     
194100         MOVE NEJ               TO KDANMORS-UPPD                          
194200       END-IF                                                             
194300     END-IF                                                               
194400                                                                          
194500     IF OKOD-FL-ANALYSNR = JA                                             
194600       MOVE LEV-IDFTG         TO W-IDFTG                                  
194700       MOVE W-IDARTNR         TO W-IDARTNR-4110-MIN                       
194800                                 W-IDARTNR-4110-MAX                       
194900       PERFORM  S01-LAS-ANALYSNRREGISTRET                                 
195000                                                                          
195100       IF WL410901-FINNS                                                  
195200         CONTINUE                                                         
195300       ELSE                                                               
195400         MOVE NEJ             TO KDANMORS-UPPD                            
195500       END-IF                                                             
195600     END-IF                                                               
195700                                                                          
195800*-KOD 70 FÅR ENBART ANVÄNDAS FÖR SOFTWARE-ARTIKEL.                        
195900     IF MID-KDANMORS-UPPD = '70'                                          
196000       IF  LEV-KDANMORS       = '75' OR '73' OR '72' OR '74'              
196100         MOVE NEJ             TO KDANMORS-UPPD                            
196200       END-IF                                                             
196300     END-IF                                                               
196400                                                                          
196500     IF MID-KDANMORS-UPPD = '72' OR '73' OR '75' OR '74'                  
196600       IF  LEV-KDANMORS       = '70'                                      
196700         MOVE NEJ             TO KDANMORS-UPPD                            
196800       END-IF                                                             
196900     END-IF                                                               
197000                                                                          
197100*-KOD 74 FÅR ENBART ANVÄNDAS FÖR INTERN-KUNDER MED N-FAKTURA.             
197200     IF MID-KDANMORS-UPPD = '74'                                          
197300       IF  LEV-KDANMORS       = '75' OR '73' OR '72'                      
197400         MOVE NEJ             TO KDANMORS-UPPD                            
197500       END-IF                                                             
197600     END-IF                                                               
197700                                                                          
197800     IF MID-KDANMORS-UPPD = '72' OR '73' OR '75'                          
197900       IF  LEV-KDANMORS       = '74'                                      
198000         MOVE NEJ             TO KDANMORS-UPPD                            
198100       END-IF                                                             
198200     END-IF                                                               
198300                                                                          
198400*-KOD 72 FÅR INTE ÄNDRAS FÖR LDC-KUNDER, ENLIGT SUSSI 2006-01             
198500*-DESSA KUNDER FÅR INTE BLANDA KOD 72 MED ANDRA KODER.(+AUTOKOD)          
198600     IF MID-KDANMORS-UPPD = '73' OR '75' OR '72'                          
198700       PERFORM IMS-GU-GMTA-WDB201                                         
198800       IF SEGMENT-FINNS                                                   
198900         CONTINUE                                                         
199000       ELSE                                                               
199100         PERFORM IMS-GET-WDB201                                           
199200       END-IF                                                             
199300       IF GMT-FLLDCKND = JA OR                                            
199400          GMT-FLRETUR  = JA                                               
199500         IF MID-KDANMORS-UPPD = '72'                                      
199600           IF  LEV-KDANMORS       = '75' OR '73'                          
199700             MOVE NEJ             TO KDANMORS-UPPD                        
199800           END-IF                                                         
199900         END-IF                                                           
200000         IF MID-KDANMORS-UPPD = '73' OR '75'                              
200100           IF  LEV-KDANMORS       = '72'                                  
200200             MOVE NEJ             TO KDANMORS-UPPD                        
200300           END-IF                                                         
200400         END-IF                                                           
200500       END-IF                                                             
200600     END-IF                                                               
200700                                                                          
200800     IF MID-KDANMORS-UPPD = '42'                                          
200900       IF  LEV-KDANMORS       = '43' OR '62'                              
201000         CONTINUE                                                         
201100       ELSE                                                               
201200         MOVE NEJ             TO KDANMORS-UPPD                            
201300       END-IF                                                             
201400     END-IF                                                               
201500                                                                          
201600     IF MID-KDANMORS-UPPD = '43' OR '62'                                  
201700       IF  LEV-KDANMORS       = '42'                                      
201800         CONTINUE                                                         
201900       ELSE                                                               
202000         IF LEV-KDANMORS       = '63' AND                                 
202100            MID-KDANMORS-UPPD  = '62'                                     
202200             CONTINUE                                                     
202300         ELSE                                                             
202400             MOVE NEJ               TO KDANMORS-UPPD                      
202500         END-IF                                                           
202600       END-IF                                                             
202700     END-IF                                                               
202800                                                                          
202900     IF MID-KDANMORS-UPPD = '82' OR '83'                                  
203000       IF  LEV-KDANMORS       = '82' OR '83'                              
203100         CONTINUE                                                         
203200       ELSE                                                               
203300         MOVE NEJ             TO KDANMORS-UPPD                            
203400       END-IF                                                             
203500     END-IF                                                               
203600                                                                          
203700*- KOD 20 FÅR BARA ÄNDRAS TILL 25 (=KOD 20 MEN EJ LAGERAVBOKNING).        
203800*- KOD 22 FÅR BARA ÄNDRAS TILL 26,27 OCH 28 FÖR IDFTG=57.                 
203900*- OK FÖR INDIEN.                                                         
204000*- EJ NA.                                                                 
204100     IF MID-KDANMORS-UPPD = '25' OR '26' OR '27' OR '28'                  
204200       IF DIST07-USA-RET-DISCR  OR                                        
204300          DIST07-CAN-RET-DISCR  OR                                        
204400          DIST35-REFILL-NA-JAP  OR                                        
204500          DIST35-REFILL-NA      OR                                        
204600          DIST35-REFILL-CN      OR                                        
204700          DIST35-CDC-IN-REFILL  OR                                        
204800          DIST35-CDC-KR-REFILL  OR                                        
204900          DIST35-CDC-TR-REFILL  OR                                        
205000          DIST35-CDC-BR-REFILL  OR                                        
205100          DIST35-CDC-ZA-REFILL  OR                                        
205200          DIST35-CDC-MX-REFILL  OR                                        
205300          DIST35-CDC-AE-REFILL  OR                                        
205400          DIST35-NONVCC-NONVCC-REFILL OR                                  
205500          DIST35-NONVCC-NONVCC-TRANSFER OR                                
205600          DIST35-CDC-MY-REFILL  OR                                        
205700          DIST35-CDC-TH-REFILL  OR                                        
205800          DIST35-CDC-TH-93-REFILL     OR                                  
205900          DIST35-CDC-TW-REFILL  OR                                        
206000*----     (LEV-IDFTG NOT = 57)                                            
206100          IDFTG-US              OR                                        
206200          IDFTG-CA                                                        
206300                                                                          
206400         MOVE NEJ             TO KDANMORS-UPPD                            
206500       ELSE                                                               
206600         IF MID-KDANMORS-UPPD = '20' OR '25'                              
206700           IF  LEV-KDANMORS       = '20' OR '25'                          
206800             CONTINUE                                                     
206900           ELSE                                                           
207000             MOVE NEJ             TO KDANMORS-UPPD                        
207100           END-IF                                                         
207200         END-IF                                                           
207300                                                                          
207400         IF MID-KDANMORS-UPPD = '26' OR '27' OR '28'                      
207500           IF  LEV-KDANMORS       = '21' OR '22' OR '23'                  
207600             CONTINUE                                                     
207700           ELSE                                                           
207800             MOVE NEJ             TO KDANMORS-UPPD                        
207900           END-IF                                                         
208000         END-IF                                                           
208100       END-IF                                                             
208200     END-IF                                                               
208300                                                                          
208400     IF MID-KDANMORS-UPPD (1:1) = LEV-KDANMORS (1:1)                      
208500       CONTINUE                                                           
208600     ELSE                                                                 
208700       IF MID-KDANMORS-UPPD = '00' OR '60'                                
208800         IF  LEV-KDANMORS       = '00' OR '60'                            
208900           CONTINUE                                                       
209000         ELSE                                                             
209100           MOVE NEJ             TO KDANMORS-UPPD                          
209200         END-IF                                                           
209300       ELSE                                                               
209400         IF MID-KDANMORS-UPPD = '42' OR '62'                              
209500           IF  LEV-KDANMORS       = '42' OR '62'                          
209600             CONTINUE                                                     
209700           ELSE                                                           
209800             MOVE NEJ             TO KDANMORS-UPPD                        
209900           END-IF                                                         
210000         ELSE                                                             
210100           MOVE NEJ             TO KDANMORS-UPPD                          
210200         END-IF                                                           
210300       END-IF                                                             
210400     END-IF                                                               
210500                                                                          
210600     IF LEV-KDKREBEH = 'R  ' OR 'Q  ' OR 'P  '                            
210700       CONTINUE                                                           
210800     ELSE                                                                 
210900       MOVE NEJ                TO KDANMORS-UPPD                           
211000     END-IF                                                               
211100     .                                                                    
211200     EJECT                                                                
211300 GDB-KOLLA-RETMATRIX    SECTION.                                          
211400                                                                          
211500     MOVE NEJ                TO SW-KDKREBEH-Q                             
211600     MOVE NEJ                TO SW-KDKREBEH-P                             
211700                                                                          
211800     IF LEV-IDARTNR = DUMMY-IDARTNR                                       
211900       CONTINUE                                                           
212000     ELSE                                                                 
212100       MOVE IDPGM              TO KTL3-IDPGM                              
212200       MOVE LEV-IDARTNR        TO KTL3-IDARTNR                            
212300       MOVE MID-KDANMORS-UPPD  TO KTL3-KDANMORS                           
212400       MOVE LEV-IDDC           TO KTL3-IDDC                               
212500       MOVE MSGI-IDDISTR       TO KTL3-IDDISTR                            
212600       MOVE MSGI-IDKUNDNR      TO KTL3-IDKUNDNR                           
212700       CALL W418KTL3 USING  KTL3-W418KTL3 KTL3-WDA8-PCB                   
212800                                          KTL3-WDB2-PCB                   
212900                                          KTL3-WDK6-PCB                   
213000                                          KTL3-WDK7-PCB                   
213100                                          KTL3-WDB6-PCB                   
213200                                          KTL3-1165-PCB                   
213300                                                                          
213400       IF KTL3-KDSVAR = YES                                               
213500         IF KTL3-KDRETBEH = 'S'                                           
213600           MOVE NEJ                TO KDANMORS-UPPD                       
213700         END-IF                                                           
213800         IF KTL3-KDRETBEH = 'Q'                                           
213900           MOVE JA                 TO SW-KDKREBEH-Q                       
214000         END-IF                                                           
214100         IF KTL3-KDRETBEH = 'P'                                           
214200           MOVE JA                 TO SW-KDKREBEH-P                       
214300         END-IF                                                           
214400       END-IF                                                             
214500     END-IF                                                               
214600                                                                          
214700     .                                                                    
214800     EJECT                                                                
214900 GE-KOLLA-PRISSATT              SECTION.                                  
215000                                                                          
215100     IF DIST79-DEALER-PRICE                                               
215200       IF LEV-FLPRQUES = JA                                               
215300         IF LEV-KDVAT = SPACE  OR  LEV-PRARTBTO-LOC = ZERO                
215400           MOVE MFS-ALFA-FAELT-FEL TO MOD-KDKREBEH-ATTR                   
215500           MOVE MFS-ROER-EJ-FAELT  TO MOD-KDKREBEH                        
215600           MOVE NEJ            TO INDATA-SW                               
215700           MOVE ERR-PRIS-MISSING   TO MED-IDMFSFEL                        
215800           CALL WMEDKONV USING MED-WMEDAREA                               
215900           MOVE MED-MFSFEL         TO MOD-TEMFSFEL                        
216000         END-IF                                                           
216100       END-IF                                                             
216200     END-IF                                                               
216300     .                                                                    
216400     EJECT                                                                
216500 GF-KOLLA-DUBBEL-INPUT          SECTION.                                  
216600                                                                          
216700     IF MID-KDKREBEH NOT = ALL '+' AND                                    
216800        MID-KDANMORS-UPPD NOT = ALL '+' AND                               
216900        MFS-UPD-V                                                         
217000                                                                          
217100       IF MID-KDKREBEH NOT = ALL '+'                                      
217200         MOVE MFS-ALFA-FAELT-FEL TO MOD-KDKREBEH-ATTR                     
217300         MOVE MFS-ROER-EJ-FAELT TO MOD-KDKREBEH                           
217400       END-IF                                                             
217500       IF MID-KDANMORS-UPPD NOT = ALL '+'                                 
217600         MOVE MFS-ALFA-FAELT-FEL TO MOD-KDANMORS-UPPD-ATTR                
217700         MOVE MFS-ROER-EJ-FAELT TO MOD-KDANMORS-UPPD                      
217800       END-IF                                                             
217900       IF MID-FLSVAR NOT = ALL '+'                                        
218000         MOVE MFS-ALFA-FAELT-FEL TO MOD-FLSVAR-ATTR                       
218100         MOVE MFS-ROER-EJ-FAELT TO MOD-FLSVAR                             
218200       END-IF                                                             
218300       IF MID-FLRETUR NOT = ALL '+'                                       
218400         MOVE MFS-ALFA-FAELT-FEL TO MOD-FLRETUR-ATTR                      
218500         MOVE MFS-ROER-EJ-FAELT TO MOD-FLRETUR                            
218600       END-IF                                                             
218700       IF MID-IDANSV NOT = ALL '+'                                        
218800         MOVE MFS-ALFA-FAELT-FEL TO MOD-IDANSV-ATTR                       
218900         MOVE MFS-ROER-EJ-FAELT TO MOD-IDANSV                             
219000       END-IF                                                             
219100       IF MID-IDPRT NOT = ALL '+'                                         
219200         MOVE MFS-ALFA-FAELT-FEL TO MOD-IDPRT-ATTR                        
219300         MOVE MFS-ROER-EJ-FAELT TO MOD-IDPRT                              
219400       END-IF                                                             
219500                                                                          
219600       MOVE NEJ                  TO INDATA-SW                             
219700       MOVE ERR-FLERA-FUNKTIONER TO MED-IDMFSFEL                          
219800       CALL WMEDKONV USING MED-WMEDAREA                                   
219900       MOVE MED-MFSFEL           TO MOD-TEMFSFEL                          
220000     END-IF                                                               
220100                                                                          
220200     IF INDATA-OK                                                         
220300       IF MID-IDPRT NOT = ALL '+'                                         
220400         MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDPRT-ATTR                     
220500         MOVE MFS-ROER-EJ-FAELT TO MOD-IDPRT                              
220600         MOVE NEJ                  TO INDATA-SW                           
220700         MOVE INF-PRESS-PF4        TO MED-IDMFSFEL                        
220800         CALL WMEDKONV USING MED-WMEDAREA                                 
220900         MOVE MED-MFSFEL           TO MOD-TEMFSFEL                        
221000       END-IF                                                             
221100     END-IF                                                               
221200     .                                                                    
221300     EJECT                                                                
221400 GG-KOLLA-FLRETUR               SECTION.                                  
221500                                                                          
221600     IF MID-FLRETUR NOT = ALL '+'                                         
221700       IF LEV-KDKREBEH = 'Q  ' OR 'P  '                                   
221800         IF MID-FLRETUR = 'J' OR 'Y' OR 'N' OR ' '                        
221900           CONTINUE                                                       
222000         ELSE                                                             
222100           MOVE NEJ                TO INDATA-SW                           
222200           MOVE MFS-ALFA-FAELT-FEL TO MOD-FLRETUR-ATTR                    
222300           MOVE MFS-ROER-EJ-FAELT  TO MOD-FLRETUR                         
222400           MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                      
222500           CALL WMEDKONV USING MED-WMEDAREA                               
222600           MOVE MED-MFSFEL      TO MOD-TEMFSFEL                           
222700         END-IF                                                           
222800       ELSE                                                               
222900         MOVE NEJ                TO INDATA-SW                             
223000         MOVE MFS-ALFA-FAELT-FEL TO MOD-FLRETUR-ATTR                      
223100         MOVE MFS-ROER-EJ-FAELT  TO MOD-FLRETUR                           
223200         MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                        
223300         CALL WMEDKONV USING MED-WMEDAREA                                 
223400         MOVE MED-MFSFEL      TO MOD-TEMFSFEL                             
223500       END-IF                                                             
223600     END-IF                                                               
223700                                                                          
223800     .                                                                    
223900     EJECT                                                                
224000 GH-KOLLA-FLAUTREM              SECTION.                                  
224100                                                                          
224200     MOVE NEJ TO SW-KDKREBEH-Y                                            
224300     IF MID-FLSVAR NOT = ALL '+'                                          
224400        IF LEV-KDKREBEH = 'RR '                                           
224500           IF MID-FLSVAR = 'J' OR 'Y'                                     
224600              PERFORM IMS-GU-GMTA-WDB201                                  
224700              IF NOT SEGMENT-FINNS                                        
224800                 PERFORM IMS-GET-WDB201                                   
224900              END-IF                                                      
225000              IF GMT-FLAUTREM = 'J' OR 'Y'                                
225100                 IF MID-KDANMORS-UPPD = ALL '+'                           
225200                    IF NOT (LEV-KDANMORS = '42' OR                        
225300                            LEV-KDANMORS = '62')                          
225400                       MOVE 'Y'  TO MOD-KDKREBEH(1:1)                     
225500                       MOVE JA   TO SW-KDKREBEH-Y                         
225600                    END-IF                                                
225700                 END-IF                                                   
225800              ELSE                                                        
225900                 IF GMT-FLAUTREM = 'N'                                    
226000                    IF MID-KDANMORS-UPPD = ALL '+'                        
226100                       IF NOT (LEV-KDANMORS = '12' OR                     
226200                               LEV-KDANMORS = '22' OR                     
226300                               LEV-KDANMORS = '42' OR                     
226400                               LEV-KDANMORS = '62')                       
226500                          MOVE 'Y'  TO MOD-KDKREBEH(1:1)                  
226600                          MOVE JA   TO SW-KDKREBEH-Y                      
226700                       END-IF                                             
226800                    END-IF                                                
226900                 END-IF                                                   
227000              END-IF                                                      
227100           ELSE                                                           
227200              IF WS-FLAUTREM-N                                            
227300                 PERFORM GHA-KOLLA-PRARTBTO                               
227400              END-IF                                                      
227500           END-IF                                                         
227600        ELSE                                                              
227700           CONTINUE                                                       
227900        END-IF                                                            
228000     ELSE                                                                 
228300        IF WS-FLAUTREM-Y                                                  
228400           PERFORM GHA-KOLLA-PRARTBTO                                     
228500        END-IF                                                            
228510     END-IF                                                               
228600     .                                                                    
228700     EJECT                                                                
228800 GHA-KOLLA-PRARTBTO             SECTION.                                  
228900                                                                          
229000     IF (MID-FLSVAR = 'N' OR LEV-FLSVAR ='N')                             
229100        IF MID-KDANMORS-UPPD = ALL '+'                                    
229200           IF NOT (LEV-KDANMORS = '00'  OR                                
229300                   LEV-KDANMORS = '11'  OR                                
229400                   LEV-KDANMORS = '12'  OR                                
229500                   LEV-KDANMORS = '20'  OR                                
229600                   LEV-KDANMORS = '21'  OR                                
229700                   LEV-KDANMORS = '22'  OR                                
229800                   LEV-KDANMORS = '42'  OR                                
229900                   LEV-KDANMORS = '62')                                   
230000              IF WS-KDVALISO-SPAR = 'SEK' AND                             
230100                 (LEV-PRARTBTO < 500 OR LEV-PRARTBTO-LOC < 500)           
230200                 IF MID-KDKREBEH = ALL '+' AND                            
230300                    LEV-KDKREBEH = 'RR '                                  
230400                    MOVE 'Y'  TO MOD-KDKREBEH(1:1)                        
230500                    MOVE JA   TO SW-KDKREBEH-Y                            
230600                 END-IF                                                   
230700              ELSE                                                        
230800                 IF WS-KDVALISO-SPAR NOT = 'SEK'                          
230900                    IF LEV-PRARTBTO  = ZERO                               
231000                       MOVE LEV-PRARTBTO-LOC TO WS-PRARTBTO-CONV          
231100                       PERFORM GHB-KOLLA-PRARTBTO-CONV                    
231200                    ELSE                                                  
231300                       IF LEV-PRARTBTO-LOC = ZERO                         
231400                          MOVE LEV-PRARTBTO  TO WS-PRARTBTO-CONV          
231500                       END-IF                                             
231600                       PERFORM GHB-KOLLA-PRARTBTO-CONV                    
231700                    END-IF                                                
231800                    IF WS-PRARTBTO-CONV < 500                             
231900                       IF MID-KDKREBEH = ALL '+' AND                      
232000                          LEV-KDKREBEH = 'RR '                            
232100                          MOVE 'Y'  TO MOD-KDKREBEH(1:1)                  
232200                          MOVE JA   TO SW-KDKREBEH-Y                      
232300                       END-IF                                             
232400                    ELSE                                                  
232500                       MOVE NEJ               TO INDATA-SW                
232600                       MOVE ERR-UPDATE-NOT-OK TO MED-IDMFSFEL             
232700                       CALL WMEDKONV       USING MED-WMEDAREA             
232800                       MOVE MED-MFSFEL        TO MOD-TEMFSFEL             
232900                    END-IF                                                
233000                 END-IF                                                   
233100              END-IF                                                      
233200           ELSE                                                           
233300              MOVE NEJ               TO INDATA-SW                         
233400              MOVE ERR-UPDATE-NOT-OK TO MED-IDMFSFEL                      
233500              CALL WMEDKONV       USING MED-WMEDAREA                      
233600              MOVE MED-MFSFEL        TO MOD-TEMFSFEL                      
233700           END-IF                                                         
233800        END-IF                                                            
233900     END-IF                                                               
234000     .                                                                    
234100     EJECT                                                                
234200 GHB-KOLLA-PRARTBTO-CONV        SECTION.                                  
234300                                                                          
234400     MOVE W-DATE-AAMM      TO CURR-TIAAMM                                 
234500     MOVE WS-KDVALISO-SPAR TO CURR-KDVALISO-ROW                           
234600                              CURR-KDVALISO-HUV                           
234700     MOVE 'M'              TO CURR-KDVALTYP                               
234800     CALL W510CURR      USING CURR-W510CURR WDG2-PCB                      
234900     IF CURR-KDSVAR = ' '                                                 
235000        MOVE CURR-PRKURS-NEW   TO WS-PRKURS                               
235100     ELSE                                                                 
235200        MOVE 1                 TO WS-PRKURS                               
235300     END-IF                                                               
235400     COMPUTE WS-PRARTBTO-CONV  ROUNDED =                                  
235500             WS-PRARTBTO-CONV * WS-PRKURS * LEV-KVLEVANM-BEKR             
235600     .                                                                    
235700     EJECT                                                                
235800 H-UPPDATERA                    SECTION.                                  
235900                                                                          
236000     MOVE SPACE                 TO KDKREBEH-SW                            
236100                                                                          
236200     PERFORM HA-AENDRA-KDKREBEH                                           
236300                                                                          
236400     MOVE +1                    TO MEAN-IX                                
236500     PERFORM IMS-GET-WLKREE01-KVAL                                        
236600     PERFORM IMS-GET-KREE11-GNP                                           
236700     PERFORM UNTIL SEGMENT-SAKNAS                                         
236800       MOVE LEV-KDKREBEH        TO TEST-KDKREBEH                          
236900       IF KDKREBEH-1 = 'R' OR 'Q' OR 'P'                                  
237000         MOVE JA                TO OBEH-RADER-FINNS-SW                    
237100         IF KDKREBEH-2 = 'R'                                              
237200           MOVE JA              TO OBEH-REMISS-FINNS-SW                   
237300         END-IF                                                           
237400       ELSE                                                               
237500         MOVE JA                TO BEH-RADER-FINNS-SW                     
237600         IF KDKREBEH-1 = 'N'                                              
237700           PERFORM HB-FYLL-MEAN-AREA                                      
237800           MOVE JA              TO AVVISADE-RADER-SW                      
237900         ELSE                                                             
238000           IF LEV-KDKREBEH = 'ANN'                                        
238100             MOVE JA            TO ANNULERADE-RADER-SW                    
238200           ELSE                                                           
238300             IF KDKREBEH-1 = 'Y' OR 'C' OR 'J'                            
238400               MOVE JA          TO GODKAENDA-RADER-SW                     
238500             END-IF                                                       
238600           END-IF                                                         
238700         END-IF                                                           
238800       END-IF                                                             
238900       PERFORM IMS-GET-KREE11-GNP                                         
239000     END-PERFORM                                                          
239100                                                                          
239200     PERFORM IMS-GET-WLKREE01-KVAL                                        
239300     IF OBEH-RADER-FINNS                                                  
239400       IF BEH-RADER-FINNS OR OBEH-REMISS-FINNS                            
239500         MOVE  2                TO ANM-KDLEVANM                           
239600         PERFORM IMS-REPL-KREE                                            
239700       END-IF                                                             
239800     ELSE                                                                 
239900       MOVE MSGI-IDFTG   TO WS-IDFTG                                      
240000       IF GODKAENDA-RADER-FINNS                                           
240100         IF IDFTG-US OR IDFTG-CA                                          
240200           CONTINUE                                                       
240300         ELSE                                                             
240400           IF ANM-IDUSER-ADM = SPACE                                      
240500             MOVE MSGI-IDUSER     TO ANM-IDUSER-ADM                       
240600             MOVE MSGI-BEANST     TO ANM-BEANST                           
240700           END-IF                                                         
240800         END-IF                                                           
240900         MOVE  3                TO ANM-KDLEVANM                           
241000         PERFORM IMS-REPL-KREE                                            
241100         IF AVVISADE-RADER-FINNS                                          
241200           IF IDFTG-US                                                    
241300             CONTINUE                                                     
241400           ELSE                                                           
241500             PERFORM HC-SKRIV-MEAN-AREA                                   
241600           END-IF                                                         
241700         END-IF                                                           
241800       ELSE                                                               
241900         MOVE '7'             TO ANM-KDLEVANM                             
242000         PERFORM IMS-REPL-KREE                                            
242100         IF AVVISADE-RADER-FINNS                                          
242200           IF IDFTG-US                                                    
242300             CONTINUE                                                     
242400           ELSE                                                           
242500             PERFORM HC-SKRIV-MEAN-AREA                                   
242600           END-IF                                                         
242700         END-IF                                                           
242800       END-IF                                                             
242900     END-IF                                                               
243000                                                                          
243100     .                                                                    
243200     EJECT                                                                
243300                                                                          
243400 HA-AENDRA-KDKREBEH             SECTION.                                  
243500                                                                          
243600     MOVE NEJ    TO ANGRA-NEKAD-RETUR-RAD-SW                              
243700     MOVE NEJ    TO ANGRA-NEKAD-RAD-SW                                    
243800                                                                          
243900     IF MID-KDKREBEH NOT = ALL '+'                                        
244000       IF MID-KDKREBEH(1:1) = 'J'                                         
244100         MOVE 'Y'               TO MID-KDKREBEH(1:1)                      
244200       END-IF                                                             
244300                                                                          
244400**-- OM MAN FÖRST NEKAR EN RAD,(STATUS<=3) OCH SEDAN ÅNGRAR SIG           
244500**-- OCH UPPDATERAR KDKREBEH=JA FÅR MAN KOLLA IDDC-RET PÅ WDA201          
244600**-- IGEN,ÄVEN KDLEVATT.NY KOLL INLAGD 2007-10-18.                        
244700       IF MID-KDKREBEH(1:1) = 'Y' AND LEV-KDKREBEH(1:1) = 'N'             
244800         MOVE JA    TO ANGRA-NEKAD-RAD-SW                                 
244900         MOVE LEV-KDANMORS    TO OKOD-KDANMORS                            
245000         CALL W418OKOD USING OKOD-W418OKOD                                
245100         IF OKOD-FL-RETILL = JA                                           
245200           MOVE JA    TO ANGRA-NEKAD-RETUR-RAD-SW                         
245300         END-IF                                                           
245400       END-IF                                                             
245500                                                                          
245600*-- Q-RADER SKALL BEHANDLAS SOM Q-REMISSER OCH SPECIALBEHANDLAS.          
245700       IF MID-KDKREBEH     = 'RR ' OR 'QR '                               
245800         IF LEV-KDKREBEH(1:1) = 'Q'                                       
245900           MOVE 'QR '           TO MID-KDKREBEH                           
246000         END-IF                                                           
246100         IF LEV-KDKREBEH(1:1) = 'P'                                       
246200           MOVE 'PR '           TO MID-KDKREBEH                           
246300         END-IF                                                           
246400       END-IF                                                             
246500                                                                          
246600       MOVE MID-KDKREBEH        TO LEV-KDKREBEH                           
246700                                   KDKREBEH-SW                            
246800                                   TEST-KDKREBEH                          
246900       MOVE JA                  TO SW-4721-HOPP                           
247000       IF MID-KDKREBEH     = 'RR ' OR 'QR ' OR 'PR'                       
247100         IF LEV-TIREMISS-UT = +0                                          
247200           MOVE WS-DATUM        TO LEV-TIREMISS-UT                        
247300         END-IF                                                           
247400       END-IF                                                             
247500       IF MID-KDKREBEH(1:1) = 'N'                                         
247600         MOVE LEV-KDANMORS      TO SPAR-KDANMORS                          
247700       END-IF                                                             
247800     END-IF                                                               
247900                                                                          
248000     IF MID-FLSVAR   NOT = ALL '+'                                        
248100       MOVE MID-FLSVAR          TO LEV-FLSVAR                             
248200       IF LEV-KDKREBEH = 'QR '                                            
248300         MOVE 'Q  '             TO LEV-KDKREBEH                           
248400       ELSE                                                               
248500         IF LEV-KDKREBEH = 'PR '                                          
248600           MOVE 'P  '             TO LEV-KDKREBEH                         
248700         ELSE                                                             
248800**-- ACTIVE THE REMISS AUTOAPPROVE --**                                   
248900           IF LEV-KDKREBEH = 'RR ' AND                                    
249000              SW-KDKREBEH-Y = JA                                          
249100              MOVE 'Y  '          TO LEV-KDKREBEH                         
249200           ELSE                                                           
249300              MOVE 'R  '          TO LEV-KDKREBEH                         
249400           END-IF                                                         
249500         END-IF                                                           
249600       END-IF                                                             
249700       MOVE WS-DATUM            TO LEV-TIREMISS-IN                        
249800     ELSE                                                                 
249900       IF MID-FLSVAR  = ALL '+'                                           
250000          IF (LEV-KDKREBEH  = 'RR ' AND                                   
250100              SW-KDKREBEH-Y = JA)                                         
250200              MOVE 'Y  '        TO LEV-KDKREBEH                           
250300              MOVE WS-DATUM     TO LEV-TIREMISS-IN                        
250400          END-IF                                                          
250500       END-IF                                                             
250600     END-IF                                                               
250700                                                                          
250800     IF MID-FLRETUR  NOT = ALL '+'                                        
250900       IF MID-FLRETUR = YES                                               
251000         MOVE JA                TO LEV-FLRETUR                            
251100       ELSE                                                               
251200         MOVE MID-FLRETUR       TO LEV-FLRETUR                            
251300       END-IF                                                             
251400     END-IF                                                               
251500                                                                          
251600     IF MID-KDANMORS-UPPD NOT = ALL '+'                                   
251700       MOVE LEV-KDANMORS        TO SPAR-KDANMORS                          
251800       MOVE LEV-IDDC-RET        TO SPAR-IDDC-RET                          
251900       MOVE MID-KDANMORS-UPPD   TO LEV-KDANMORS                           
252000       MOVE LEV-IDFTG    TO WS-IDFTG                                      
252100       IF MID-KDANMORS-UPPD = '53' OR                                     
252200          ( IDFTG-PV AND MID-KDANMORS-UPPD = '55' )                       
252300         MOVE WS-IDANALYS-UPPD  TO LEV-IDANALYS                           
252400         MOVE WS-IDKONTO-UPPD   TO LEV-IDKONTO                            
252500         MOVE WS-IDKST-UPPD     TO LEV-IDKST                              
252600       ELSE                                                               
252700          IF MID-KDANMORS-UPPD = '52' OR                                  
252800          ( IDFTG-PV AND MID-KDANMORS-UPPD = '54' )                       
252900           MOVE WS-IDANALYS-UPPD  TO LEV-IDANALYS                         
253000           MOVE ZERO              TO LEV-IDKONTO                          
253100           MOVE SPACE             TO LEV-IDKST                            
253200         END-IF                                                           
253300       END-IF                                                             
253400                                                                          
253500**-- OM KOD ÄNDRAS SKALL IDDC-RET KOLLAS FÖR LDC-KUNDER.2006-01-30        
253600       PERFORM IMS-GU-GMTA-WDB201                                         
253700       IF SEGMENT-FINNS                                                   
253800         CONTINUE                                                         
253900       ELSE                                                               
254000         PERFORM IMS-GET-WDB201                                           
254100       END-IF                                                             
254200       IF GMT-FLLDCKND = JA OR                                            
254300          GMT-FLRETUR  = JA                                               
254400         PERFORM HG-KOLLA-IDDC-RET                                        
254500       END-IF                                                             
254600                                                                          
254700       IF SW-KDKREBEH-Q = JA                                              
254800         MOVE 'Q  '             TO LEV-KDKREBEH                           
254900       ELSE                                                               
255000         IF SW-KDKREBEH-P = JA                                            
255100           MOVE 'P  '           TO LEV-KDKREBEH                           
255200         ELSE                                                             
255300           MOVE 'C05'           TO LEV-KDKREBEH                           
255400         END-IF                                                           
255500       END-IF                                                             
255600       MOVE JA                  TO SW-4721-HOPP                           
255700     END-IF                                                               
255800                                                                          
255900     IF MID-IDANSV   NOT = ALL '+'                                        
256000       MOVE MID-IDANSV(1:3)     TO LEV-KDARBTYP                           
256100       MOVE MID-IDANSV(4:3)     TO LEV-IDPERSON                           
256200       IF LEV-KDKREBEH = 'RR ' OR 'QR ' OR 'PR'                           
256300         MOVE MID-IDANSV(1:3)   TO LEV-KDARBTYP-REM                       
256400         MOVE MID-IDANSV(4:3)   TO LEV-IDPERSON-REM                       
256500       END-IF                                                             
256600     END-IF                                                               
256700     MOVE LEV-KDANMORS          TO OKOD-KDANMORS                          
256800                                                                          
256900     PERFORM IMS-REPL-KREE                                                
257000     MOVE INF-UPDATE-DONE       TO MED-IDMFSINF                           
257100     CALL WMEDKONV USING MED-WMEDAREA                                     
257200     MOVE MED-MFSINF            TO MOD-TEMFSINF                           
257300                                                                          
257400**OM OK-KDKREBEH UPPDATERAS ANTAL PÅ WL4110 ,ÄVEN FÖR ÄNDRAD KOD          
257500*                           SOM BLIR GODKÄND DIREKT MED C05               
257600     IF OK-GODKAEND                                                       
257700       PERFORM HAB-UPPD-ANALYSNRREGISTRET                                 
257800     END-IF                                                               
257900                                                                          
258000**-- OM RAD NEKAS SKALL RADPRISET FÖR KNOTARAD RÄKNAS AV FRÅN WDR5        
258100**-- WDGX4104 - SOX-ÄNDRING 20050425.UNDANTAG USA/CAN.                    
258200     MOVE MSGI-IDFTG  TO WS-IDFTG                                         
258300     IF IDFTG-PV OR IDFTG-CN OR IDFTG-IN OR IDFTG-KR OR IDFTG-TR          
258400                 OR IDFTG-MY OR IDFTG-TH OR IDFTG-TW                      
258500                 OR IDFTG-MX OR IDFTG-BR OR IDFTG-ZA                      
258600       MOVE LEV-IDDC    TO WS-IDDC                                        
258700       IF NDC-PACIFIC                                                     
258800         CONTINUE                                                         
258900       ELSE                                                               
259000         IF OK-FELKOD                                                     
259100           PERFORM HD-EV-UPPDATERA-KN-WDGX4103                            
259200           PERFORM HF-KOLLA-KDLEVATT-WDA201                               
259300         END-IF                                                           
259400**-- OM MAN ÅNGRAR NEKAD RAD MÅSTE RADPRISET PÅ KNOTAN RÄKNAS OM.         
259500         IF ANGRA-NEKAD-RAD                                               
259600           PERFORM HL-UPPDAT-KN-WDGX4103-IGEN                             
259700           PERFORM HF-KOLLA-KDLEVATT-WDA201                               
259800         END-IF                                                           
259900       END-IF                                                             
260000     END-IF                                                               
260100                                                                          
260200**-- OM RAD NEKAS SKALL IDDC-RET KOLLAS/ÄNDRAS PÅ WDA201.                 
260300     IF OK-FELKOD                                                         
260400       MOVE SPAR-KDANMORS TO OKOD-KDANMORS                                
260500       CALL W418OKOD USING OKOD-W418OKOD                                  
260600                                                                          
260700       IF OKOD-FL-RETILL = JA                                             
260800         PERFORM IMS-GU-GMTA-WDB201                                       
260900         IF SEGMENT-FINNS                                                 
261000           CONTINUE                                                       
261100         ELSE                                                             
261200           PERFORM IMS-GET-WDB201                                         
261300         END-IF                                                           
261400         IF GMT-FLLDCKND = JA OR                                          
261500            GMT-FLRETUR  = JA                                             
261600           PERFORM HJ-EV-UPPD-DC-RET-WDA201                               
261700         ELSE                                                             
261800           PERFORM HK-EV-UPPD-DC-RET-OEVRIGT                              
261900         END-IF                                                           
262000       ELSE                                                               
262100         CONTINUE                                                         
262200       END-IF                                                             
262300     END-IF                                                               
262400                                                                          
262500**-- OM MAN ÅNGRAR NEKAD RETUR-RAD MÅSTE DC-RET KOLLAS PÅ WDA201.         
262600     IF ANGRA-NEKAD-RETUR-RAD                                             
262700       PERFORM IMS-GU-GMTA-WDB201                                         
262800       IF SEGMENT-FINNS                                                   
262900         CONTINUE                                                         
263000       ELSE                                                               
263100         PERFORM IMS-GET-WDB201                                           
263200       END-IF                                                             
263300       IF GMT-FLLDCKND = JA OR                                            
263400          GMT-FLRETUR  = JA                                               
263500         PERFORM HM-UPPD-DC-RET-WDA201-IGEN                               
263600       ELSE                                                               
263700         PERFORM HN-UPPD-DC-RET-OEVRIGT-IGEN                              
263800       END-IF                                                             
263900     END-IF                                                               
264000                                                                          
264100     IF MID-KDANMORS-UPPD NOT = ALL '+'                                   
264200       IF LEV-KDKREBEH = 'C05'                                            
264300         PERFORM HAB-UPPD-ANALYSNRREGISTRET                               
264400                                                                          
264500**-- OM KOD ÄNDRAS SKALL SUMMAN FÖR KNOTAN RÄKNAS OM PÅ WDR5              
264600**-- WDGX4104 - SOX-ÄNDRING 20050425.UNDANTAG USA/CAN                     
264700         MOVE MSGI-IDFTG    TO WS-IDFTG                                   
264800         IF IDFTG-PV OR IDFTG-CN OR IDFTG-IN OR IDFTG-KR OR               
264900            IDFTG-TR OR IDFTG-MY OR IDFTG-TH OR IDFTG-TW OR               
265000            IDFTG-MX OR IDFTG-BR OR IDFTG-ZA                              
265100           MOVE LEV-IDDC    TO WS-IDDC                                    
265200           IF NDC-PACIFIC                                                 
265300             CONTINUE                                                     
265400           ELSE                                                           
265500             PERFORM HE-EV-UPPDAT-KOD-WDGX4103                            
265600             PERFORM HF-KOLLA-KDLEVATT-WDA201                             
265700           END-IF                                                         
265800         END-IF                                                           
265900                                                                          
266000**-- OM KOD ÄNDRAS SKALL IDDC-RET KOLLAS/ÄNDRAS PÅ WDA2                   
266100         IF GMT-FLLDCKND = JA OR                                          
266200            GMT-FLRETUR  = JA                                             
266300           PERFORM HH-KOLLA-IDDC-RET-WDA201                               
266400         ELSE                                                             
266500           PERFORM HI-KOLLA-IDDC-RET-OEVRIGT                              
266600         END-IF                                                           
266700       END-IF                                                             
266800     END-IF                                                               
266900                                                                          
267000     IF MID-KDKREBEH     = W-RR OR W-QR OR W-PR                           
267100       PERFORM HAA-REMISS                                                 
267200     END-IF                                                               
267300     .                                                                    
267400     EJECT                                                                
267500                                                                          
267600 HAA-REMISS                     SECTION.                                  
267700                                                                          
267800                                                                          
267900     IF MID-IDANSV   NOT = ALL '+'                                        
268000       MOVE MID-IDANSV(1:3)     TO W-KDARBTYP                             
268100                                   MERE-KDARBTYP                          
268200       MOVE MID-IDANSV(4:3)     TO W-IDPERSON                             
268300                                   MERE-IDPERSON                          
268400       PERFORM IMS-GET-WDP311                                             
268500       IF SEGMENT-FINNS                                                   
268600          MOVE PERS-IDMAIL      TO MERE-IDMAIL                            
268700       END-IF                                                             
268800     ELSE                                                                 
268900       MOVE +2                  TO ANSV-KDCALL                            
269000       MOVE MSGI-IDDISTR        TO ANSV-IDDISTR                           
269100       MOVE LEV-IDDC            TO ANSV-IDDC                              
269200       MOVE MSGI-IDFTG          TO ANSV-IDFTG                             
269300       MOVE MSGI-IDKUNDNR       TO ANSV-IDKUNDNR                          
269400       MOVE LEV-KDANMORS        TO ANSV-KDANMORS                          
269500                                                                          
269600       MOVE LEV-IDFAKT      TO W-IDFAKT-L5                                
269700       MOVE MSGI-IDDISTR    TO W-IDDISTR-L5                               
269800       MOVE MSGI-IDKUNDNR   TO W-IDKUNDNR-L5                              
269900       MOVE LEV-IDKUNDRF    TO W-IDKUNDRF-L5                              
270000       MOVE LEV-IDKOLLI     TO W-IDKOLLI-L5                               
270100       MOVE LEV-IDARTNR     TO W-IDARTNR-L5                               
270200                                                                          
270300       MOVE LEV-KDORDKL     TO ANSV-KDORDKL                               
270400       MOVE LEV-ADLAGOMR    TO ANSV-ADLAGOMR                              
270500                                                                          
270600       PERFORM IMS-GU-WDL501                                              
270700       IF SEGMENT-FINNS                                                   
270800          PERFORM IMS-GNP-WDL511                                          
270900          IF SEGMENT-FINNS                                                
271000            MOVE FAKC-KDORDKL    TO ANSV-KDORDKL                          
271100            MOVE FAKC-IDPRODNR   TO W-IDPRODNR-L5                         
271200            PERFORM IMS-GNP-WDL521                                        
271300            IF SEGMENT-FINNS                                              
271400              MOVE FAKL-ADLAGOMR TO ANSV-ADLAGOMR                         
271500            END-IF                                                        
271600          END-IF                                                          
271700       END-IF                                                             
271800                                                                          
271900       CALL W418ANSV USING ANSV-W418ANSV 4113-PCB                         
272000                                         4115-PCB                         
272100                                         4117-PCB                         
272200                                                                          
272300                                                                          
272400       IF ANSV-OK                                                         
272500         MOVE ANSV-KDARBTYP     TO W-KDARBTYP                             
272600                                     MERE-KDARBTYP                        
272700         MOVE ANSV-IDPERSON     TO W-IDPERSON                             
272800                                     MERE-IDPERSON                        
272900         PERFORM IMS-GET-WDP311                                           
273000         IF SEGMENT-FINNS                                                 
273100            MOVE PERS-IDMAIL    TO MERE-IDMAIL                            
273200         END-IF                                                           
273300       END-IF                                                             
273400     END-IF                                                               
273500                                                                          
273600     PERFORM HAAA-SKAPA-REMISS-MAIL                                       
273700     .                                                                    
273800                                                                          
273900     EJECT                                                                
274000 HAAA-SKAPA-REMISS-MAIL         SECTION.                                  
274100                                                                          
274200     MOVE LEV-IDDC    TO WS-IDDC                                          
274300                         W-IDDC-B6                                        
274400                                                                          
274500     MOVE JA                    TO WDB6-SW                                
274600     PERFORM IMS-GU-WDB601                                                
274700     IF SEGMENT-SAKNAS                                                    
274800       MOVE NEJ                 TO WDB6-SW                                
274900     END-IF                                                               
275000                                                                          
275100     PERFORM HAAAA-FIXA-LEVANM-INFO                                       
275200     PERFORM HAAAB-FIXA-FAKT-INFO                                         
275300     PERFORM HAAAC-FIXA-ART-INFO                                          
275400                                                                          
275500     CALL W418MERE USING MERE-W418MERE MAIL-PCB                           
275600     .                                                                    
275700                                                                          
275800     EJECT                                                                
275900 HAAAA-FIXA-LEVANM-INFO         SECTION.                                  
276000                                                                          
276100     MOVE MSGI-IDDISTR          TO MERE-IDDISTR                           
276200     MOVE MSGI-IDKUNDNR         TO MERE-IDKUNDNR                          
276300     MOVE MSGI-IDRAPPNR         TO MERE-IDRAPPNR                          
276400                                                                          
276500     MOVE LEV-IDARTNR           TO MERE-IDARTNR                           
276600                                   W-IDARTNR-A2                           
276700     MOVE LEV-IDRADNR           TO MERE-IDRADNR                           
276800                                   W-IDRADNR-A2                           
276900     MOVE LEV-KDANMORS          TO MERE-KDANMORS                          
277000     MOVE LEV-KVLEVANM-BEKR     TO MERE-KVLEVANM-BEKR                     
277100     IF  WDB6-FINNS                                                       
277200     AND DCS-FLPRISSPR = JA                                               
277300       MOVE ZERO                TO MERE-PRARTBTO                          
277400     ELSE                                                                 
277500       IF DIST79-DEALER-PRICE OR                                          
277600          DIST79-ECOM-PRICE                                               
277700         MOVE LEV-PRARTBTO-LOC  TO MERE-PRARTBTO                          
277800       ELSE                                                               
277900         MOVE LEV-PRARTBTO      TO MERE-PRARTBTO                          
278000       END-IF                                                             
278100     END-IF                                                               
278200     MOVE LEV-IDORDNR7          TO MERE-IDORDNR5                          
278300     MOVE LEV-IDKOLLI           TO MERE-IDKOLLI                           
278400     MOVE LEV-KDFAKTYP          TO MERE-KDFAKTYP                          
278500                                                                          
278600     MOVE LEV-IDFAKT            TO MERE-IDFAKT                            
278700     MOVE LEV-TIFAKT            TO MERE-TIFAKT                            
278800                                                                          
278900     MOVE +1                    TO INDX                                   
279000     PERFORM UNTIL INDX        >  3                                       
279100      MOVE SPACE                TO MERE-TEANMNOT-REG(INDX)                
279200                                  MERE-TEANMNOT-ADM(INDX)                 
279300                                  MERE-TEANMNOT-REM(INDX)                 
279400                                  MERE-TEANMNOT-RET(INDX)                 
279500                                                                          
279600      ADD +1                    TO INDX                                   
279700     END-PERFORM                                                          
279800                                                                          
279900     IF LEV-FLTEXT             =  JA                                      
280000        PERFORM IMS-GNP-WLKREE21                                          
280100        IF SEGMENT-FINNS                                                  
280200           MOVE +1              TO INDX                                   
280300           PERFORM UNTIL INDX           >  3                              
280400            MOVE TXT-TEANMNOT-REG(INDX) TO MERE-TEANMNOT-REG(INDX)        
280500            MOVE TXT-TEANMNOT-ADM(INDX) TO MERE-TEANMNOT-ADM(INDX)        
280600            MOVE TXT-TEANMNOT-REM(INDX) TO MERE-TEANMNOT-REM(INDX)        
280700            MOVE TXT-TEANMNOT-RET(INDX) TO MERE-TEANMNOT-RET(INDX)        
280800                                                                          
280900            ADD +1                       TO INDX                          
281000           END-PERFORM                                                    
281100        END-IF                                                            
281200     END-IF                                                               
281300                                                                          
281400     .                                                                    
281500     EJECT                                                                
281600                                                                          
281700 HAAAB-FIXA-FAKT-INFO           SECTION.                                  
281800                                                                          
281900     MOVE LEV-IDFAKT            TO W-IDFAKT-L5                            
282000     MOVE MSGI-IDDISTR          TO W-IDDISTR-L5                           
282100     MOVE MSGI-IDKUNDNR         TO W-IDKUNDNR-L5                          
282200     MOVE LEV-IDKUNDRF          TO W-IDKUNDRF-L5                          
282300     MOVE LEV-IDKOLLI           TO W-IDKOLLI-L5                           
282400     MOVE LEV-IDARTNR           TO W-IDARTNR-L5                           
282500                                                                          
282600     PERFORM IMS-GU-WDL501                                                
282700     IF SEGMENT-FINNS                                                     
282800        PERFORM IMS-GNP-WDL511                                            
282900        IF SEGMENT-FINNS                                                  
283000          MOVE FAKC-IDPRODNR TO W-IDPRODNR-L5                             
283100          PERFORM IMS-GNP-WDL521                                          
283200        END-IF                                                            
283300     END-IF                                                               
283400     IF SEGMENT-FINNS                                                     
283500        MOVE FAKL-KVBEART-Q     TO MERE-KVBEART-Q                         
283600        MOVE FAKL-KVLEVART      TO MERE-KVLEVART                          
283700        MOVE FAKC-VKORDBTO-KOLLI TO MERE-VKORDBTO-KOLLI                   
283800                                   VKORDBTO-KOLLI                         
283900        MOVE FAKC-KDORDKL       TO MERE-KDORDKL                           
284000        MOVE FAKC-KDKOLLI       TO W-KDKOLLI                              
284100        MOVE FAK-FLDIRLEV       TO MERE-FLDIRLEV                          
284200        MOVE FAKL-IDUSER-PACK   TO MERE-IDUSER-PACK                       
284300        MOVE FAKC-IDPRODNR      TO MERE-IDPRODNR                          
284400        MOVE FAKL-KVORDRAD      TO MERE-KVORDRAD                          
284500        MOVE FAKL-IDUSER-OREG   TO MERE-IDUSER-OREG                       
284600        MOVE FAKC-TIFAKT        TO MERE-TIREGDAT                          
284700        MOVE +0                 TO MERE-VKORDNTO-TOT                      
284800     ELSE                                                                 
284900        MOVE ZERO               TO MERE-KVBEART-Q                         
285000                                   MERE-KVLEVART                          
285100                                   MERE-VKORDBTO-KOLLI                    
285200                                   MERE-VKORDNTO-TOT                      
285300                                   MERE-VKTARA                            
285400                                   MERE-KDORDKL                           
285500                                   MERE-KVORDRAD                          
285600                                   MERE-IDPRODNR                          
285700                                   MERE-TIREGDAT                          
285800        MOVE SPACE              TO MERE-FLDIRLEV                          
285900                                   MERE-IDUSER-PACK                       
286000                                   MERE-IDUSER-OREG                       
286100                                                                          
286200     END-IF                                                               
286300     PERFORM IMS-GET-EMBB01                                               
286400     IF SEGMENT-FINNS                                                     
286500       MOVE EMB-VKTARA          TO MERE-VKTARA                            
286600                                   VKTARA                                 
286700       COMPUTE MERE-VKORDNTO-KOLLI = VKORDBTO-KOLLI - VKTARA              
286800     ELSE                                                                 
286900       MOVE +0                  TO MERE-VKTARA                            
287000                                   MERE-VKORDNTO-KOLLI                    
287100     END-IF                                                               
287200     .                                                                    
287300     EJECT                                                                
287400                                                                          
287500 HAAAC-FIXA-ART-INFO            SECTION.                                  
287600                                                                          
287700     PERFORM IMS-GU-WLARTC01                                              
287800                                                                          
287900     MOVE ART-REKSIFFR          TO MERE-REKSIFFR                          
288000     MOVE ART-IDFKNGRP          TO MERE-IDFKNGRP                          
288100     MOVE ART-KDPRODSL          TO MERE-KDPRODSL                          
288200                                                                          
288300     MOVE LEV-IDDC    TO W-IDDC                                           
288400     PERFORM S20-READ-ARTC11-WDK712-22                                    
288500                                                                          
288600     COMPUTE W-KVPB-TOT       =  CLAG-KVPB-SEP +                          
288700                                 CLAG-KVPB-SATS +                         
288800                                 CLAG-KVPB-TPO                            
288900                                                                          
289000     MOVE CLAG-ADLAGOMR         TO MERE-ADLAGOMR                          
289100     MOVE CLAG-ADGANG           TO MERE-ADGANG                            
289200     MOVE CLAG-ADPLATS          TO MERE-ADPLATS                           
289300     MOVE CLAG-KVLS             TO MERE-KVLS                              
289400     MOVE W-KVPB-TOT            TO MERE-KVPB-TOT                          
289500     MOVE CLAG-KDERS            TO MERE-KDERS                             
289600     MOVE WS-VKART              TO MERE-VKART                             
289700     IF  WDB6-FINNS                                                       
289800     AND DCS-FLPRISSPR = JA                                               
289900       MOVE ZERO                TO MERE-PRARTBTO-EXP                      
290000                                   MERE-PRINK                             
290100     ELSE                                                                 
290200       MOVE CLAG-PRARTBTO-EXP   TO MERE-PRARTBTO-EXP                      
290300       MOVE CLAG-PRINK          TO MERE-PRINK                             
290400     END-IF                                                               
290500     MOVE CLAG-TIINVDAT         TO MERE-TIINVDAT                          
290600     MOVE CLAG-KVINVS           TO MERE-KVINVS                            
290700     IF NDC-NA OR NDC-CN OR NDC-IN OR NDC-KR OR                           
290800        NDC-TR OR NDC-MY OR NDC-TH OR NDC-TW OR                           
290900        NDC-MX OR NDC-BR OR NDC-ZA                                        
291000        PERFORM IMS-GU-WDK711                                             
291100        IF SEGMENT-FINNS                                                  
291200           MOVE SLAG-IDPERSON-BUY                                         
291300                                TO MERE-IDANSK                            
291400        END-IF                                                            
291500     ELSE                                                                 
291600        MOVE WS-IDANSK          TO MERE-IDANSK                            
291700     END-IF                                                               
291800                                                                          
291900                                                                          
292000     PERFORM IMS-GU-WLBENA11                                              
292100     IF SEGMENT-FINNS                                                     
292200        MOVE TEXT-BEART         TO MERE-BEART                             
292300     ELSE                                                                 
292400        MOVE SPACE              TO MERE-BEART                             
292500     END-IF                                                               
292600                                                                          
292700     .                                                                    
292800     EJECT                                                                
292900 HAB-UPPD-ANALYSNRREGISTRET     SECTION.                                  
293000                                                                          
293100     CALL W418OKOD USING OKOD-W418OKOD                                    
293200     IF OKOD-FL-ANALYSNR = JA                                             
293300       MOVE LEV-IDFTG              TO W-IDFTG                             
293400       MOVE W-IDARTNR              TO W-IDARTNR-4110-MIN                  
293500                                      W-IDARTNR-4110-MAX                  
293600       MOVE LEV-KDANMORS           TO W-KDANMORS-4110-MIN                 
293700                                      W-KDANMORS-4110-MAX                 
293800       PERFORM IMS-GU-410901-ROT                                          
293900                                                                          
294000       PERFORM IMS-GHNP-410911-KVAL                                       
294100                                                                          
294200       MOVE NEJ                    TO TRAEFF-SW                           
294300       PERFORM UNTIL SEGMENT-SAKNAS OR TRAEFF-SW = JA                     
294400          IF WS-DATUM-Y2K >= 4110-DAGILTIG-FOM AND                        
294500                          <= 4110-DAGILTIG-TOM                            
294600             ADD LEV-KVLEVANM-BEKR TO 4110-KVART                          
294700             PERFORM IMS-REPL-4109                                        
294800             MOVE JA               TO TRAEFF-SW                           
294900          ELSE                                                            
295000             PERFORM IMS-GHNP-410911-KVAL                                 
295100          END-IF                                                          
295200       END-PERFORM                                                        
295300     END-IF                                                               
295400     .                                                                    
295500     EJECT                                                                
295600                                                                          
295700 HB-FYLL-MEAN-AREA              SECTION.                                  
295800                                                                          
295900     IF MEAN-IX < +14                                                     
296000       MOVE MSGI-IDDISTR        TO MEAN-IDDISTR  (MEAN-IX)                
296100       MOVE MSGI-IDKUNDNR       TO MEAN-IDKUNDNR (MEAN-IX)                
296200       MOVE MSGI-IDRAPPNR       TO MEAN-IDRAPPNR (MEAN-IX)                
296300                                                                          
296400       MOVE LEV-IDARTNR         TO MEAN-IDARTNR  (MEAN-IX)                
296500                                   W-IDARTNR-A2                           
296600       MOVE LEV-IDRADNR         TO MEAN-IDRADNR  (MEAN-IX)                
296700                                   W-IDRADNR-A2                           
296800       MOVE LEV-KDKREBEH        TO MEAN-KDKREBEH (MEAN-IX)                
296900                                                                          
297000       PERFORM IMS-GNP-WLKREE21                                           
297100                                                                          
297200       IF SEGMENT-FINNS                                                   
297300        MOVE TXT-TEANMNOT-ADM (1) TO                                      
297400             MEAN-TEANMNOT-ADM (MEAN-IX, 1)                               
297500        MOVE TXT-TEANMNOT-ADM (2) TO                                      
297600             MEAN-TEANMNOT-ADM (MEAN-IX, 2)                               
297700        MOVE TXT-TEANMNOT-ADM (3) TO                                      
297800             MEAN-TEANMNOT-ADM (MEAN-IX, 3)                               
297900                                                                          
298000        MOVE TXT-TEANMNOT-REM (1) TO                                      
298100             MEAN-TEANMNOT-REM (MEAN-IX, 1)                               
298200        MOVE TXT-TEANMNOT-REM (2) TO                                      
298300             MEAN-TEANMNOT-REM (MEAN-IX, 2)                               
298400        MOVE TXT-TEANMNOT-REM (3) TO                                      
298500             MEAN-TEANMNOT-REM (MEAN-IX, 3)                               
298600       ELSE                                                               
298700         MOVE SPACE             TO MEAN-TEANMNOT-ADM-GRP (MEAN-IX)        
298800         MOVE SPACE             TO MEAN-TEANMNOT-REM-GRP (MEAN-IX)        
298900       END-IF                                                             
299000                                                                          
299100       ADD +1                   TO MEAN-IX                                
299200     END-IF                                                               
299300     .                                                                    
299400     EJECT                                                                
299500 HC-SKRIV-MEAN-AREA             SECTION.                                  
299600                                                                          
299700     MOVE +1                    TO ANSV-KDCALL                            
299800     MOVE MSGI-IDDISTR          TO ANSV-IDDISTR                           
299900     MOVE MSGI-IDFTG            TO ANSV-IDFTG                             
300000     MOVE MSGI-IDKUNDNR         TO ANSV-IDKUNDNR                          
300100     MOVE LEV-KDANMORS          TO ANSV-KDANMORS                          
300200     MOVE ZERO                  TO ANSV-KDORDKL                           
300300                                   ANSV-ADLAGOMR                          
300400                                                                          
300500     CALL W418ANSV USING ANSV-W418ANSV 4113-PCB                           
300600                                       4115-PCB                           
300700                                       4117-PCB                           
300800                                                                          
300900     IF ANSV-OK                                                           
301000       MOVE ANSV-KDARBTYP       TO W-KDARBTYP                             
301100       MOVE ANSV-IDPERSON       TO W-IDPERSON                             
301200       PERFORM IMS-GET-WDP311                                             
301300       IF SEGMENT-FINNS                                                   
301400          MOVE PERS-IDMAIL      TO MEAN-IDMAIL                            
301500       END-IF                                                             
301600     ELSE                                                                 
301700       IF ANSV-KDSVAR = 'S'                                               
301800         MOVE HIGH-VALUE            TO ANSV-KDANMORS                      
301900         CALL W418ANSV USING ANSV-W418ANSV 4113-PCB                       
302000         IF ANSV-OK                                                       
302100           MOVE ANSV-KDARBTYP       TO W-KDARBTYP                         
302200           MOVE ANSV-IDPERSON       TO W-IDPERSON                         
302300           PERFORM IMS-GET-WDP311                                         
302400           IF SEGMENT-FINNS                                               
302500              MOVE PERS-IDMAIL      TO MEAN-IDMAIL                        
302600           END-IF                                                         
302700         END-IF                                                           
302800       END-IF                                                             
302900     END-IF                                                               
303000                                                                          
303100     CALL W418MEAN USING MEAN-W418MEAN MAIL-PCB                           
303200     .                                                                    
303300     EJECT                                                                
303400 HD-EV-UPPDATERA-KN-WDGX4103  SECTION.                                    
303500                                                                          
303600     MOVE ZERO            TO WS-RADPRIS                                   
303700     MOVE LEV-KDANMORS    TO OKOD-KDANMORS                                
303800     CALL W418OKOD USING OKOD-W418OKOD                                    
303900                                                                          
304000*** KOLLA IFALL KODEN GER KNOTA SOM SKALL ATTESTERAS.                     
304100     IF LEV-KDANMORS = '12' OR '22' OR '99' OR  '13' OR '23' OR           
304200                       '84' OR '27' OR '28' OR  '74'                      
304300       CONTINUE                                                           
304400     ELSE                                                                 
304500       IF (OKOD-FL-KRENOT-DIREKT = JA)  OR                                
304600          (OKOD-FL-KRENOT-DIREKT-SKR = JA)  OR                            
304700          (OKOD-FL-KRENOT-EFTER-RT = JA) OR                               
304800          LEV-KDANMORS = '97'                                             
304900                                                                          
305000         MOVE LEV-IDDC            TO W-IDDC-4104                          
305100                                                                          
305200         IF (OKOD-FL-KRENOT-EFTER-RT = JA)  OR                            
305300              LEV-KDANMORS = '97'                                         
305400           MOVE 'RP'  TO W-KDKRENOT-4104                                  
305500         ELSE                                                             
305600           MOVE 'CN'  TO W-KDKRENOT-4104                                  
305700         END-IF                                                           
305800                                                                          
305900         PERFORM IMS-GHU-WDGX4104                                         
306000         IF SEGMENT-FINNS                                                 
306100           IF DIST79-DEALER-PRICE OR                                      
306200              DIST79-ECOM-PRICE                                           
306300             COMPUTE WS-RADPRIS ROUNDED =                                 
306400                     LEV-KVLEVANM-BEKR * LEV-PRARTBTO-LOC                 
306500           ELSE                                                           
306600*                                                                         
306700*CHINA-PRICE1                                                             
306800*INDIA-PRICE1                                                             
306900*KOREA-PRICE1                                                             
307000             PERFORM S22-SET-KDVALISO-AND-RADPRIS                         
307100           END-IF                                                         
307200                                                                          
307300           SUBTRACT WS-RADPRIS FROM 4104-SUKRENOT                         
307400                                                                          
307500           PERFORM IMS-REPL-WDGX4104                                      
307600         END-IF                                                           
307700       END-IF                                                             
307800     END-IF                                                               
307900     .                                                                    
308000     EJECT                                                                
308100 HE-EV-UPPDAT-KOD-WDGX4103  SECTION.                                      
308200                                                                          
308300     PERFORM IMS-GU-WDGX4103                                              
308400     IF SEGMENT-FINNS                                                     
308500       MOVE JA   TO WDR501-UPD-SW                                         
308600     ELSE                                                                 
308700       MOVE NEJ  TO WDR501-UPD-SW                                         
308800     END-IF                                                               
308900                                                                          
309000     MOVE ZERO TO WS-RADPRIS                                              
309100     MOVE ZERO TO WS-NYTT-RADPRIS                                         
309200                                                                          
309300     IF SPAR-KDANMORS = '12' OR '22' OR '99' OR  '13' OR '23' OR          
309400                        '84' OR '27' OR '28' OR  '74'                     
309500       PERFORM HEA-EV-SKAPA-NYTT-WDGX4104                                 
309600     ELSE                                                                 
309700       MOVE SPAR-KDANMORS TO OKOD-KDANMORS                                
309800       CALL W418OKOD USING OKOD-W418OKOD                                  
309900       IF (OKOD-FL-KRENOT-DIREKT = JA)  OR                                
310000          (OKOD-FL-KRENOT-DIREKT-SKR = JA)  OR                            
310100          (OKOD-FL-KRENOT-EFTER-RT = JA) OR                               
310200          SPAR-KDANMORS = '97'                                            
310300                                                                          
310400         MOVE LEV-IDDC            TO W-IDDC-4104                          
310500                                                                          
310600         IF (OKOD-FL-KRENOT-EFTER-RT = JA)  OR                            
310700             SPAR-KDANMORS = '97'                                         
310800           MOVE 'RP'       TO W-KDKRENOT-4104                             
310900                              SPAR-KDKRENOT-4104                          
311000         ELSE                                                             
311100           MOVE 'CN'       TO W-KDKRENOT-4104                             
311200                              SPAR-KDKRENOT-4104                          
311300         END-IF                                                           
311400                                                                          
311500         IF WDR501-FINNS                                                  
311600           PERFORM IMS-GNP-WDGX4104-KVAL                                  
311700           IF SEGMENT-FINNS                                               
311800             MOVE LEV-KDANMORS TO OKOD-KDANMORS                           
311900             CALL W418OKOD USING OKOD-W418OKOD                            
312000             IF (OKOD-FL-KRENOT-DIREKT = JA)  OR                          
312100                (OKOD-FL-KRENOT-DIREKT-SKR = JA)  OR                      
312200                (OKOD-FL-KRENOT-EFTER-RT = JA) OR                         
312300                LEV-KDANMORS = '97'                                       
312400                                                                          
312500               MOVE LEV-IDDC     TO W-IDDC-4104                           
312600                                                                          
312700               IF (OKOD-FL-KRENOT-EFTER-RT = JA)  OR                      
312800                   LEV-KDANMORS = '97'                                    
312900                 MOVE 'RP'       TO W-KDKRENOT-4104                       
313000               ELSE                                                       
313100                 MOVE 'CN'       TO W-KDKRENOT-4104                       
313200               END-IF                                                     
313300                                                                          
313400               IF SPAR-KDKRENOT-4104 = W-KDKRENOT-4104                    
313500                 CONTINUE                                                 
313600               ELSE                                                       
313700                 PERFORM HEB-RAKNA-AV-WDGX4104-OLD                        
313800                 PERFORM HEA-EV-SKAPA-NYTT-WDGX4104                       
313900               END-IF                                                     
314000             ELSE                                                         
314100               PERFORM HEB-RAKNA-AV-WDGX4104-OLD                          
314200             END-IF                                                       
314300           ELSE                                                           
314400             MOVE LEV-KDANMORS TO OKOD-KDANMORS                           
314500             CALL W418OKOD USING OKOD-W418OKOD                            
314600             PERFORM HEA-EV-SKAPA-NYTT-WDGX4104                           
314700           END-IF                                                         
314800         ELSE                                                             
314900           MOVE LEV-KDANMORS TO OKOD-KDANMORS                             
315000           CALL W418OKOD USING OKOD-W418OKOD                              
315100           PERFORM HEA-EV-SKAPA-NYTT-WDGX4104                             
315200         END-IF                                                           
315300       ELSE                                                               
315400         MOVE LEV-KDANMORS TO OKOD-KDANMORS                               
315500         CALL W418OKOD USING OKOD-W418OKOD                                
315600         PERFORM HEA-EV-SKAPA-NYTT-WDGX4104                               
315700       END-IF                                                             
315800     END-IF                                                               
315900     .                                                                    
316000     EJECT                                                                
316100 HEA-EV-SKAPA-NYTT-WDGX4104  SECTION.                                     
316200                                                                          
316300     MOVE ZERO TO WS-RADPRIS                                              
316400                                                                          
316500     IF LEV-KDANMORS = '12' OR '22' OR '99' OR  '13' OR '23' OR           
316600                       '84' OR '27' OR '28' OR  '74'                      
316700       CONTINUE                                                           
316800     ELSE                                                                 
316900       MOVE LEV-KDANMORS TO OKOD-KDANMORS                                 
317000       CALL W418OKOD USING OKOD-W418OKOD                                  
317100       IF (OKOD-FL-KRENOT-DIREKT = JA)  OR                                
317200          (OKOD-FL-KRENOT-DIREKT-SKR = JA)  OR                            
317300          (OKOD-FL-KRENOT-EFTER-RT = JA) OR                               
317400          LEV-KDANMORS = '97'                                             
317500                                                                          
317600         MOVE LEV-IDDC            TO W-IDDC-4104                          
317700                                                                          
317800         IF (OKOD-FL-KRENOT-EFTER-RT = JA)  OR                            
317900              LEV-KDANMORS = '97'                                         
318000           MOVE 'RP'  TO W-KDKRENOT-4104                                  
318100         ELSE                                                             
318200           MOVE 'CN'  TO W-KDKRENOT-4104                                  
318300         END-IF                                                           
318400                                                                          
318500         IF WDR501-FINNS                                                  
318600           PERFORM IMS-GHNP-WDGX4104-KVAL                                 
318700           IF SEGMENT-FINNS                                               
318800             IF DIST79-DEALER-PRICE OR                                    
318900                DIST79-ECOM-PRICE                                         
319000               COMPUTE WS-RADPRIS ROUNDED =                               
319100                              LEV-KVLEVANM-BEKR * LEV-PRARTBTO-LOC        
319200             ELSE                                                         
319300*CHINA-PRICE2                                                             
319400*CHINA-PRICE2                                                             
319500*KOREA-PRICE2                                                             
319600               PERFORM S22-SET-KDVALISO-AND-RADPRIS                       
319700             END-IF                                                       
319800                                                                          
319900             ADD WS-RADPRIS   TO 4104-SUKRENOT                            
320000                                                                          
320100             PERFORM IMS-REPL-WDGX4104                                    
320200           ELSE                                                           
320300             PERFORM S11-INSERT-WDGX4104                                  
320400           END-IF                                                         
320500         ELSE                                                             
320600           MOVE '4103'           TO 4103-IDHTYP                           
320700           MOVE W-IDDISTR-4103   TO 4103-IDDISTR                          
320800           MOVE W-IDKUNDNR-4103  TO 4103-IDKUNDNR                         
320900           MOVE W-IDRAPPNR-4103  TO 4103-IDRAPPNR                         
321000           MOVE LOW-VALUE        TO 4103-LOW-VALUE                        
321100                                                                          
321200           PERFORM IMS-ISRT-WDGX4103                                      
321300           PERFORM S11-INSERT-WDGX4104                                    
321400         END-IF                                                           
321500       END-IF                                                             
321600     END-IF                                                               
321700     .                                                                    
321800     EJECT                                                                
321900 HEB-RAKNA-AV-WDGX4104-OLD  SECTION.                                      
322000                                                                          
322100     MOVE ZERO TO WS-RADPRIS                                              
322200                                                                          
322300     MOVE LEV-IDDC           TO W-IDDC-4104                               
322400     MOVE SPAR-KDKRENOT-4104 TO W-KDKRENOT-4104                           
322500                                                                          
322600     PERFORM IMS-GHNP-WDGX4104-KVAL                                       
322700     IF SEGMENT-FINNS                                                     
322800       IF DIST79-DEALER-PRICE OR                                          
322900          DIST79-ECOM-PRICE                                               
323000         COMPUTE WS-RADPRIS ROUNDED =                                     
323100                 LEV-KVLEVANM-BEKR * LEV-PRARTBTO-LOC                     
323200       ELSE                                                               
323300*                                                                         
323400*CHINA-PRICE3                                                             
323500*INDIA-PRICE3                                                             
323600*KOREA-PRICE3                                                             
323700         PERFORM S22-SET-KDVALISO-AND-RADPRIS                             
323800       END-IF                                                             
323900                                                                          
324000       SUBTRACT WS-RADPRIS FROM 4104-SUKRENOT                             
324100                                                                          
324200       PERFORM IMS-REPL-WDGX4104                                          
324300     END-IF                                                               
324400     .                                                                    
324500     EJECT                                                                
324600 HF-KOLLA-KDLEVATT-WDA201       SECTION.                                  
324700                                                                          
324800*-KOLLA IFALL DET FORTFARANDE FINNS RADER SOM GER KN OCH SKALL            
324900*-ATTESTERAS.                                                             
325000                                                                          
325100     MOVE NEJ TO KNOTA-RAD-FINNS-SW                                       
325200                                                                          
325300     PERFORM IMS-GU-WDGX4103                                              
325400     IF SEGMENT-FINNS                                                     
325500       PERFORM IMS-GHNP-WDGX4104                                          
325600       PERFORM UNTIL SEGMENT-SAKNAS                                       
325700         IF 4104-SUKRENOT = ZERO                                          
325800           PERFORM IMS-DLET-WDGX4104                                      
325900         ELSE                                                             
326000           MOVE JA  TO KNOTA-RAD-FINNS-SW                                 
326100         END-IF                                                           
326200         PERFORM IMS-GHNP-WDGX4104                                        
326300       END-PERFORM                                                        
326400                                                                          
326500       PERFORM IMS-GET-WLKREE01-KVAL                                      
326600       IF SEGMENT-FINNS                                                   
326700         IF KNOTA-RAD-FINNS                                               
326800           IF ANM-KDLEVATT = 0                                            
326900             MOVE 1 TO ANM-KDLEVATT                                       
327000             PERFORM IMS-REPL-KREE                                        
327100           END-IF                                                         
327200         ELSE                                                             
327300           IF ANM-KDLEVATT = 1                                            
327400             MOVE 0 TO ANM-KDLEVATT                                       
327500             PERFORM IMS-REPL-KREE                                        
327600           END-IF                                                         
327700         END-IF                                                           
327800       END-IF                                                             
327900     END-IF                                                               
328000     .                                                                    
328100     EJECT                                                                
328200 HG-KOLLA-IDDC-RET    SECTION.                                            
328300                                                                          
328400     MOVE LEV-KDANMORS TO OKOD-KDANMORS                                   
328500     CALL W418OKOD USING OKOD-W418OKOD                                    
328600                                                                          
328700     IF OKOD-FL-RETILL = JA                                               
328800                                                                          
328900       IF SPAR-ANM-IXDCCLEAR = 0 AND                                      
329000          (SPAR-ANM-IDDC-RET NOT = SPACE)                                 
329100         MOVE SPAR-ANM-IDDC-RET     TO LEV-IDDC-RET                       
329200                                       WS-ANM-IDDC-RET                    
329300         MOVE 0                     TO WS-ANM-IXDCCLEAR                   
329400       ELSE                                                               
329500         IF GMT-IDDC-RET72(1) = SPACE                                     
329600           IF DIST34-KINA-NDC  OR DIST34-INDIA-NDC OR                     
329700              DIST34-KOREA-NDC OR DIST34-TURKEY-NDC OR                    
329800              DIST34-MEXICO-NDC OR                                        
329900              DIST34-BRAZIL-NDC OR                                        
330000              DIST34-MALAYSIA-NDC OR DIST34-THAILAND-NDC OR               
330100              DIST34-TAIWAN-NDC   OR                                      
330110              DIST34-SOUTH-AFRICA-NDC                                     
330200             MOVE GMT-IDDC-RET        TO LEV-IDDC-RET                     
330300                                         WS-ANM-IDDC-RET                  
330400           ELSE                                                           
330500             MOVE WS-CDC-SE           TO LEV-IDDC-RET                     
330600                                         WS-ANM-IDDC-RET                  
330700           END-IF                                                         
330800           MOVE 3                   TO WS-ANM-IXDCCLEAR                   
330900         ELSE                                                             
331000           IF DIST34-KINA-NDC  OR DIST34-INDIA-NDC OR                     
331100              DIST34-KOREA-NDC OR DIST34-TURKEY-NDC OR                    
331200              DIST34-MEXICO-NDC OR                                        
331300              DIST34-BRAZIL-NDC OR                                        
331400              DIST34-MALAYSIA-NDC OR DIST34-THAILAND-NDC OR               
331500              DIST34-TAIWAN-NDC OR                                        
331510              DIST34-SOUTH-AFRICA-NDC                                     
331600             IF GMT-IDDC-RET72(1) = GMT-IDDC-RET                          
331700               MOVE GMT-IDDC-RET      TO LEV-IDDC-RET                     
331800                                         WS-ANM-IDDC-RET                  
331900               MOVE 3                 TO WS-ANM-IXDCCLEAR                 
332000             ELSE                                                         
332100               PERFORM S13-KOLLA-RETUR-DC                                 
332200             END-IF                                                       
332300           ELSE                                                           
332400             IF GMT-IDDC-RET72(1) = WS-CDC-SE                             
332500               MOVE WS-CDC-SE         TO LEV-IDDC-RET                     
332600                                         WS-ANM-IDDC-RET                  
332700               MOVE 3                 TO WS-ANM-IXDCCLEAR                 
332800             ELSE                                                         
332900               PERFORM S13-KOLLA-RETUR-DC                                 
333000             END-IF                                                       
333100           END-IF                                                         
333200         END-IF                                                           
333300       END-IF                                                             
333400     ELSE                                                                 
333500       IF DIST34-KINA-NDC  OR DIST34-INDIA-NDC OR                         
333600          DIST34-KOREA-NDC OR DIST34-TURKEY-NDC OR                        
333700          DIST34-MEXICO-NDC OR                                            
333800          DIST34-BRAZIL-NDC OR                                            
333900          DIST34-MALAYSIA-NDC OR DIST34-THAILAND-NDC OR                   
334000          DIST34-TAIWAN-NDC   OR                                          
334010          DIST34-SOUTH-AFRICA-NDC                                         
334100         IF SPAR-IDDC-RET = GMT-IDDC-RET                                  
334200           CONTINUE                                                       
334300         ELSE                                                             
334400           MOVE GMT-IDDC-RET       TO LEV-IDDC-RET                        
334500         END-IF                                                           
334600       ELSE                                                               
334700         IF SPAR-IDDC-RET = WS-CDC-SE                                     
334800           CONTINUE                                                       
334900         ELSE                                                             
335000           MOVE WS-CDC-SE          TO LEV-IDDC-RET                        
335100         END-IF                                                           
335200       END-IF                                                             
335300       MOVE SPACE                TO WS-ANM-IDDC-RET                       
335400       MOVE 0                    TO WS-ANM-IXDCCLEAR                      
335500     END-IF                                                               
335600     .                                                                    
335700     EJECT                                                                
335800 HH-KOLLA-IDDC-RET-WDA201       SECTION.                                  
335900                                                                          
336000*- KOLLA OM GAMLA KODEN ÄR EN RETURKOD.                                   
336100     MOVE SPAR-KDANMORS  TO OKOD-KDANMORS                                 
336200     CALL W418OKOD USING OKOD-W418OKOD                                    
336300                                                                          
336400     IF OKOD-FL-RETILL = JA                                               
336500       PERFORM HHA-KOLLA-OM-RETUR-KVAR                                    
336600     ELSE                                                                 
336700       MOVE MID-KDANMORS-UPPD TO OKOD-KDANMORS                            
336800       CALL W418OKOD USING OKOD-W418OKOD                                  
336900       IF OKOD-FL-RETILL = JA                                             
337000         PERFORM IMS-GET-WLKREE01-KVAL                                    
337100         IF SEGMENT-FINNS                                                 
337200           IF ANM-IDDC-RET = SPACE                                        
337300             MOVE WS-ANM-IDDC-RET  TO ANM-IDDC-RET                        
337400             MOVE WS-ANM-IXDCCLEAR TO ANM-IXDCCLEAR                       
337500             PERFORM IMS-REPL-KREE                                        
337600           ELSE                                                           
337700             IF ANM-IXDCCLEAR = 3 OR 0                                    
337800               CONTINUE                                                   
337900             ELSE                                                         
338000               IF WS-ANM-IXDCCLEAR = 3                                    
338100                 MOVE WS-ANM-IDDC-RET  TO ANM-IDDC-RET                    
338200                 MOVE WS-ANM-IXDCCLEAR TO ANM-IXDCCLEAR                   
338300                 PERFORM IMS-REPL-KREE                                    
338400               ELSE                                                       
338500                 IF ANM-IXDCCLEAR = 1 AND WS-ANM-IXDCCLEAR = 2            
338600                   MOVE WS-ANM-IDDC-RET  TO ANM-IDDC-RET                  
338700                   MOVE WS-ANM-IXDCCLEAR TO ANM-IXDCCLEAR                 
338800                   PERFORM IMS-REPL-KREE                                  
338900                 END-IF                                                   
339000               END-IF                                                     
339100             END-IF                                                       
339200           END-IF                                                         
339300         END-IF                                                           
339400       ELSE                                                               
339500         CONTINUE                                                         
339600       END-IF                                                             
339700     END-IF                                                               
339800     .                                                                    
339900     EJECT                                                                
340000 HHA-KOLLA-OM-RETUR-KVAR        SECTION.                                  
340100                                                                          
340200     MOVE NEJ TO RETURRADER-KVAR-SW                                       
340300     MOVE NEJ TO IXDCCLEAR-2-SW                                           
340400     MOVE NEJ TO IXDCCLEAR-3-SW                                           
340500                                                                          
340600     PERFORM IMS-GU-WLKREE01                                              
340700     IF SEGMENT-FINNS                                                     
340800       MOVE ANM-IDDC-RET  TO SPAR-ANM-IDDC-RET                            
340900       MOVE ANM-IXDCCLEAR TO SPAR-ANM-IXDCCLEAR                           
341000       PERFORM IMS-GET-KREE11-GNP                                         
341100       PERFORM UNTIL SEGMENT-SAKNAS                                       
341200         IF LEV-KDKREBEH(1:1) = 'N'                                       
341300           CONTINUE                                                       
341400         ELSE                                                             
341500           MOVE LEV-KDANMORS   TO OKOD-KDANMORS                           
341600           CALL W418OKOD USING OKOD-W418OKOD                              
341700                                                                          
341800           IF OKOD-FL-RETILL = JA                                         
341900             MOVE JA TO RETURRADER-KVAR-SW                                
342000             IF SPAR-ANM-IXDCCLEAR = 0                                    
342100               CONTINUE                                                   
342200             ELSE                                                         
342300               IF LEV-IDDC-RET = GMT-IDDC-RET72 (2)                       
342400                 MOVE JA TO IXDCCLEAR-2-SW                                
342500               END-IF                                                     
342600               IF DIST34-KINA-NDC  OR DIST34-INDIA-NDC OR                 
342700                  DIST34-KOREA-NDC OR DIST34-TURKEY-NDC OR                
342800                  DIST34-MEXICO-NDC OR                                    
342900                  DIST34-BRAZIL-NDC OR                                    
343000                  DIST34-MALAYSIA-NDC OR DIST34-THAILAND-NDC OR           
343100                  DIST34-TAIWAN-NDC   OR                                  
343110                  DIST34-SOUTH-AFRICA-NDC                                 
343200                 IF LEV-IDDC-RET = GMT-IDDC-RET72 (3) OR                  
343300                    LEV-IDDC-RET = GMT-IDDC-RET                           
343400                   MOVE JA TO IXDCCLEAR-3-SW                              
343500                 END-IF                                                   
343600               ELSE                                                       
343700                 IF LEV-IDDC-RET = GMT-IDDC-RET72 (3) OR                  
343800                    LEV-IDDC-RET = WS-CDC-SE                              
343900                   MOVE JA TO IXDCCLEAR-3-SW                              
344000                 END-IF                                                   
344100               END-IF                                                     
344200             END-IF                                                       
344300           END-IF                                                         
344400         END-IF                                                           
344500                                                                          
344600         PERFORM IMS-GET-KREE11-GNP                                       
344700       END-PERFORM                                                        
344800     END-IF                                                               
344900                                                                          
345000     IF RETURRADER-KVAR                                                   
345100       IF SPAR-ANM-IXDCCLEAR = 3                                          
345200         IF IXDCCLEAR-3-SW = JA                                           
345300           CONTINUE                                                       
345400         ELSE                                                             
345500           IF IXDCCLEAR-2-SW = JA                                         
345600             PERFORM IMS-GET-WLKREE01-KVAL                                
345700             MOVE GMT-IDDC-RET72 (2)  TO ANM-IDDC-RET                     
345800             MOVE 2                   TO ANM-IXDCCLEAR                    
345900             PERFORM IMS-REPL-KREE                                        
346000           ELSE                                                           
346100             PERFORM IMS-GET-WLKREE01-KVAL                                
346200             MOVE GMT-IDDC-RET72 (1)  TO ANM-IDDC-RET                     
346300             MOVE 1                   TO ANM-IXDCCLEAR                    
346400             PERFORM IMS-REPL-KREE                                        
346500           END-IF                                                         
346600         END-IF                                                           
346700       ELSE                                                               
346800         IF SPAR-ANM-IXDCCLEAR = 2                                        
346900           IF IXDCCLEAR-2-SW = JA                                         
347000             CONTINUE                                                     
347100           ELSE                                                           
347200             PERFORM IMS-GET-WLKREE01-KVAL                                
347300             MOVE GMT-IDDC-RET72 (1)  TO ANM-IDDC-RET                     
347400             MOVE 1                   TO ANM-IXDCCLEAR                    
347500             PERFORM IMS-REPL-KREE                                        
347600           END-IF                                                         
347700         ELSE                                                             
347800           IF SPAR-ANM-IXDCCLEAR = 1                                      
347900             IF IXDCCLEAR-3-SW = JA                                       
348000               PERFORM IMS-GET-WLKREE01-KVAL                              
348100               MOVE GMT-IDDC-RET72 (3)  TO ANM-IDDC-RET                   
348200               IF ANM-IDDC-RET = SPACE                                    
348300                 IF DIST34-KINA-NDC  OR DIST34-INDIA-NDC OR               
348400                    DIST34-KOREA-NDC OR DIST34-TURKEY-NDC OR              
348500                    DIST34-MEXICO-NDC OR                                  
348600                    DIST34-BRAZIL-NDC OR                                  
348700                    DIST34-MALAYSIA-NDC OR DIST34-THAILAND-NDC OR         
348800                    DIST34-TAIWAN-NDC   OR                                
348810                    DIST34-SOUTH-AFRICA-NDC                               
348900                   MOVE GMT-IDDC-RET    TO ANM-IDDC-RET                   
349000                 ELSE                                                     
349100                   MOVE WS-CDC-SE       TO ANM-IDDC-RET                   
349200                 END-IF                                                   
349300               END-IF                                                     
349400               MOVE 3                   TO ANM-IXDCCLEAR                  
349500               PERFORM IMS-REPL-KREE                                      
349600             ELSE                                                         
349700               IF IXDCCLEAR-2-SW = JA                                     
349800                 PERFORM IMS-GET-WLKREE01-KVAL                            
349900                 MOVE GMT-IDDC-RET72 (2)  TO ANM-IDDC-RET                 
350000                 MOVE 2                   TO ANM-IXDCCLEAR                
350100                 PERFORM IMS-REPL-KREE                                    
350200               END-IF                                                     
350300             END-IF                                                       
350400           END-IF                                                         
350500         END-IF                                                           
350600       END-IF                                                             
350700     ELSE                                                                 
350800       PERFORM IMS-GET-WLKREE01-KVAL                                      
350900       MOVE SPACE            TO ANM-IDDC-RET                              
351000       MOVE 0                TO ANM-IXDCCLEAR                             
351100       PERFORM IMS-REPL-KREE                                              
351200     END-IF                                                               
351300     .                                                                    
351400     EJECT                                                                
351500 HI-KOLLA-IDDC-RET-OEVRIGT  SECTION.                                      
351600                                                                          
351700*- KOLLA OM GAMLA KODEN ÄR EN RETURKOD.                                   
351800     MOVE SPAR-KDANMORS  TO OKOD-KDANMORS                                 
351900     CALL W418OKOD USING OKOD-W418OKOD                                    
352000                                                                          
352100     IF OKOD-FL-RETILL = JA                                               
352200       PERFORM HIA-KOLLA-RETURRADER-OEVRIGT                               
352300     ELSE                                                                 
352400       MOVE MID-KDANMORS-UPPD TO OKOD-KDANMORS                            
352500       CALL W418OKOD USING OKOD-W418OKOD                                  
352600       IF OKOD-FL-RETILL = JA                                             
352700         PERFORM IMS-GET-WLKREE01-KVAL                                    
352800         IF SEGMENT-FINNS                                                 
352900           IF ANM-IDDC-RET = SPACE                                        
353000             MOVE GMT-IDDC-RET     TO ANM-IDDC-RET                        
353100             MOVE 3                TO ANM-IXDCCLEAR                       
353200             PERFORM IMS-REPL-KREE                                        
353300           ELSE                                                           
353400             CONTINUE                                                     
353500           END-IF                                                         
353600         END-IF                                                           
353700       ELSE                                                               
353800         CONTINUE                                                         
353900       END-IF                                                             
354000     END-IF                                                               
354100     .                                                                    
354200     EJECT                                                                
354300 HIA-KOLLA-RETURRADER-OEVRIGT  SECTION.                                   
354400                                                                          
354500     MOVE NEJ TO RETURRADER-KVAR-SW                                       
354600                                                                          
354700     PERFORM IMS-GU-WLKREE01                                              
354800     IF SEGMENT-FINNS                                                     
354900       MOVE ANM-IDDC-RET  TO SPAR-ANM-IDDC-RET                            
355000       MOVE ANM-IXDCCLEAR TO SPAR-ANM-IXDCCLEAR                           
355100       PERFORM IMS-GET-KREE11-GNP                                         
355200       PERFORM UNTIL SEGMENT-SAKNAS OR RETURRADER-KVAR-SW = JA            
355300         IF LEV-KDKREBEH(1:1) = 'N'                                       
355400           CONTINUE                                                       
355500         ELSE                                                             
355600           MOVE LEV-KDANMORS   TO OKOD-KDANMORS                           
355700           CALL W418OKOD USING OKOD-W418OKOD                              
355800                                                                          
355900           IF OKOD-FL-RETILL = JA                                         
356000             MOVE JA TO RETURRADER-KVAR-SW                                
356100           END-IF                                                         
356200         END-IF                                                           
356300                                                                          
356400         PERFORM IMS-GET-KREE11-GNP                                       
356500       END-PERFORM                                                        
356600     END-IF                                                               
356700                                                                          
356800     IF RETURRADER-KVAR                                                   
356900       CONTINUE                                                           
357000     ELSE                                                                 
357100       PERFORM IMS-GET-WLKREE01-KVAL                                      
357200       MOVE SPACE            TO ANM-IDDC-RET                              
357300       MOVE 0                TO ANM-IXDCCLEAR                             
357400       PERFORM IMS-REPL-KREE                                              
357500     END-IF                                                               
357600     .                                                                    
357700     EJECT                                                                
357800 HJ-EV-UPPD-DC-RET-WDA201 SECTION.                                        
357900                                                                          
358000     MOVE NEJ TO RETURRADER-KVAR-SW                                       
358100     MOVE NEJ TO IXDCCLEAR-2-SW                                           
358200     MOVE NEJ TO IXDCCLEAR-3-SW                                           
358300                                                                          
358400     PERFORM IMS-GU-WLKREE01                                              
358500     IF SEGMENT-FINNS                                                     
358600       MOVE ANM-IDDC-RET  TO SPAR-ANM-IDDC-RET                            
358700       MOVE ANM-IXDCCLEAR TO SPAR-ANM-IXDCCLEAR                           
358800       PERFORM IMS-GET-KREE11-GNP                                         
358900       PERFORM UNTIL SEGMENT-SAKNAS                                       
359000         IF LEV-KDKREBEH(1:1) = 'N'                                       
359100           CONTINUE                                                       
359200         ELSE                                                             
359300           MOVE LEV-KDANMORS   TO OKOD-KDANMORS                           
359400           CALL W418OKOD USING OKOD-W418OKOD                              
359500                                                                          
359600           IF OKOD-FL-RETILL = JA                                         
359700             MOVE JA TO RETURRADER-KVAR-SW                                
359800             IF SPAR-ANM-IXDCCLEAR = 0                                    
359900               CONTINUE                                                   
360000             ELSE                                                         
360100               IF LEV-IDDC-RET = GMT-IDDC-RET72 (2)                       
360200                 MOVE JA TO IXDCCLEAR-2-SW                                
360300               END-IF                                                     
360400               IF DIST34-KINA-NDC  OR DIST34-INDIA-NDC OR                 
360500                  DIST34-KOREA-NDC OR DIST34-TURKEY-NDC OR                
360600                  DIST34-MEXICO-NDC OR                                    
360700                  DIST34-BRAZIL-NDC OR                                    
360800                  DIST34-MALAYSIA-NDC OR DIST34-THAILAND-NDC OR           
360900                  DIST34-TAIWAN-NDC   OR                                  
360910                  DIST34-SOUTH-AFRICA-NDC                                 
361000                 IF LEV-IDDC-RET = GMT-IDDC-RET72 (3) OR                  
361100                    LEV-IDDC-RET = GMT-IDDC-RET                           
361200                   MOVE JA TO IXDCCLEAR-3-SW                              
361300                 END-IF                                                   
361400               ELSE                                                       
361500                 IF LEV-IDDC-RET = GMT-IDDC-RET72 (3) OR                  
361600                    LEV-IDDC-RET = WS-CDC-SE                              
361700                   MOVE JA TO IXDCCLEAR-3-SW                              
361800                 END-IF                                                   
361900               END-IF                                                     
362000             END-IF                                                       
362100           END-IF                                                         
362200         END-IF                                                           
362300                                                                          
362400         PERFORM IMS-GET-KREE11-GNP                                       
362500       END-PERFORM                                                        
362600     END-IF                                                               
362700                                                                          
362800     IF RETURRADER-KVAR                                                   
362900       IF SPAR-ANM-IXDCCLEAR = 3                                          
363000         IF IXDCCLEAR-3-SW = JA                                           
363100           CONTINUE                                                       
363200         ELSE                                                             
363300           IF IXDCCLEAR-2-SW = JA                                         
363400             PERFORM IMS-GET-WLKREE01-KVAL                                
363500             MOVE GMT-IDDC-RET72 (2)  TO ANM-IDDC-RET                     
363600             MOVE 2                   TO ANM-IXDCCLEAR                    
363700             PERFORM IMS-REPL-KREE                                        
363800           ELSE                                                           
363900             PERFORM IMS-GET-WLKREE01-KVAL                                
364000             MOVE GMT-IDDC-RET72 (1)  TO ANM-IDDC-RET                     
364100             MOVE 1                   TO ANM-IXDCCLEAR                    
364200             PERFORM IMS-REPL-KREE                                        
364300           END-IF                                                         
364400         END-IF                                                           
364500       ELSE                                                               
364600         IF SPAR-ANM-IXDCCLEAR = 2                                        
364700           IF IXDCCLEAR-2-SW = JA                                         
364800             CONTINUE                                                     
364900           ELSE                                                           
365000             PERFORM IMS-GET-WLKREE01-KVAL                                
365100             MOVE GMT-IDDC-RET72 (1)  TO ANM-IDDC-RET                     
365200             MOVE 1                   TO ANM-IXDCCLEAR                    
365300             PERFORM IMS-REPL-KREE                                        
365400           END-IF                                                         
365500         END-IF                                                           
365600       END-IF                                                             
365700     ELSE                                                                 
365800       PERFORM IMS-GET-WLKREE01-KVAL                                      
365900       MOVE SPACE            TO ANM-IDDC-RET                              
366000       MOVE 0                TO ANM-IXDCCLEAR                             
366100       PERFORM IMS-REPL-KREE                                              
366200     END-IF                                                               
366300     .                                                                    
366400     EJECT                                                                
366500 HK-EV-UPPD-DC-RET-OEVRIGT SECTION.                                       
366600                                                                          
366700     MOVE NEJ TO RETURRADER-KVAR-SW                                       
366800                                                                          
366900     PERFORM IMS-GU-WLKREE01                                              
367000     IF SEGMENT-FINNS                                                     
367100       MOVE ANM-IDDC-RET  TO SPAR-ANM-IDDC-RET                            
367200       MOVE ANM-IXDCCLEAR TO SPAR-ANM-IXDCCLEAR                           
367300       PERFORM IMS-GET-KREE11-GNP                                         
367400       PERFORM UNTIL SEGMENT-SAKNAS OR RETURRADER-KVAR-SW = JA            
367500         IF LEV-KDKREBEH(1:1) = 'N'                                       
367600           CONTINUE                                                       
367700         ELSE                                                             
367800           MOVE LEV-KDANMORS   TO OKOD-KDANMORS                           
367900           CALL W418OKOD USING OKOD-W418OKOD                              
368000                                                                          
368100           IF OKOD-FL-RETILL = JA                                         
368200             MOVE JA TO RETURRADER-KVAR-SW                                
368300           END-IF                                                         
368400         END-IF                                                           
368500                                                                          
368600         PERFORM IMS-GET-KREE11-GNP                                       
368700       END-PERFORM                                                        
368800     END-IF                                                               
368900                                                                          
369000     IF RETURRADER-KVAR                                                   
369100       CONTINUE                                                           
369200     ELSE                                                                 
369300       PERFORM IMS-GET-WLKREE01-KVAL                                      
369400       MOVE SPACE            TO ANM-IDDC-RET                              
369500       MOVE 0                TO ANM-IXDCCLEAR                             
369600       PERFORM IMS-REPL-KREE                                              
369700     END-IF                                                               
369800     .                                                                    
369900     EJECT                                                                
370000 HL-UPPDAT-KN-WDGX4103-IGEN  SECTION.                                     
370100                                                                          
370200     MOVE ZERO TO WS-RADPRIS                                              
370300                                                                          
370400     IF LEV-KDANMORS = '12' OR '22' OR '99' OR  '13' OR '23' OR           
370500                       '84' OR '27' OR '28' OR  '74'                      
370600       CONTINUE                                                           
370700     ELSE                                                                 
370800       MOVE LEV-KDANMORS TO OKOD-KDANMORS                                 
370900       CALL W418OKOD USING OKOD-W418OKOD                                  
371000       IF (OKOD-FL-KRENOT-DIREKT = JA)  OR                                
371100          (OKOD-FL-KRENOT-DIREKT-SKR = JA)  OR                            
371200          (OKOD-FL-KRENOT-EFTER-RT = JA) OR                               
371300          LEV-KDANMORS = '97'                                             
371400                                                                          
371500         MOVE LEV-IDDC            TO W-IDDC-4104                          
371600                                                                          
371700         IF (OKOD-FL-KRENOT-EFTER-RT = JA)  OR                            
371800              LEV-KDANMORS = '97'                                         
371900           MOVE 'RP'  TO W-KDKRENOT-4104                                  
372000         ELSE                                                             
372100           MOVE 'CN'  TO W-KDKRENOT-4104                                  
372200         END-IF                                                           
372300                                                                          
372400         PERFORM IMS-GU-WDGX4103                                          
372500         IF SEGMENT-FINNS                                                 
372600           PERFORM IMS-GHNP-WDGX4104-KVAL                                 
372700           IF SEGMENT-FINNS                                               
372800             IF DIST79-DEALER-PRICE OR                                    
372900                DIST79-ECOM-PRICE                                         
373000               COMPUTE WS-RADPRIS ROUNDED =                               
373100                              LEV-KVLEVANM-BEKR * LEV-PRARTBTO-LOC        
373200             ELSE                                                         
373300*CHINA-PRICE4                                                             
373400*INDIA-PRICE4                                                             
373500*KOREA-PRICE4                                                             
373600               PERFORM S22-SET-KDVALISO-AND-RADPRIS                       
373700             END-IF                                                       
373800                                                                          
373900             ADD WS-RADPRIS   TO 4104-SUKRENOT                            
374000                                                                          
374100             PERFORM IMS-REPL-WDGX4104                                    
374200           ELSE                                                           
374300             PERFORM S11-INSERT-WDGX4104                                  
374400           END-IF                                                         
374500         ELSE                                                             
374600           MOVE '4103'           TO 4103-IDHTYP                           
374700           MOVE W-IDDISTR-4103   TO 4103-IDDISTR                          
374800           MOVE W-IDKUNDNR-4103  TO 4103-IDKUNDNR                         
374900           MOVE W-IDRAPPNR-4103  TO 4103-IDRAPPNR                         
375000           MOVE LOW-VALUE        TO 4103-LOW-VALUE                        
375100                                                                          
375200           PERFORM IMS-ISRT-WDGX4103                                      
375300           PERFORM S11-INSERT-WDGX4104                                    
375400         END-IF                                                           
375500       END-IF                                                             
375600     END-IF                                                               
375700     .                                                                    
375800     EJECT                                                                
375900 HM-UPPD-DC-RET-WDA201-IGEN  SECTION.                                     
376000                                                                          
376100     MOVE NEJ TO IXDCCLEAR-2-SW                                           
376200     MOVE NEJ TO IXDCCLEAR-3-SW                                           
376300                                                                          
376400     PERFORM IMS-GU-WLKREE01                                              
376500     IF SEGMENT-FINNS                                                     
376600       MOVE ANM-IDDC-RET  TO SPAR-ANM-IDDC-RET                            
376700       MOVE ANM-IXDCCLEAR TO SPAR-ANM-IXDCCLEAR                           
376800       PERFORM IMS-GET-KREE11-GNP                                         
376900       PERFORM UNTIL SEGMENT-SAKNAS                                       
377000         IF LEV-KDKREBEH(1:1) = 'N'                                       
377100           CONTINUE                                                       
377200         ELSE                                                             
377300           MOVE LEV-KDANMORS   TO OKOD-KDANMORS                           
377400           CALL W418OKOD USING OKOD-W418OKOD                              
377500                                                                          
377600           IF OKOD-FL-RETILL = JA                                         
377700             IF LEV-IDDC-RET = GMT-IDDC-RET72 (2)                         
377800               MOVE JA TO IXDCCLEAR-2-SW                                  
377900             END-IF                                                       
378000             IF DIST34-KINA-NDC  OR DIST34-INDIA-NDC OR                   
378100                DIST34-KOREA-NDC OR DIST34-TURKEY-NDC OR                  
378200                DIST34-MEXICO-NDC OR                                      
378300                DIST34-BRAZIL-NDC OR                                      
378400                DIST34-MALAYSIA-NDC OR DIST34-THAILAND-NDC OR             
378500                DIST34-TAIWAN-NDC   OR                                    
378510                DIST34-SOUTH-AFRICA-NDC                                   
378600               IF LEV-IDDC-RET = GMT-IDDC-RET72 (3) OR                    
378700                  LEV-IDDC-RET = GMT-IDDC-RET                             
378800                 MOVE JA TO IXDCCLEAR-3-SW                                
378900               END-IF                                                     
379000             ELSE                                                         
379100               IF LEV-IDDC-RET = GMT-IDDC-RET72 (3) OR                    
379200                  LEV-IDDC-RET = WS-CDC-SE                                
379300                 MOVE JA TO IXDCCLEAR-3-SW                                
379400               END-IF                                                     
379500             END-IF                                                       
379600           END-IF                                                         
379700         END-IF                                                           
379800                                                                          
379900         PERFORM IMS-GET-KREE11-GNP                                       
380000       END-PERFORM                                                        
380100     END-IF                                                               
380200                                                                          
380300     IF SPAR-ANM-IXDCCLEAR = 3                                            
380400       IF IXDCCLEAR-3-SW = JA                                             
380500         CONTINUE                                                         
380600       ELSE                                                               
380700         IF IXDCCLEAR-2-SW = JA                                           
380800           PERFORM IMS-GET-WLKREE01-KVAL                                  
380900           MOVE GMT-IDDC-RET72 (2)  TO ANM-IDDC-RET                       
381000           MOVE 2                   TO ANM-IXDCCLEAR                      
381100           PERFORM IMS-REPL-KREE                                          
381200         ELSE                                                             
381300           PERFORM IMS-GET-WLKREE01-KVAL                                  
381400           MOVE GMT-IDDC-RET72 (1)  TO ANM-IDDC-RET                       
381500           MOVE 1                   TO ANM-IXDCCLEAR                      
381600           PERFORM IMS-REPL-KREE                                          
381700         END-IF                                                           
381800       END-IF                                                             
381900     ELSE                                                                 
382000       IF SPAR-ANM-IXDCCLEAR = 2                                          
382100         IF IXDCCLEAR-3-SW = JA                                           
382200           PERFORM IMS-GET-WLKREE01-KVAL                                  
382300           MOVE GMT-IDDC-RET72 (3)  TO ANM-IDDC-RET                       
382400           IF ANM-IDDC-RET = SPACE                                        
382500             IF DIST34-KINA-NDC  OR DIST34-INDIA-NDC OR                   
382600                DIST34-KOREA-NDC OR DIST34-TURKEY-NDC OR                  
382700                DIST34-MEXICO-NDC OR                                      
382800                DIST34-BRAZIL-NDC OR                                      
382900                DIST34-MALAYSIA-NDC OR DIST34-THAILAND-NDC OR             
383000                DIST34-TAIWAN-NDC   OR                                    
383010                DIST34-SOUTH-AFRICA-NDC                                   
383100               MOVE GMT-IDDC-RET    TO ANM-IDDC-RET                       
383200             ELSE                                                         
383300               MOVE WS-CDC-SE       TO ANM-IDDC-RET                       
383400             END-IF                                                       
383500           END-IF                                                         
383600           MOVE 3                   TO ANM-IXDCCLEAR                      
383700           PERFORM IMS-REPL-KREE                                          
383800         ELSE                                                             
383900           IF IXDCCLEAR-2-SW = JA                                         
384000             CONTINUE                                                     
384100           ELSE                                                           
384200             PERFORM IMS-GET-WLKREE01-KVAL                                
384300             MOVE GMT-IDDC-RET72 (1)  TO ANM-IDDC-RET                     
384400             MOVE 1                   TO ANM-IXDCCLEAR                    
384500             PERFORM IMS-REPL-KREE                                        
384600           END-IF                                                         
384700         END-IF                                                           
384800       ELSE                                                               
384900         IF SPAR-ANM-IXDCCLEAR = 1                                        
385000           IF IXDCCLEAR-3-SW = JA                                         
385100             PERFORM IMS-GET-WLKREE01-KVAL                                
385200             MOVE GMT-IDDC-RET72 (3)  TO ANM-IDDC-RET                     
385300             IF ANM-IDDC-RET = SPACE                                      
385400               IF DIST34-KINA-NDC  OR DIST34-INDIA-NDC OR                 
385500                  DIST34-KOREA-NDC OR DIST34-TURKEY-NDC OR                
385600                  DIST34-MEXICO-NDC OR                                    
385700                  DIST34-BRAZIL-NDC OR                                    
385800                  DIST34-MALAYSIA-NDC OR DIST34-THAILAND-NDC OR           
385900                  DIST34-TAIWAN-NDC   OR                                  
385910                  DIST34-SOUTH-AFRICA-NDC                                 
386000                 MOVE GMT-IDDC-RET    TO ANM-IDDC-RET                     
386100               ELSE                                                       
386200                 MOVE WS-CDC-SE       TO ANM-IDDC-RET                     
386300               END-IF                                                     
386400             END-IF                                                       
386500             MOVE 3                   TO ANM-IXDCCLEAR                    
386600             PERFORM IMS-REPL-KREE                                        
386700           ELSE                                                           
386800             IF IXDCCLEAR-2-SW = JA                                       
386900               PERFORM IMS-GET-WLKREE01-KVAL                              
387000               MOVE GMT-IDDC-RET72 (2)  TO ANM-IDDC-RET                   
387100               MOVE 2                   TO ANM-IXDCCLEAR                  
387200               PERFORM IMS-REPL-KREE                                      
387300             ELSE                                                         
387400               CONTINUE                                                   
387500             END-IF                                                       
387600           END-IF                                                         
387700         ELSE                                                             
387800           IF SPAR-ANM-IXDCCLEAR = 0 AND SPAR-ANM-IDDC-RET = SPACE        
387900             IF IXDCCLEAR-3-SW = JA                                       
388000               PERFORM IMS-GET-WLKREE01-KVAL                              
388100               MOVE GMT-IDDC-RET72 (3)  TO ANM-IDDC-RET                   
388200               IF ANM-IDDC-RET = SPACE                                    
388300                 IF DIST34-KINA-NDC  OR DIST34-INDIA-NDC OR               
388400                    DIST34-KOREA-NDC OR DIST34-TURKEY-NDC OR              
388500                    DIST34-MEXICO-NDC OR                                  
388600                    DIST34-BRAZIL-NDC OR                                  
388700                    DIST34-MALAYSIA-NDC OR DIST34-THAILAND-NDC OR         
388800                    DIST34-TAIWAN-NDC   OR                                
388810                    DIST34-SOUTH-AFRICA-NDC                               
388900                   MOVE GMT-IDDC-RET    TO ANM-IDDC-RET                   
389000                 ELSE                                                     
389100                   MOVE WS-CDC-SE       TO ANM-IDDC-RET                   
389200                 END-IF                                                   
389300               END-IF                                                     
389400               MOVE 3                   TO ANM-IXDCCLEAR                  
389500               PERFORM IMS-REPL-KREE                                      
389600             ELSE                                                         
389700               IF IXDCCLEAR-2-SW = JA                                     
389800                 PERFORM IMS-GET-WLKREE01-KVAL                            
389900                 MOVE GMT-IDDC-RET72 (2)  TO ANM-IDDC-RET                 
390000                 MOVE 2                   TO ANM-IXDCCLEAR                
390100                 PERFORM IMS-REPL-KREE                                    
390200               ELSE                                                       
390300                 PERFORM IMS-GET-WLKREE01-KVAL                            
390400                 MOVE GMT-IDDC-RET72 (1)  TO ANM-IDDC-RET                 
390500                 MOVE 1                   TO ANM-IXDCCLEAR                
390600                 PERFORM IMS-REPL-KREE                                    
390700               END-IF                                                     
390800             END-IF                                                       
390900           END-IF                                                         
391000         END-IF                                                           
391100       END-IF                                                             
391200     END-IF                                                               
391300     .                                                                    
391400     EJECT                                                                
391500 HN-UPPD-DC-RET-OEVRIGT-IGEN  SECTION.                                    
391600                                                                          
391700     PERFORM IMS-GET-WLKREE01-KVAL                                        
391800     IF SEGMENT-FINNS                                                     
391900       IF ANM-IDDC-RET = SPACE                                            
392000         MOVE GMT-IDDC-RET     TO ANM-IDDC-RET                            
392100         MOVE 3                TO ANM-IXDCCLEAR                           
392200         PERFORM IMS-REPL-KREE                                            
392300       END-IF                                                             
392400     END-IF                                                               
392500     .                                                                    
392600     EJECT                                                                
392700 I-SKRIV-DETALJLISTA            SECTION.                                  
392800                                                                          
392900     PERFORM IA-KOLLA-PRINTER                                             
393000     IF INDATA-OK                                                         
393100        PERFORM IMS-GHU-WLKREE11                                          
393200                                                                          
393300        MOVE LEV-IDDC           TO W-IDDC                                 
393400                                   W-IDDC-B6                              
393500                                   WS-IDDC                                
393600                                                                          
393700        MOVE JA                 TO WDB6-SW                                
393800        PERFORM IMS-GU-WDB601                                             
393900        IF SEGMENT-SAKNAS                                                 
394000          MOVE NEJ              TO WDB6-SW                                
394100        END-IF                                                            
394200                                                                          
394300        MOVE PRT-IDPRTLST       TO UDET-IDPRTLST                          
394400                                                                          
394500        PERFORM IB-FIXA-LEVANM-INFO                                       
394600        PERFORM IC-FIXA-FAKT-INFO                                         
394700        PERFORM ID-FIXA-ART-INFO                                          
394800                                                                          
394900        CALL W418UDET USING UDET-W418UDET ALT-PCB                         
395000                                                                          
395100        MOVE INF-PRINT-BEG      TO MED-IDMFSINF                           
395200        CALL WMEDKONV USING MED-WMEDAREA                                  
395300        MOVE MED-MFSINF         TO MOD-TEMFSINF                           
395400     END-IF                                                               
395500                                                                          
395600     .                                                                    
395700     EJECT                                                                
395800                                                                          
395900 IA-KOLLA-PRINTER               SECTION.                                  
396000                                                                          
396100     MOVE SPACE                TO PRT-IDPRTLST                            
396200     MOVE '4LA'                TO PRT-IDPRTLST(1:3)                       
396300                                                                          
396400     MOVE MID-IDPRT            TO PRT-IDPRTLST(4:3)                       
396500     MOVE 1                    TO PRT-KDCALL                              
396600     CALL W006PRT USING PRT-W006PRT                                       
396700                                                                          
396800     IF PRT-KDSVAR                     = 'F'                              
396900         MOVE ERR-WRONG-PRINTER  TO MED-IDMFSFEL                          
397000         MOVE NEJ              TO INDATA-SW                               
397100         MOVE MFS-ALFA-FAELT-FEL TO MOD-IDPRT-ATTR                        
397200         PERFORM MFS-ROER-EJ-FAELT-IN                                     
397300         PERFORM MFS-ROER-EJ-FAELT-UT                                     
397400         CALL WMEDKONV USING MED-WMEDAREA                                 
397500         MOVE MED-MFSFEL        TO MOD-TEMFSFEL                           
397600     END-IF                                                               
397700     .                                                                    
397800     EJECT                                                                
397900                                                                          
398000 IB-FIXA-LEVANM-INFO            SECTION.                                  
398100                                                                          
398200     MOVE MSGI-IDDISTR          TO UDET-IDDISTR                           
398300     MOVE MSGI-IDKUNDNR         TO UDET-IDKUNDNR                          
398400     MOVE MSGI-IDRAPPNR         TO UDET-IDRAPPNR                          
398500                                                                          
398600     MOVE LEV-IDARTNR           TO UDET-IDARTNR                           
398700                                   W-IDARTNR-A2                           
398800     MOVE LEV-IDRADNR           TO UDET-IDRADNR                           
398900                                   W-IDRADNR-A2                           
399000     MOVE LEV-KDANMORS          TO UDET-KDANMORS                          
399100     MOVE LEV-KVLEVANM-BEKR     TO UDET-KVLEVANM-BEKR                     
399200     MOVE LEV-IDORDNR7          TO UDET-IDORDNR5                          
399300     MOVE LEV-IDKOLLI           TO UDET-IDKOLLI                           
399400                                                                          
399500     IF CDC-SE OR GOOD-DDC                                                
399600       IF DIST79-DEALER-PRICE OR                                          
399700          DIST79-ECOM-PRICE                                               
399800         MOVE LEV-PRARTBTO-LOC  TO UDET-PRARTBTO                          
399900       ELSE                                                               
400000         MOVE LEV-PRARTBTO      TO UDET-PRARTBTO                          
400100       END-IF                                                             
400200       MOVE LEV-KDFAKTYP        TO UDET-KDFAKTYP                          
400300       MOVE LEV-IDFAKT          TO UDET-IDFAKT                            
400400       MOVE LEV-TIFAKT          TO UDET-TIFAKT                            
400500     ELSE                                                                 
400600       MOVE LEV-PRARTBTO-LOCINV TO UDET-PRARTBTO                          
400700       MOVE LEV-KDFAKTYP        TO UDET-KDFAKTYP                          
400800       MOVE LEV-IDFAKT-LOC      TO UDET-IDFAKT                            
400900       MOVE LEV-TIFAKT-LOC      TO UDET-TIFAKT                            
401000     END-IF                                                               
401100                                                                          
401200     MOVE +1                    TO INDX                                   
401300     PERFORM UNTIL INDX        >  3                                       
401400      MOVE SPACE                TO UDET-TEANMNOT-REG(INDX)                
401500                                  UDET-TEANMNOT-ADM(INDX)                 
401600                                  UDET-TEANMNOT-REM(INDX)                 
401700                                  UDET-TEANMNOT-RET(INDX)                 
401800                                                                          
401900      ADD +1                    TO INDX                                   
402000     END-PERFORM                                                          
402100                                                                          
402200     IF LEV-FLTEXT             =  JA                                      
402300        PERFORM IMS-GNP-WLKREE21                                          
402400        IF SEGMENT-FINNS                                                  
402500           MOVE +1              TO INDX                                   
402600           PERFORM UNTIL INDX           >  3                              
402700            MOVE TXT-TEANMNOT-REG(INDX) TO UDET-TEANMNOT-REG(INDX)        
402800            MOVE TXT-TEANMNOT-ADM(INDX) TO UDET-TEANMNOT-ADM(INDX)        
402900            MOVE TXT-TEANMNOT-REM(INDX) TO UDET-TEANMNOT-REM(INDX)        
403000            MOVE TXT-TEANMNOT-RET(INDX) TO UDET-TEANMNOT-RET(INDX)        
403100                                                                          
403200            ADD +1                       TO INDX                          
403300           END-PERFORM                                                    
403400        END-IF                                                            
403500     END-IF                                                               
403600                                                                          
403700     .                                                                    
403800     EJECT                                                                
403900                                                                          
404000 IC-FIXA-FAKT-INFO              SECTION.                                  
404100                                                                          
404200     MOVE LEV-IDFAKT            TO W-IDFAKT-L5                            
404300     MOVE MSGI-IDDISTR          TO W-IDDISTR-L5                           
404400     MOVE MSGI-IDKUNDNR         TO W-IDKUNDNR-L5                          
404500     MOVE LEV-IDKUNDRF          TO W-IDKUNDRF-L5                          
404600     MOVE LEV-IDKOLLI           TO W-IDKOLLI-L5                           
404700     MOVE LEV-IDARTNR           TO W-IDARTNR-L5                           
404800                                                                          
404900     PERFORM IMS-GU-WDL501                                                
405000     IF SEGMENT-FINNS                                                     
405100        PERFORM IMS-GNP-WDL511                                            
405200        IF SEGMENT-FINNS                                                  
405300          MOVE FAKC-IDPRODNR TO W-IDPRODNR-L5                             
405400          PERFORM IMS-GNP-WDL521                                          
405500        END-IF                                                            
405600     END-IF                                                               
405700     IF SEGMENT-FINNS                                                     
405800        MOVE FAKL-KVBEART-Q     TO UDET-KVBEART-Q                         
405900        MOVE FAKL-KVLEVART      TO UDET-KVLEVART                          
406000                                                                          
406100        IF MSGI-KDMATT = 'U'                                              
406200          COMPUTE WS-VKORDBTO = FAKC-VKORDBTO-KOLLI *                     
406300                                CONV-KG-TO-LB                             
406400          MOVE WS-VKORDBTO      TO UDET-VKORDBTO-KOLLI                    
406500*                                  VKORDBTO-KOLLI                         
406600          COMPUTE WS-VKORDNTO = FAKC-VKORDNTO-KOLLI *                     
406700                                CONV-KG-TO-LB                             
406800          MOVE WS-VKORDNTO      TO UDET-VKORDNTO-KOLLI                    
406900        ELSE                                                              
407000          MOVE FAKC-VKORDBTO-KOLLI TO UDET-VKORDBTO-KOLLI                 
407100*                                    VKORDBTO-KOLLI                       
407200          MOVE FAKC-VKORDNTO-KOLLI TO UDET-VKORDNTO-KOLLI                 
407300        END-IF                                                            
407400                                                                          
407500        MOVE FAKC-KDORDKL       TO UDET-KDORDKL                           
407600        MOVE FAKC-KDKOLLI       TO W-KDKOLLI                              
407700                                                                          
407800        IF FAK-FLDIRLEV          = JA                                     
407900          IF MSGI-IDLAND-SPR = 'GB'                                       
408000            MOVE YES            TO UDET-FLDIRLEV                          
408100          ELSE                                                            
408200            MOVE FAK-FLDIRLEV   TO UDET-FLDIRLEV                          
408300          END-IF                                                          
408400        ELSE                                                              
408500          MOVE FAK-FLDIRLEV       TO UDET-FLDIRLEV                        
408600        END-IF                                                            
408700                                                                          
408800        MOVE FAKL-IDUSER-PACK   TO UDET-IDUSER-PACK                       
408900        MOVE FAKC-IDPRODNR      TO UDET-IDPRODNR                          
409000        MOVE FAKL-KVORDRAD      TO UDET-KVORDRAD                          
409100        MOVE FAKL-IDUSER-OREG   TO UDET-IDUSER-OREG                       
409200        MOVE FAKC-TIFAKT        TO UDET-TIREGDAT                          
409300        MOVE +0                 TO UDET-VKORDNTO-TOT                      
409400     ELSE                                                                 
409500        MOVE ZERO               TO UDET-KVBEART-Q                         
409600                                   UDET-KVLEVART                          
409700                                   UDET-VKORDBTO-KOLLI                    
409800                                   UDET-VKORDNTO-KOLLI                    
409900                                   UDET-VKORDNTO-TOT                      
410000                                   UDET-VKTARA                            
410100                                   UDET-KDORDKL                           
410200                                   UDET-KVORDRAD                          
410300                                   UDET-IDPRODNR                          
410400                                   UDET-TIREGDAT                          
410500        MOVE SPACE              TO UDET-FLDIRLEV                          
410600                                   UDET-IDUSER-PACK                       
410700                                   UDET-IDUSER-OREG                       
410800                                                                          
410900     END-IF                                                               
411000     PERFORM IMS-GET-EMBB01                                               
411100     IF SEGMENT-FINNS                                                     
411200       IF MSGI-KDMATT = 'U'                                               
411300         COMPUTE WS-VKTARA = EMB-VKTARA * CONV-KG-TO-LB                   
411400         END-COMPUTE                                                      
411500         MOVE WS-VKTARA           TO UDET-VKTARA                          
411600       ELSE                                                               
411700         MOVE EMB-VKTARA          TO UDET-VKTARA                          
411800       END-IF                                                             
411900     ELSE                                                                 
412000       MOVE +0                    TO UDET-VKTARA                          
412100     END-IF                                                               
412200     .                                                                    
412300     EJECT                                                                
412400                                                                          
412500 ID-FIXA-ART-INFO               SECTION.                                  
412600                                                                          
412700     MOVE +0                    TO WS-KVOKS-TOT-CDC                       
412800     MOVE MSGI-IDARTNR          TO W-IDARTNR                              
412900                                                                          
413000     PERFORM IMS-GET-ARTM-WDK9                                            
413100     IF SEGMENT-FINNS                                                     
413200       COMPUTE WS-KVOKS-TOT-CDC = ART-KVOKS-BULK +                        
413300                                  ART-KVOKS-DAG  +                        
413400                                  ART-KVOKS-VOR                           
413500     END-IF                                                               
413600                                                                          
413700     PERFORM IMS-GU-WLARTC01                                              
413800                                                                          
413900     MOVE ART-REKSIFFR          TO UDET-REKSIFFR                          
414000     MOVE ART-IDFKNGRP          TO UDET-IDFKNGRP                          
414100     MOVE ART-KDPRODSL          TO UDET-KDPRODSL                          
414200                                                                          
414300     MOVE LEV-IDDC    TO W-IDDC                                           
414400     PERFORM S20-READ-ARTC11-WDK712-22                                    
414500                                                                          
414600     COMPUTE W-KVPB-TOT       =  CLAG-KVPB-SEP +                          
414700                                 CLAG-KVPB-SATS +                         
414800                                 CLAG-KVPB-TPO                            
414900                                                                          
415000     MOVE W-KVPB-TOT            TO UDET-KVPB-TOT                          
415100     MOVE CLAG-KDERS            TO UDET-KDERS                             
415200     MOVE WS-VKART              TO UDET-VKART                             
415300     MOVE CLAG-PRARTBTO-EXP     TO UDET-PRARTBTO-EXP                      
415400     IF NDC-NA OR NDC-CN OR NDC-IN OR NDC-KR OR                           
415500        NDC-TR OR NDC-MY OR NDC-TH OR NDC-TW OR                           
415600        NDC-MX OR NDC-BR OR NDC-ZA                                        
415700        CONTINUE                                                          
415800     ELSE                                                                 
415900        MOVE WS-IDANSK          TO UDET-IDANSK                            
416000     END-IF                                                               
416100                                                                          
416200     IF CDC-SE OR GOOD-DDC                                                
416300       MOVE CLAG-ADLAGOMR       TO UDET-ADLAGOMR                          
416400       MOVE CLAG-ADGANG         TO UDET-ADGANG                            
416500       MOVE CLAG-ADPLATS        TO UDET-ADPLATS                           
416600       MOVE CLAG-PRINK          TO UDET-PRINK                             
416700       MOVE CLAG-TIINVDAT       TO UDET-TIINVDAT                          
416800       MOVE CLAG-KVINVS         TO UDET-KVINVS                            
416900       COMPUTE WS-KVDISP = CLAG-KVLS   -                                  
417000                           CLAG-KVRESS -                                  
417100                           CLAG-KVUTRS -                                  
417200                           WS-KVOKS-TOT-CDC                               
417300       MOVE WS-KVDISP           TO UDET-KVLS                              
417400     ELSE                                                                 
417500       IF DCS-SDC OR DCS-NDC-PF                                           
417600         MOVE CLAG-PRINK        TO UDET-PRINK                             
417700       END-IF                                                             
417800       PERFORM IMS-GU-WDK711                                              
417900         IF SEGMENT-FINNS                                                 
418000           MOVE SLAG-ADLAGOMR   TO UDET-ADLAGOMR                          
418100           MOVE SLAG-ADGANG     TO UDET-ADGANG                            
418200           MOVE SLAG-ADPLATS    TO UDET-ADPLATS                           
418300           MOVE SLAG-TIINVDAT   TO UDET-TIINVDAT                          
418400           MOVE SLAG-KVINVS     TO UDET-KVINVS                            
418500           COMPUTE WS-KVDISP     = SLAG-KVLS -                            
418600                                   SLAG-KVUTRS                            
418700           MOVE WS-KVDISP       TO UDET-KVLS                              
418800           IF NDC-NA OR NDC-CN OR NDC-IN OR NDC-KR OR                     
418900              NDC-TR OR NDC-MY OR NDC-TH OR NDC-TW OR                     
419000              NDC-MX OR NDC-BR OR NDC-ZA                                  
419100             MOVE SLAG-KVPB-REF TO UDET-KVPB-TOT                          
419200             MOVE SLAG-PRAVCOST TO UDET-PRINK                             
419300             MOVE SLAG-IDPERSON-BUY                                       
419400                                TO UDET-IDANSK                            
419500           END-IF                                                         
419600         ELSE                                                             
419700           MOVE +0              TO UDET-ADLAGOMR                          
419800                                   UDET-ADGANG                            
419900                                   UDET-ADPLATS                           
420000                                   UDET-TIINVDAT                          
420100                                   UDET-KVINVS                            
420200                                   UDET-KVLS                              
420300                                   UDET-KVPB-TOT                          
420400                                   UDET-PRINK                             
420500         END-IF                                                           
420600     END-IF                                                               
420700                                                                          
420800     PERFORM IMS-GU-WLBENA11                                              
420900     IF SEGMENT-FINNS                                                     
421000        MOVE TEXT-BEART         TO UDET-BEART                             
421100     ELSE                                                                 
421200        MOVE SPACE              TO UDET-BEART                             
421300     END-IF                                                               
421400                                                                          
421500     .                                                                    
421600     EJECT                                                                
421700 J-4721-HOPP                    SECTION.                                  
421800                                                                          
421900     MOVE MFS-KDMFSFOR       TO ALT-SPRAK                                 
422000     PERFORM IMS-INSERT-ALT-MSG                                           
422100     .                                                                    
422200     EJECT                                                                
422300     EJECT                                                                
422400 MFS-RENSA-FAELT-UT             SECTION.                                  
422500                                                                          
422600     MOVE MFS-RENSA-FAELT       TO MOD-KDKREBEH                           
422700                                   MOD-IDANSV                             
422800                                   MOD-FLSVAR                             
422900                                   MOD-FLRETUR                            
423000                                   MOD-IDPRT                              
423100                                   MOD-BEART                              
423200                                   MOD-KDANMORS                           
423300                                   MOD-IDARTNR                            
423400                                   MOD-REKSIFFR                           
423500                                   MOD-KVLEVANM-BEKR                      
423600                                   MOD-PRARTBTO                           
423700                                   MOD-PRARTBTO-LOC                       
423800                                   MOD-IDORDNR5                           
423900                                   MOD-IDKOLLI                            
424000                                   MOD-KDFAKTYP                           
424100                                   MOD-IDFAKT                             
424200                                   MOD-IDFAKT-LOC                         
424300                                   MOD-TIFAKT                             
424400                                   MOD-TIFAKT-LOC                         
424500                                   MOD-FLTEXT                             
424600                                   MOD-IDDC                               
424700                                   MOD-SULEVANM                           
424800                                   MOD-IDUSER-OREG                        
424900                                   MOD-TIREGDAT                           
425000                                                                          
425100                                   MOD-KVBEART-Q                          
425200                                   MOD-KVLEVART                           
425300                                   MOD-VKORDBTO-KOLLI                     
425400                                   MOD-VKORDBTO-DIFF                      
425500                                   MOD-VKTARA                             
425600                                   MOD-KDKOLLI                            
425700                                   MOD-KDORDKL                            
425800                                   MOD-FLDIRLEV                           
425900                                   MOD-IDUSER-PACK                        
426000                                   MOD-IDPRODNR                           
426100                                   MOD-KVORDRAD                           
426200                                   MOD-VLORDBTO-KOLLI                     
426300                                   MOD-KDFRAKT                            
426400                                   MOD-IDBORD                             
426500                                                                          
426600                                   MOD-ADLAGOMR                           
426700                                   MOD-ADGANG                             
426800                                   MOD-ADPLATS                            
426900                                   MOD-KVLS                               
427000                                   MOD-KVPB-TOT                           
427100                                   MOD-KDERS                              
427200                                   MOD-VKART                              
427300                                   MOD-PRARTBTO-EXP                       
427400                                   MOD-PRINK                              
427500                                   MOD-KDPRODSL                           
427600                                   MOD-TIINVDAT                           
427700                                   MOD-KVINVS                             
427800                                   MOD-IDANSK                             
427900                                   MOD-PRFRAKT                            
428000     .                                                                    
428100                                                                          
428200                                                                          
428300 MFS-ROER-EJ-FAELT-UT           SECTION.                                  
428400                                                                          
428500     MOVE MFS-ROER-EJ-FAELT     TO                                        
428600                                   MOD-BEART                              
428700                                   MOD-KDANMORS                           
428800                                   MOD-IDARTNR                            
428900                                   MOD-REKSIFFR                           
429000                                   MOD-KVLEVANM-BEKR                      
429100                                   MOD-PRARTBTO                           
429200                                   MOD-PRARTBTO-LOC                       
429300                                   MOD-IDORDNR5                           
429400                                   MOD-IDKOLLI                            
429500                                   MOD-KDFAKTYP                           
429600                                   MOD-IDFAKT                             
429700                                   MOD-IDFAKT-LOC                         
429800                                   MOD-TIFAKT                             
429900                                   MOD-TIFAKT-LOC                         
430000                                   MOD-FLTEXT                             
430100                                   MOD-IDDC                               
430200                                   MOD-SULEVANM                           
430300                                   MOD-IDUSER-OREG                        
430400                                   MOD-TIREGDAT                           
430500                                                                          
430600                                   MOD-KVBEART-Q                          
430700                                   MOD-KVLEVART                           
430800                                   MOD-VKORDBTO-KOLLI                     
430900                                   MOD-VKORDBTO-DIFF                      
431000                                   MOD-VKTARA                             
431100                                   MOD-KDKOLLI                            
431200                                   MOD-KDORDKL                            
431300                                   MOD-FLDIRLEV                           
431400                                   MOD-IDUSER-PACK                        
431500                                   MOD-IDPRODNR                           
431600                                   MOD-KVORDRAD                           
431700                                   MOD-VLORDBTO-KOLLI                     
431800                                   MOD-KDFRAKT                            
431900                                   MOD-IDBORD                             
432000                                                                          
432100                                   MOD-ADLAGOMR                           
432200                                   MOD-ADGANG                             
432300                                   MOD-ADPLATS                            
432400                                   MOD-KVLS                               
432500                                   MOD-KVPB-TOT                           
432600                                   MOD-KDERS                              
432700                                   MOD-VKART                              
432800                                   MOD-PRARTBTO-EXP                       
432900                                   MOD-PRINK                              
433000                                   MOD-KDPRODSL                           
433100                                   MOD-TIINVDAT                           
433200                                   MOD-KVINVS                             
433300                                   MOD-IDANSK                             
433400                                   MOD-PRFRAKT                            
433500     .                                                                    
433600                                                                          
433700 MFS-ROER-EJ-FAELT-IN           SECTION.                                  
433800                                                                          
433900     MOVE MFS-ROER-EJ-FAELT     TO MOD-KDKREBEH                           
434000                                   MOD-IDANSV                             
434100                                   MOD-FLSVAR                             
434200                                   MOD-FLRETUR                            
434300                                   MOD-IDPRT                              
434400                                   MOD-KDANMORS-UPPD                      
434500     .                                                                    
434600     EJECT                                                                
434700 MFS-STAENG-FAELT-IN            SECTION.                                  
434800                                                                          
434900     MOVE MFS-STAENG-FAELT      TO MOD-KDKREBEH-ATTR                      
435000                                   MOD-IDANSV-ATTR                        
435100                                   MOD-FLSVAR-ATTR                        
435200                                   MOD-FLRETUR-ATTR                       
435300                                   MOD-IDPRT-ATTR                         
435400                                   MOD-KDANMORS-UPPD-ATTR                 
435500     .                                                                    
435600     EJECT                                                                
435700 S01-LAS-ANALYSNRREGISTRET SECTION.                                       
435800     MOVE '*** S01-LAS-ANALYSNRREGISTRET *** '                            
435900                              TO FELTEXT                                  
436000                                                                          
436100     PERFORM IMS-GU-410901-ROT                                            
436200                                                                          
436300     PERFORM IMS-GNP-410911-KVAL                                          
436400     IF SEGMENT-SAKNAS                                                    
436500       MOVE NEJ              TO WL410901-SW                               
436600     ELSE                                                                 
436700       MOVE NEJ              TO WL410901-SW                               
436800       PERFORM UNTIL SEGMENT-SAKNAS OR WL410901-SW = JA                   
436900                                                                          
437000         IF WS-DATUM-Y2K >= 4110-DAGILTIG-FOM AND                         
437100                         <= 4110-DAGILTIG-TOM                             
437200            MOVE JA             TO WL410901-SW                            
437300            MOVE 4110-IDANALYS  TO WS-IDANALYS-UPPD                       
437400            MOVE 4110-IDKONTO   TO WS-IDKONTO-UPPD                        
437500            MOVE 4110-IDKST     TO WS-IDKST-UPPD                          
437600         ELSE                                                             
437700            PERFORM IMS-GNP-410911-KVAL                                   
437800         END-IF                                                           
437900       END-PERFORM                                                        
438000     END-IF                                                               
438100     .                                                                    
438200     EJECT                                                                
438300 S02-FELMEDDELANDE SECTION.                                               
438400                                                                          
438500     MOVE MFS-ALFA-FAELT-FEL TO MOD-FLSVAR-ATTR                           
438600     MOVE MFS-ROER-EJ-FAELT TO MOD-FLSVAR                                 
438700     MOVE NEJ           TO INDATA-SW                                      
438800     MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                            
438900     CALL WMEDKONV USING MED-WMEDAREA                                     
439000     MOVE MED-MFSFEL TO MOD-TEMFSFEL                                      
439100     .                                                                    
439200     EJECT                                                                
439300 S10-HAMTA-KDVALISO   SECTION.                                            
439400                                                                          
439500     PERFORM IMS-GU-WLKREE01                                              
439600     IF SEGMENT-FINNS                                                     
439700       MOVE MSGI-IDFTG           TO WS-IDFTG                              
439800       MOVE ANM-KDVALISO         TO MOD-KDVALISO                          
439900       IF (ANM-KDVALISO = SPACE) OR IDFTG-US OR IDFTG-CA OR               
440000                                    IDFTG-CN OR IDFTG-IN OR               
440100                                    IDFTG-KR OR IDFTG-TR OR               
440200                                    IDFTG-MY OR IDFTG-TH OR               
440300                                    IDFTG-TW OR IDFTG-MX OR               
440400                                    IDFTG-BR OR IDFTG-ZA                  
440500         PERFORM IMS-GU-GMTA-WDB201                                       
440600         IF SEGMENT-FINNS                                                 
440700           CONTINUE                                                       
440800         ELSE                                                             
440900           PERFORM IMS-GET-WDB201                                         
441000         END-IF                                                           
441100         MOVE GMT-IDPARTNR       TO W-WDB1-IDPARTNR                       
441200         MOVE GMT-IDFTG          TO W-WDB1-IDFTG                          
441300         PERFORM IMS-GU-WDB1-WDB101                                       
441400         IF SEGMENT-FINNS                                                 
441500           IF DIST79-DEALER-PRICE OR                                      
441600                                     IDFTG-US OR IDFTG-CA OR              
441700                                     IDFTG-CN OR IDFTG-IN OR              
441800                                     IDFTG-KR OR IDFTG-TR OR              
441900                                     IDFTG-MY OR IDFTG-TH OR              
442000                                     IDFTG-TW OR IDFTG-MX OR              
442100                                     IDFTG-BR OR IDFTG-ZA                 
442200                                                                          
442300**-- HÅRDKODA ENLIGT BOSSE H EFTERSOM BET-KDVALISO = SEK (5131)           
442400**-- FÖR KINA OCH VIPS FAKTURERAR I CNY.                                  
442500**-- FÖR INDIA OCH VIPS FAKTURERAR I INR.                                 
442600**-- FÖR KOREA OCH VIPS FAKTURERAR I KRW.                                 
442700             IF IDFTG-CN                                                  
442800               MOVE 'CNY'          TO MOD-KDVALISO                        
442900             ELSE                                                         
443000               IF IDFTG-IN                                                
443100                 MOVE 'INR'        TO MOD-KDVALISO                        
443200               ELSE                                                       
443300                 IF IDFTG-KR                                              
443400                   MOVE 'KRW'      TO MOD-KDVALISO                        
443500                 ELSE                                                     
443600                   IF IDFTG-TR                                            
443700                     MOVE 'TRY'    TO MOD-KDVALISO                        
443800                   ELSE                                                   
443900                     IF IDFTG-MY                                          
444000                       MOVE 'MYR'    TO MOD-KDVALISO                      
444100                     ELSE                                                 
444200                       IF IDFTG-MX                                        
444300                         MOVE 'MXN'    TO MOD-KDVALISO                    
444400                       ELSE                                               
444500                         IF IDFTG-BR                                      
444600                           MOVE 'BRL'    TO MOD-KDVALISO                  
444700                         ELSE                                             
444710                           IF IDFTG-ZA                                    
444711                             MOVE 'ZAR'  TO MOD-KDVALISO                  
444720                           ELSE                                           
444800                             MOVE BET-KDVALISO TO MOD-KDVALISO            
444810                           END-IF                                         
444900                         END-IF                                           
445000                       END-IF                                             
445100                     END-IF                                               
445200                   END-IF                                                 
445300                 END-IF                                                   
445400               END-IF                                                     
445500             END-IF                                                       
445600           ELSE                                                           
445700             MOVE 'SEK'            TO MOD-KDVALISO                        
445800           END-IF                                                         
445900         ELSE                                                             
446000           MOVE SPACE            TO MOD-KDVALISO                          
446100         END-IF                                                           
446200       END-IF                                                             
446300     ELSE                                                                 
446400       MOVE SPACE                TO MOD-KDVALISO                          
446500     END-IF                                                               
446600     .                                                                    
446700     EJECT                                                                
446800 S11-INSERT-WDGX4104 SECTION.                                             
446900                                                                          
447000     MOVE LEV-IDDC          TO 4104-IDDC                                  
447100     MOVE W-KDKRENOT-4104   TO 4104-KDKRENOT                              
447200     MOVE NEJ               TO 4104-FLKREATT                              
447300     MOVE WS-KDVALISO-SPAR  TO 4104-KDVALISO                              
447400     MOVE SPACE             TO 4104-FILLER                                
447500                                                                          
447600     IF DIST79-DEALER-PRICE OR                                            
447700        DIST79-ECOM-PRICE                                                 
447800       COMPUTE WS-RADPRIS ROUNDED =                                       
447900                      LEV-KVLEVANM-BEKR * LEV-PRARTBTO-LOC                
448000     ELSE                                                                 
448100*                                                                         
448200*CHINA-PRICE5                                                             
448300*INDIA-PRICE5                                                             
448400*KOREA-PRICE5                                                             
448500       PERFORM S22-SET-KDVALISO-AND-RADPRIS                               
448600     END-IF                                                               
448700                                                                          
448800     MOVE WS-RADPRIS        TO 4104-SUKRENOT                              
448900                                                                          
449000     PERFORM IMS-ISRT-WDGX4104                                            
449100                                                                          
449200     .                                                                    
449300     EJECT                                                                
449400 S13-KOLLA-RETUR-DC    SECTION.                                           
449500                                                                          
449600     MOVE NEJ                       TO  GODK-KOD-SW                       
449700     MOVE JA                        TO  GODK-ARTIKEL-SW                   
449800                                        GODK-IDFKNGRP-SW                  
449900                                        GODK-DC-ARTIKEL-SW                
450000                                        GODK-DC-LEV-SW                    
450100                                                                          
450200     MOVE GMT-IDDC-RET72(1)   TO W-IDDC-B6                                
450300                                 W-IDDC                                   
450400     PERFORM IMS-GU-WDB601                                                
450500     IF SEGMENT-SAKNAS                                                    
450600       IF DIST34-KINA-NDC  OR DIST34-INDIA-NDC OR                         
450700          DIST34-KOREA-NDC OR DIST34-TURKEY-NDC OR                        
450800          DIST34-MEXICO-NDC OR                                            
450900          DIST34-BRAZIL-NDC OR                                            
451000          DIST34-MALAYSIA-NDC OR DIST34-THAILAND-NDC OR                   
451100          DIST34-TAIWAN-NDC   OR                                          
451110          DIST34-SOUTH-AFRICA-NDC                                         
451200         MOVE GMT-IDDC-RET      TO LEV-IDDC-RET                           
451300                                   WS-ANM-IDDC-RET                        
451400       ELSE                                                               
451500         MOVE WS-CDC-SE         TO LEV-IDDC-RET                           
451600                                   WS-ANM-IDDC-RET                        
451700       END-IF                                                             
451800       MOVE 3                 TO WS-ANM-IXDCCLEAR                         
451900     ELSE                                                                 
452000       IF DCS-FLARTDC = JA                                                
452100*- KOLLA OM ARTIKELN FINNS PÅ DC'T. KRAV FÖR ATT TA EMOT RETUR.           
452200         MOVE LEV-IDARTNR     TO W-IDARTNR                                
452300         PERFORM IMS-GU-WDK711                                            
452400         IF SEGMENT-SAKNAS                                                
452500           MOVE NEJ  TO  GODK-DC-ARTIKEL-SW                               
452600         END-IF                                                           
452700       END-IF                                                             
452800                                                                          
452900       IF EJ-GODK-DC-ARTIKEL                                              
453000         CONTINUE                                                         
453100       ELSE                                                               
453200         MOVE SPACE           TO W-URV-TEELMT                             
453300         MOVE WC-IDARTNR      TO W-URV-TEELMT                             
453400         MOVE SPACE           TO W-URV-FILLER                             
453500         MOVE LEV-IDARTNR     TO W-URV-IDARTNR-EXCP                       
453600                                                                          
453700         PERFORM IMS-GNP-WDB611-FIRST                                     
453800         IF SEGMENT-FINNS                                                 
453900           MOVE NEJ           TO GODK-ARTIKEL-SW                          
454000         ELSE                                                             
454100           MOVE LEV-IDARTNR   TO W-IDARTNR                                
454200           PERFORM IMS-GU-WLARTC01                                        
454300           IF SEGMENT-SAKNAS                                              
454400             MOVE NEJ  TO  GODK-DC-ARTIKEL-SW                             
454500           ELSE                                                           
454600             MOVE SPACE           TO W-URV-TEELMT                         
454700             MOVE WC-IDFKNGRP     TO W-URV-TEELMT                         
454800             MOVE SPACE           TO W-URV-FILLER                         
454900             MOVE ART-IDFKNGRP    TO W-URV-IDFKNGRP-EXCP                  
455000                                                                          
455100             PERFORM IMS-GNP-WDB611-FIRST                                 
455200             IF SEGMENT-FINNS                                             
455300               MOVE NEJ           TO GODK-IDFKNGRP-SW                     
455400             ELSE                                                         
455500               MOVE SPACE           TO W-URV-TEELMT                       
455600               MOVE WC-IDDC-EXCP    TO W-URV-TEELMT                       
455700               MOVE SPACE           TO W-URV-FILLER                       
455800               MOVE LEV-IDDC        TO W-URV-IDDC-EXCP                    
455900                                                                          
456000               PERFORM IMS-GNP-WDB611-FIRST                               
456100               IF SEGMENT-FINNS                                           
456200                 MOVE NEJ           TO GODK-DC-LEV-SW                     
456300               ELSE                                                       
456400                 MOVE SPACE           TO W-URV-TEELMT                     
456500                 MOVE WC-KDANMORS     TO W-URV-TEELMT                     
456600                 MOVE SPACE           TO W-URV-FILLER                     
456700                 MOVE LEV-KDANMORS    TO W-URV-KDANMORS-RET               
456800                                                                          
456900                 PERFORM IMS-GNP-WDB611-FIRST                             
457000                 IF SEGMENT-FINNS                                         
457100                   MOVE JA            TO GODK-KOD-SW                      
457200                 END-IF                                                   
457300               END-IF                                                     
457400             END-IF                                                       
457500           END-IF                                                         
457600         END-IF                                                           
457700       END-IF                                                             
457800                                                                          
457900       IF EJ-GODK-DC-ARTIKEL OR                                           
458000          EJ-GODK-ARTIKEL OR                                              
458100          EJ-GODK-IDFKNGRP OR                                             
458200          EJ-GODK-DC-LEV   OR                                             
458300          EJ-GODK-KOD                                                     
458400                                                                          
458500          IF GMT-IDDC-RET72(2) = SPACE                                    
458600            MOVE GMT-IDDC-RET72(3)  TO LEV-IDDC-RET                       
458700                                       WS-ANM-IDDC-RET                    
458800            MOVE 3                  TO WS-ANM-IXDCCLEAR                   
458900            IF LEV-IDDC-RET  = SPACE                                      
459000              IF DIST34-KINA-NDC  OR DIST34-INDIA-NDC OR                  
459100                 DIST34-KOREA-NDC OR DIST34-TURKEY-NDC OR                 
459200                 DIST34-MEXICO-NDC OR                                     
459300                 DIST34-BRAZIL-NDC OR                                     
459400                 DIST34-MALAYSIA-NDC OR DIST34-THAILAND-NDC OR            
459500                 DIST34-TAIWAN-NDC   OR                                   
459510                 DIST34-SOUTH-AFRICA-NDC                                  
459600                MOVE GMT-IDDC-RET     TO LEV-IDDC-RET                     
459700                                         WS-ANM-IDDC-RET                  
459800              ELSE                                                        
459900                MOVE WS-CDC-SE        TO LEV-IDDC-RET                     
460000                                         WS-ANM-IDDC-RET                  
460100              END-IF                                                      
460200              MOVE 3                TO WS-ANM-IXDCCLEAR                   
460300            END-IF                                                        
460400          ELSE                                                            
460500            PERFORM S14-KOLLA-RETUR-DC-2                                  
460600          END-IF                                                          
460700       ELSE                                                               
460800         MOVE GMT-IDDC-RET72(1)     TO LEV-IDDC-RET                       
460900                                       WS-ANM-IDDC-RET                    
461000         MOVE 1                     TO WS-ANM-IXDCCLEAR                   
461100       END-IF                                                             
461200     END-IF                                                               
461300     .                                                                    
461400     EJECT                                                                
461500 S14-KOLLA-RETUR-DC-2  SECTION.                                           
461600                                                                          
461700     MOVE NEJ                       TO  GODK-KOD-SW                       
461800     MOVE JA                        TO  GODK-ARTIKEL-SW                   
461900                                        GODK-IDFKNGRP-SW                  
462000                                        GODK-DC-ARTIKEL-SW                
462100                                        GODK-DC-LEV-SW                    
462200                                                                          
462300     MOVE GMT-IDDC-RET72(2)   TO W-IDDC-B6                                
462400                                 W-IDDC                                   
462500     PERFORM IMS-GU-WDB601                                                
462600     IF SEGMENT-SAKNAS                                                    
462700       IF DIST34-KINA-NDC  OR DIST34-INDIA-NDC OR                         
462800          DIST34-KOREA-NDC OR DIST34-TURKEY-NDC OR                        
462900          DIST34-MEXICO-NDC OR                                            
463000          DIST34-BRAZIL-NDC OR                                            
463100          DIST34-MALAYSIA-NDC OR DIST34-THAILAND-NDC OR                   
463200          DIST34-TAIWAN-NDC   OR                                          
463210          DIST34-SOUTH-AFRICA-NDC                                         
463300         MOVE GMT-IDDC-RET      TO LEV-IDDC-RET                           
463400                                   WS-ANM-IDDC-RET                        
463500       ELSE                                                               
463600         MOVE WS-CDC-SE         TO LEV-IDDC-RET                           
463700                                   WS-ANM-IDDC-RET                        
463800       END-IF                                                             
463900       MOVE 3                 TO WS-ANM-IXDCCLEAR                         
464000     ELSE                                                                 
464100       IF DCS-FLARTDC = JA                                                
464200*- KOLLA OM ARTIKELN FINNS PÅ DC'T. KRAV FÖR ATT TA EMOT RETUR.           
464300         MOVE LEV-IDARTNR  TO W-IDARTNR                                   
464400         PERFORM IMS-GU-WDK711                                            
464500         IF SEGMENT-SAKNAS                                                
464600           MOVE NEJ  TO  GODK-DC-ARTIKEL-SW                               
464700         END-IF                                                           
464800       END-IF                                                             
464900                                                                          
465000       IF EJ-GODK-DC-ARTIKEL                                              
465100         CONTINUE                                                         
465200       ELSE                                                               
465300         MOVE SPACE           TO W-URV-TEELMT                             
465400         MOVE WC-IDARTNR      TO W-URV-TEELMT                             
465500         MOVE SPACE           TO W-URV-FILLER                             
465600         MOVE LEV-IDARTNR     TO W-URV-IDARTNR-EXCP                       
465700                                                                          
465800         PERFORM IMS-GNP-WDB611-FIRST                                     
465900         IF SEGMENT-FINNS                                                 
466000           MOVE NEJ           TO GODK-ARTIKEL-SW                          
466100         ELSE                                                             
466200           MOVE LEV-IDARTNR  TO W-IDARTNR                                 
466300           PERFORM IMS-GU-WLARTC01                                        
466400           IF SEGMENT-SAKNAS                                              
466500             MOVE NEJ  TO  GODK-DC-ARTIKEL-SW                             
466600           ELSE                                                           
466700             MOVE SPACE            TO W-URV-TEELMT                        
466800             MOVE WC-IDFKNGRP      TO W-URV-TEELMT                        
466900             MOVE SPACE            TO W-URV-FILLER                        
467000             MOVE ART-IDFKNGRP     TO W-URV-IDFKNGRP-EXCP                 
467100                                                                          
467200             PERFORM IMS-GNP-WDB611-FIRST                                 
467300             IF SEGMENT-FINNS                                             
467400               MOVE NEJ            TO GODK-IDFKNGRP-SW                    
467500             ELSE                                                         
467600               MOVE SPACE            TO W-URV-TEELMT                      
467700               MOVE WC-IDDC-EXCP     TO W-URV-TEELMT                      
467800               MOVE SPACE            TO W-URV-FILLER                      
467900               MOVE LEV-IDDC         TO W-URV-IDDC-EXCP                   
468000                                                                          
468100               PERFORM IMS-GNP-WDB611-FIRST                               
468200               IF SEGMENT-FINNS                                           
468300                 MOVE NEJ            TO GODK-DC-LEV-SW                    
468400               ELSE                                                       
468500                 MOVE SPACE          TO W-URV-TEELMT                      
468600                 MOVE WC-KDANMORS    TO W-URV-TEELMT                      
468700                 MOVE SPACE          TO W-URV-FILLER                      
468800                 MOVE LEV-KDANMORS   TO W-URV-KDANMORS-RET                
468900                                                                          
469000                 PERFORM IMS-GNP-WDB611-FIRST                             
469100                 IF SEGMENT-FINNS                                         
469200                   MOVE JA           TO GODK-KOD-SW                       
469300                 END-IF                                                   
469400               END-IF                                                     
469500             END-IF                                                       
469600           END-IF                                                         
469700         END-IF                                                           
469800       END-IF                                                             
469900                                                                          
470000       IF EJ-GODK-DC-ARTIKEL OR                                           
470100          EJ-GODK-ARTIKEL OR                                              
470200          EJ-GODK-IDFKNGRP OR                                             
470300          EJ-GODK-DC-LEV OR                                               
470400          EJ-GODK-KOD                                                     
470500                                                                          
470600          MOVE GMT-IDDC-RET72(3)   TO LEV-IDDC-RET                        
470700                                      WS-ANM-IDDC-RET                     
470800          MOVE 3                   TO WS-ANM-IXDCCLEAR                    
470900          IF LEV-IDDC-RET  = SPACE                                        
471000            IF DIST34-KINA-NDC  OR DIST34-INDIA-NDC OR                    
471100               DIST34-KOREA-NDC OR DIST34-TURKEY-NDC OR                   
471200               DIST34-MEXICO-NDC OR                                       
471300               DIST34-BRAZIL-NDC OR                                       
471400               DIST34-MALAYSIA-NDC OR DIST34-THAILAND-NDC OR              
471500               DIST34-TAIWAN-NDC   OR                                     
471510               DIST34-SOUTH-AFRICA-NDC                                    
471600              MOVE GMT-IDDC-RET      TO LEV-IDDC-RET                      
471700                                        WS-ANM-IDDC-RET                   
471800            ELSE                                                          
471900              MOVE WS-CDC-SE         TO LEV-IDDC-RET                      
472000                                        WS-ANM-IDDC-RET                   
472100            END-IF                                                        
472200            MOVE 3                 TO WS-ANM-IXDCCLEAR                    
472300          END-IF                                                          
472400       ELSE                                                               
472500         MOVE GMT-IDDC-RET72(2)    TO LEV-IDDC-RET                        
472600                                      WS-ANM-IDDC-RET                     
472700         MOVE 2                    TO WS-ANM-IXDCCLEAR                    
472800       END-IF                                                             
472900     END-IF                                                               
473000     .                                                                    
473100     EJECT                                                                
473200 S20-READ-ARTC11-WDK712-22  SECTION.                                      
473300                                                                          
473400     MOVE ZERO TO WS-IDANSK                                               
473500                  WS-VKART                                                
473600                  WS-VLARTNTO                                             
473700                                                                          
473800     PERFORM IMS-GNP-WLARTC11                                             
473900     IF SEGMENT-FINNS                                                     
474000        MOVE CLAG-IDANSK    TO WS-IDANSK                                  
474100        MOVE CLAG-VKART     TO WS-VKART                                   
474200        MOVE CLAG-VLARTNTO  TO WS-VLARTNTO                                
474300     END-IF                                                               
474400                                                                          
474500     PERFORM IMS-GU-WDK722                                                
474600     IF SEGMENT-FINNS                                                     
474700        MOVE XLAG-IDANSK    TO WS-IDANSK                                  
474800     END-IF                                                               
474900                                                                          
475000     SEARCH ALL DC-LAND                                                   
475100        AT END                                                            
475200           MOVE SPACE          TO W-IDLAND                                
475300        WHEN DCLAND-IDDC (DCLAND-IX) = W-IDDC                             
475400           MOVE DCLAND-IDLANDX2 (DCLAND-IX)                               
475500                               TO W-IDLAND                                
475600     END-SEARCH                                                           
475700     PERFORM IMS-GU-WDK712                                                
475800     IF SEGMENT-FINNS                                                     
475900        MOVE LART-VKART     TO WS-VKART                                   
476000        MOVE LART-VLARTNTO  TO WS-VLARTNTO                                
476100     END-IF                                                               
476200                                                                          
476300     .                                                                    
476400     EJECT                                                                
476500                                                                          
476600 S22-SET-KDVALISO-AND-RADPRIS    SECTION.                                 
476700                                                                          
476800     MOVE LEV-IDFTG            TO WS-IDFTG                                
476900     EVALUATE TRUE                                                        
477000     WHEN IDFTG-CN                                                        
477100         MOVE 'CNY'            TO 4104-KDVALISO                           
477200         COMPUTE WS-RADPRIS ROUNDED =                                     
477300                 LEV-KVLEVANM-BEKR * LEV-PRARTBTO-LOCINV                  
477400     WHEN IDFTG-IN                                                        
477500         MOVE 'INR'            TO 4104-KDVALISO                           
477600         COMPUTE WS-RADPRIS ROUNDED =                                     
477700                 LEV-KVLEVANM-BEKR * LEV-PRARTBTO-LOCINV                  
477800     WHEN IDFTG-KR                                                        
477900         MOVE 'KRW'            TO 4104-KDVALISO                           
478000         COMPUTE WS-RADPRIS ROUNDED =                                     
478100                 LEV-KVLEVANM-BEKR * LEV-PRARTBTO-LOCINV                  
478200     WHEN IDFTG-TR                                                        
478300         MOVE 'TRY'              TO 4104-KDVALISO                         
478400         COMPUTE WS-RADPRIS ROUNDED =                                     
478500                 LEV-KVLEVANM-BEKR * LEV-PRARTBTO-LOCINV                  
478600     WHEN IDFTG-MX                                                        
478700         MOVE 'MXN'              TO 4104-KDVALISO                         
478800         COMPUTE WS-RADPRIS ROUNDED =                                     
478900                 LEV-KVLEVANM-BEKR * LEV-PRARTBTO-LOCINV                  
479000     WHEN IDFTG-BR                                                        
479100         MOVE 'BRL'              TO 4104-KDVALISO                         
479200         COMPUTE WS-RADPRIS ROUNDED =                                     
479300                 LEV-KVLEVANM-BEKR * LEV-PRARTBTO-LOCINV                  
479400     WHEN IDFTG-MY                                                        
479500         MOVE 'MYR'                TO 4104-KDVALISO                       
479600         COMPUTE WS-RADPRIS ROUNDED =                                     
479700                 LEV-KVLEVANM-BEKR * LEV-PRARTBTO-LOCINV                  
479800     WHEN IDFTG-TH                                                        
479900         MOVE 'THB'                TO 4104-KDVALISO                       
480000         COMPUTE WS-RADPRIS ROUNDED =                                     
480100                 LEV-KVLEVANM-BEKR * LEV-PRARTBTO-LOCINV                  
480200     WHEN IDFTG-TW                                                        
480300         MOVE 'TWD'                TO 4104-KDVALISO                       
480400         COMPUTE WS-RADPRIS ROUNDED =                                     
480500                 LEV-KVLEVANM-BEKR * LEV-PRARTBTO-LOCINV                  
480510     WHEN IDFTG-ZA                                                        
480520         MOVE 'ZAR'                TO 4104-KDVALISO                       
480530         COMPUTE WS-RADPRIS ROUNDED =                                     
480540                 LEV-KVLEVANM-BEKR * LEV-PRARTBTO-LOCINV                  
480600     WHEN OTHER                                                           
480700         COMPUTE WS-RADPRIS ROUNDED =                                     
480800                 LEV-KVLEVANM-BEKR * LEV-PRARTBTO                         
480900     END-EVALUATE                                                         
481000                                                                          
481100     .                                                                    
481200     EJECT                                                                
481300                                                                          
481400* --- IMS SEKTIONER ---                                                   
481500                                                                          
481600 IMS-GET-MSG                    SECTION.                                  
481700                                                                          
481800     MOVE '  QC' TO GODK-STATUSKODER                                      
481900     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
482000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
482100     PERFORM IMS-STATUSKONTROLL                                           
482200     .                                                                    
482300                                                                          
482400 IMS-INSERT-MSG                 SECTION.                                  
482500                                                                          
482600     IF MSGI-IDLAND-SPR = 'GB'                                            
482700       MOVE 'N' TO MFS-KDHUVOMR                                           
482800     END-IF                                                               
482900     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
483000     MOVE SPACE TO GODK-STATUSKODER                                       
483100     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
483200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
483300     PERFORM IMS-STATUSKONTROLL                                           
483400     .                                                                    
483500     EJECT                                                                
483600                                                                          
483700 IMS-INSERT-ALT-MSG SECTION.                                              
483800                                                                          
483900     IF MSGI-IDLAND-SPR = 'GB'                                            
484000       MOVE 'N' TO MFS-KDHUVOMR                                           
484100     END-IF                                                               
484200     MOVE LOW-VALUE TO ALT-Z1 ALT-Z2                                      
484300     MOVE SPACE TO GODK-STATUSKODER                                       
484400     CALL CBLTDLI USING ISRT 4721-PCB ALT-MSG-IO-AREA                     
484500     MOVE 4721-STATUS-CODE TO STATUS-WS                                   
484600     PERFORM IMS-STATUSKONTROLL                                           
484700     EJECT                                                                
484800     .                                                                    
484900 IMS-GU-WLKREE01               SECTION.                                   
485000                                                                          
485100     STRING 'WLKREE01(IDLEVANM =' W-IDLEVANM-X ')'                        
485200          DELIMITED BY SIZE INTO SSA1                                     
485300     MOVE '  GE'           TO GODK-STATUSKODER                            
485400     CALL CBLTDLI USING GU KREE-PCB DLI-IO-AREA SSA1                      
485500     MOVE KREE-STATUS-CODE TO STATUS-WS                                   
485600     PERFORM IMS-STATUSKONTROLL                                           
485700     .                                                                    
485800                                                                          
485900 IMS-GET-WLKREE01-KVAL          SECTION.                                  
486000                                                                          
486100     STRING 'WLKREE01(IDLEVANM =' W-IDLEVANM-X ')'                        
486200          DELIMITED BY SIZE INTO SSA1                                     
486300     MOVE '  GE'           TO GODK-STATUSKODER                            
486400     CALL CBLTDLI USING GHU KREE-PCB DLI-IO-AREA SSA1                     
486500     MOVE KREE-STATUS-CODE TO STATUS-WS                                   
486600     PERFORM IMS-STATUSKONTROLL                                           
486700     .                                                                    
486800                                                                          
486900 IMS-GET-KREE11-GNP             SECTION.                                  
487000                                                                          
487100     MOVE 'WLKREE11 ' TO SSA1                                             
487200     MOVE '  GE' TO GODK-STATUSKODER                                      
487300     CALL CBLTDLI USING GNP KREE-PCB DLI-IO-AREA SSA1                     
487400     MOVE KREE-STATUS-CODE TO STATUS-WS                                   
487500     PERFORM IMS-STATUSKONTROLL                                           
487600     .                                                                    
487700                                                                          
487800 IMS-GHU-WLKREE11               SECTION.                                  
487900                                                                          
488000     STRING 'WLKREE01(IDLEVANM =' W-IDLEVANM-X ')'                        
488100          DELIMITED BY SIZE INTO SSA1                                     
488200     STRING 'WLKREE11(WDA211KY =' W-WDA211KY-X ')'                        
488300          DELIMITED BY SIZE INTO SSA2                                     
488400     MOVE '  GE'           TO GODK-STATUSKODER                            
488500     CALL CBLTDLI USING GHU KREE-PCB DLI-IO-AREA SSA1 SSA2                
488600     MOVE KREE-STATUS-CODE TO STATUS-WS                                   
488700     PERFORM IMS-STATUSKONTROLL                                           
488800     .                                                                    
488900                                                                          
489000 IMS-GU-WLKREE11-KVAL          SECTION.                                   
489100                                                                          
489200     STRING 'WLKREE01(IDLEVANM =' W-IDLEVANM-X ')'                        
489300          DELIMITED BY SIZE INTO SSA1                                     
489400     STRING 'WLKREE11(WDA211KY =' W-WDA211KY-X ')'                        
489500          DELIMITED BY SIZE INTO SSA2                                     
489600     MOVE '  GE'           TO GODK-STATUSKODER                            
489700     CALL CBLTDLI USING GU KREE-PCB DLI-IO-AREA SSA1 SSA2                 
489800     MOVE KREE-STATUS-CODE TO STATUS-WS                                   
489900     PERFORM IMS-STATUSKONTROLL                                           
490000     .                                                                    
490100     EJECT                                                                
490200 IMS-REPL-KREE                  SECTION.                                  
490300                                                                          
490400     MOVE '    '           TO GODK-STATUSKODER                            
490500     CALL CBLTDLI USING REPL KREE-PCB DLI-IO-AREA                         
490600     MOVE KREE-STATUS-CODE TO STATUS-WS                                   
490700     PERFORM IMS-STATUSKONTROLL                                           
490800     .                                                                    
490900     EJECT                                                                
491000                                                                          
491100                                                                          
491200 IMS-GNP-WLKREE21               SECTION.                                  
491300                                                                          
491400     STRING 'WLKREE01(IDLEVANM =' W-IDLEVANM-X ')'                        
491500            DELIMITED BY SIZE INTO SSA1                                   
491600     STRING 'WLKREE11(WDA211KY =' W-WDA211KY-X ')'                        
491700            DELIMITED BY SIZE INTO SSA2                                   
491800     MOVE 'WLKREE21 ' TO SSA3                                             
491900     MOVE '  GE' TO GODK-STATUSKODER                                      
492000     CALL CBLTDLI USING GNP KREE-PCB DLI-IO-AREA2 SSA1 SSA2 SSA3          
492100     MOVE KREE-STATUS-CODE TO STATUS-WS                                   
492200     PERFORM IMS-STATUSKONTROLL                                           
492300     .                                                                    
492400     EJECT                                                                
492500 IMS-GN-WLKREI-SEQ              SECTION.                                  
492600                                                                          
492700     STRING 'WLKREI01(WDA2D1KY>=' W-WDA2DSEQ-MIN-X                        
492800                    '&WDA2D1KY<=' W-WDA2DSEQ-MAX-X ')'                    
492900            DELIMITED BY SIZE INTO SSA1                                   
493000     MOVE '  GEGB' TO GODK-STATUSKODER                                    
493100     CALL CBLTDLI USING GN KREI-PCB DLI-IO-AREA2 SSA1                     
493200     MOVE KREI-STATUS-CODE TO STATUS-WS                                   
493300     PERFORM IMS-STATUSKONTROLL                                           
493400     .                                                                    
493500     EJECT                                                                
493600 IMS-GU-WLARTC01                SECTION.                                  
493700                                                                          
493800     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
493900          DELIMITED BY SIZE INTO SSA1                                     
494000     MOVE '  GE'           TO GODK-STATUSKODER                            
494100     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA2 SSA1                     
494200     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
494300     PERFORM IMS-STATUSKONTROLL                                           
494400     .                                                                    
494500                                                                          
494600 IMS-GNP-WLARTC11               SECTION.                                  
494700                                                                          
494800     MOVE 'WLARTC11'       TO SSA1                                        
494900     MOVE '  GE'           TO GODK-STATUSKODER                            
495000     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA2 SSA1                    
495100     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
495200     PERFORM IMS-STATUSKONTROLL                                           
495300     .                                                                    
495400     EJECT                                                                
495500 IMS-GET-ARTM-WDK9              SECTION.                                  
495600                                                                          
495700     STRING 'WLARTM01(IDARTNR  =' W-IDARTNR-X ')'                         
495800     DELIMITED BY SIZE INTO SSA1                                          
495900     MOVE '  GE' TO GODK-STATUSKODER                                      
496000     CALL CBLTDLI USING GU ARTM-PCB DLI-IO-AREA2 SSA1                     
496100     MOVE ARTM-STATUS-CODE TO STATUS-WS                                   
496200     PERFORM IMS-STATUSKONTROLL                                           
496300     .                                                                    
496400     EJECT                                                                
496500 IMS-GU-WDK711                 SECTION.                                   
496600                                                                          
496700     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
496800     DELIMITED BY SIZE INTO SSA1                                          
496900     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
497000     DELIMITED BY SIZE INTO SSA2                                          
497100     MOVE '  GE' TO GODK-STATUSKODER                                      
497200     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-AREA-K711 SSA1 SSA2            
497300     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
497400     PERFORM IMS-STATUSKONTROLL                                           
497500     .                                                                    
497600     EJECT                                                                
497700 IMS-GU-WDK712                 SECTION.                                   
497800                                                                          
497900     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
498000     DELIMITED BY SIZE INTO SSA1                                          
498100     STRING 'WDK712  (IDLAND   =' W-IDLAND-X ')'                          
498200     DELIMITED BY SIZE INTO SSA2                                          
498300     MOVE '  GE' TO GODK-STATUSKODER                                      
498400     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-AREA-K712 SSA1 SSA2            
498500     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
498600     PERFORM IMS-STATUSKONTROLL                                           
498700     .                                                                    
498800     EJECT                                                                
498900 IMS-GU-WDK722                 SECTION.                                   
499000                                                                          
499100     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
499200     DELIMITED BY SIZE INTO SSA1                                          
499300     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
499400     DELIMITED BY SIZE INTO SSA2                                          
499500     STRING 'WDK722  (KDSEGKEY =1)'                                       
499600     DELIMITED BY SIZE INTO SSA3                                          
499700     MOVE '  GE' TO GODK-STATUSKODER                                      
499800     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-AREA-K722                      
499900                                    SSA1 SSA2 SSA3                        
500000     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
500100     PERFORM IMS-STATUSKONTROLL                                           
500200     .                                                                    
500300     EJECT                                                                
500400                                                                          
500500 IMS-GU-WDL501                  SECTION.                                  
500600                                                                          
500700     STRING 'WDL501  (IDFAKT   =' W-IDFAKT-X ')'                          
500800          DELIMITED BY SIZE INTO SSA1                                     
500900     MOVE '  GE'           TO GODK-STATUSKODER                            
501000     CALL CBLTDLI USING GU WDL5-PCB DLI-IO-WDL501 SSA1                    
501100     MOVE WDL5-STATUS-CODE TO STATUS-WS                                   
501200     PERFORM IMS-STATUSKONTROLL                                           
501300     .                                                                    
501400     EJECT                                                                
501500 IMS-GNP-WDL511                 SECTION.                                  
501600                                                                          
501700     STRING 'WDL511  (IDGMTREF =' W-IDGMTREF-X                            
501800                    '&IDKOLLI  =' W-IDKOLLI-L5-X ')'                      
501900          DELIMITED BY SIZE INTO SSA1                                     
502000     MOVE '  GE'           TO GODK-STATUSKODER                            
502100     CALL CBLTDLI USING GNP WDL5-PCB DLI-IO-WDL511 SSA1                   
502200     MOVE WDL5-STATUS-CODE TO STATUS-WS                                   
502300     PERFORM IMS-STATUSKONTROLL                                           
502400     .                                                                    
502500     EJECT                                                                
502600 IMS-GNP-WDL521                 SECTION.                                  
502700                                                                          
502800     STRING 'WDL511  (WDL511KY =' W-WDL511KY-X ')'                        
502900          DELIMITED BY SIZE INTO SSA1                                     
503000     STRING 'WDL521  (IDARTNR  =' W-IDARTNR-L5-X ')'                      
503100          DELIMITED BY SIZE INTO SSA2                                     
503200     MOVE '  GE'           TO GODK-STATUSKODER                            
503300     CALL CBLTDLI USING GNP WDL5-PCB DLI-IO-WDL521 SSA1 SSA2              
503400     MOVE WDL5-STATUS-CODE TO STATUS-WS                                   
503500     PERFORM IMS-STATUSKONTROLL                                           
503600     .                                                                    
503700     EJECT                                                                
503800 IMS-GNP-WDL521-KLI-1ST         SECTION.                                  
503900                                                                          
504000     STRING 'WDL511  (WDL511KY =' W-WDL511KY-X ')'                        
504100          DELIMITED BY SIZE INTO SSA1                                     
504200     MOVE 'WDL521  *F '       TO SSA2                                     
504300     MOVE '  GE'           TO GODK-STATUSKODER                            
504400     CALL CBLTDLI USING GNP WDL5-PCB DLI-IO-WDL521 SSA1 SSA2              
504500     MOVE WDL5-STATUS-CODE TO STATUS-WS                                   
504600     PERFORM IMS-STATUSKONTROLL                                           
504700     .                                                                    
504800     EJECT                                                                
504900 IMS-GNP-WDL521-KLI             SECTION.                                  
505000                                                                          
505100     STRING 'WDL511  (WDL511KY =' W-WDL511KY-X ')'                        
505200          DELIMITED BY SIZE INTO SSA1                                     
505300     MOVE 'WDL521  '       TO SSA2                                        
505400     MOVE '  GE'           TO GODK-STATUSKODER                            
505500     CALL CBLTDLI USING GNP WDL5-PCB DLI-IO-WDL521 SSA1 SSA2              
505600     MOVE WDL5-STATUS-CODE TO STATUS-WS                                   
505700     PERFORM IMS-STATUSKONTROLL                                           
505800     .                                                                    
505900     EJECT                                                                
506000 IMS-GU-WLBENA11                SECTION.                                  
506100                                                                          
506200     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
506300            DELIMITED BY SIZE INTO SSA1                                   
506400     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
506500            DELIMITED BY SIZE INTO SSA2                                   
506600     MOVE '  ' TO GODK-STATUSKODER                                        
506700     CALL CBLTDLI USING GU  BENA-PCB DLI-IO-AREA2 SSA1 SSA2               
506800     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
506900     PERFORM IMS-STATUSKONTROLL                                           
507000     .                                                                    
507100     EJECT                                                                
507200                                                                          
507300 IMS-GET-WDP311                 SECTION.                                  
507400                                                                          
507500     STRING 'WDP301  (KDARBTYP =' W-KDARBTYP-X ')'                        
507600            DELIMITED BY SIZE INTO SSA1                                   
507700     STRING 'WDP311  (IDPERSON =' W-IDPERSON-X ')'                        
507800            DELIMITED BY SIZE INTO SSA2                                   
507900     MOVE '  GE' TO GODK-STATUSKODER                                      
508000     CALL CBLTDLI USING GU  WDP3-PCB DLI-IO-AREA-P311 SSA1 SSA2           
508100     MOVE WDP3-STATUS-CODE TO STATUS-WS                                   
508200     PERFORM IMS-STATUSKONTROLL                                           
508300     .                                                                    
508400     EJECT                                                                
508500 IMS-GET-EMBB01                 SECTION.                                  
508600                                                                          
508700     STRING 'WLEMBB01(KDKOLLI  =' W-KDKOLLI-X ')'                         
508800          DELIMITED BY SIZE INTO SSA1                                     
508900     MOVE '  GE'           TO GODK-STATUSKODER                            
509000     CALL CBLTDLI USING GU EMBB-PCB DLI-IO-AREA2 SSA1                     
509100     MOVE EMBB-STATUS-CODE TO STATUS-WS                                   
509200     PERFORM IMS-STATUSKONTROLL                                           
509300     .                                                                    
509400     EJECT                                                                
509500 IMS-GU-410901-ROT              SECTION.                                  
509600     STRING 'WL410901(WDGXKEY  =' W-4109-X ')'                            
509700          DELIMITED BY SIZE INTO SSA1                                     
509800     MOVE '    ' TO GODK-STATUSKODER                                      
509900     CALL CBLTDLI USING GU 4109-PCB DLI-IO-AREA-4109 SSA1                 
510000     MOVE 4109-STATUS-CODE TO STATUS-WS                                   
510100     PERFORM IMS-STATUSKONTROLL                                           
510200     .                                                                    
510300                                                                          
510400 IMS-GHNP-410911-KVAL           SECTION.                                  
510500     STRING 'WL410911(KEY4110 =>' W-WDGXKEY-MIN-X                         
510600                    '&KEY4110 =<' W-WDGXKEY-MAX-X ')'                     
510700          DELIMITED BY SIZE INTO SSA1                                     
510800     MOVE '  GE' TO GODK-STATUSKODER                                      
510900     CALL CBLTDLI USING GHNP 4109-PCB DLI-IO-AREA-4110 SSA1               
511000     MOVE 4109-STATUS-CODE TO STATUS-WS                                   
511100     PERFORM IMS-STATUSKONTROLL                                           
511200     .                                                                    
511300                                                                          
511400 IMS-REPL-4109                  SECTION.                                  
511500                                                                          
511600     MOVE '  ' TO GODK-STATUSKODER                                        
511700     CALL CBLTDLI USING REPL 4109-PCB DLI-IO-AREA-4110                    
511800     MOVE 4109-STATUS-CODE TO STATUS-WS                                   
511900     PERFORM IMS-STATUSKONTROLL                                           
512000     .                                                                    
512100     EJECT                                                                
512200 IMS-GNP-410911-KVAL SECTION.                                             
512300     MOVE '*** IMS-GNP-410911-KVAL *** '                                  
512400                              TO FELTEXT                                  
512500                                                                          
512600     STRING 'WL410911(KEY4110 =>' W-WDGXKEY-MIN-X                         
512700                    '&KEY4110 =<' W-WDGXKEY-MAX-X ')'                     
512800          DELIMITED BY SIZE INTO SSA1                                     
512900     MOVE '  GE' TO GODK-STATUSKODER                                      
513000     CALL CBLTDLI USING                                                   
513100           GNP 4109-PCB DLI-IO-AREA-4110 SSA1                             
513200     MOVE 4109-STATUS-CODE TO STATUS-WS                                   
513300     PERFORM IMS-STATUSKONTROLL                                           
513400     .                                                                    
513500     EJECT                                                                
513600 IMS-GU-GMTA-WDB201               SECTION.                                
513700                                                                          
513800     STRING 'WDB201  (IDGMT    =' W-IDGMT-X ')'                           
513900          DELIMITED BY SIZE INTO SSA1                                     
514000     MOVE '  GE'              TO GODK-STATUSKODER                         
514100                                                                          
514200     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-WDB201 SSA1                    
514300     MOVE WDB2-STATUS-CODE    TO STATUS-WS                                
514400     PERFORM IMS-STATUSKONTROLL                                           
514500     .                                                                    
514600     EJECT                                                                
514700 IMS-GET-WDB201 SECTION.                                                  
514800                                                                          
514900     STRING 'WDB201  (IDGMT   >=' W-IDGMT-MIN-X                           
515000                    '&IDGMT   <=' W-IDGMT-MAX-X ')'                       
515100          DELIMITED BY SIZE INTO SSA1                                     
515200     MOVE '    '              TO GODK-STATUSKODER                         
515300                                                                          
515400     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-WDB201 SSA1                    
515500     MOVE WDB2-STATUS-CODE    TO STATUS-WS                                
515600     PERFORM IMS-STATUSKONTROLL                                           
515700     .                                                                    
515800     EJECT                                                                
515900 IMS-GU-WDB1-WDB101              SECTION.                                 
516000                                                                          
516100     STRING 'WDB101  (WDB101KY =' W-WDB101KY-X ')'                        
516200          DELIMITED BY SIZE INTO SSA1                                     
516300     MOVE '  GE'              TO GODK-STATUSKODER                         
516400                                                                          
516500     CALL CBLTDLI USING GU WDB1-PCB DLI-IO-WDB101 SSA1                    
516600     MOVE WDB1-STATUS-CODE    TO STATUS-WS                                
516700     PERFORM IMS-STATUSKONTROLL                                           
516800     .                                                                    
516900     EJECT                                                                
517000 IMS-GU-WDB601    SECTION.                                                
517100     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
517200          DELIMITED BY SIZE INTO SSA1                                     
517300     MOVE '  GE' TO GODK-STATUSKODER                                      
517400     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
517500     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
517600     PERFORM IMS-STATUSKONTROLL                                           
517700     .                                                                    
517800     EJECT                                                                
517900 IMS-GNP-WDB611-FIRST    SECTION.                                         
518000                                                                          
518100     STRING 'WDB611  *F(WDB611KY =' W-WDB611KY-X ')'                      
518200          DELIMITED BY SIZE INTO SSA1                                     
518300     MOVE '  GE' TO GODK-STATUSKODER                                      
518400     CALL CBLTDLI USING GNP WDB6-PCB DLI-IO-WDB611 SSA1                   
518500     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
518600     PERFORM IMS-STATUSKONTROLL                                           
518700     .                                                                    
518800     EJECT                                                                
518900 IMS-GU-WDGX6327 SECTION.                                                 
519000                                                                          
519100     STRING 'WDR501  (WDGXKEY  =' W-WDGXKEY-6327-X ')'                    
519200          DELIMITED BY SIZE INTO SSA1                                     
519300     MOVE '  GE' TO GODK-STATUSKODER                                      
519400     CALL CBLTDLI USING GU 6327-PCB DLI-IO-WDGX6327 SSA1                  
519500     MOVE 6327-STATUS-CODE TO STATUS-WS                                   
519600     PERFORM IMS-STATUSKONTROLL                                           
519700     .                                                                    
519800     EJECT                                                                
519900 IMS-GNP-WDGX6328 SECTION.                                                
520000                                                                          
520100     STRING 'WDGX6328(KY6328  >=' W-KY6328-MIN-X                          
520200                    '&KY6328  <=' W-KY6328-MAX-X                          
520300                    '&IDUSERGK =' W-IDUSER-6328-X ')'                     
520400          DELIMITED BY SIZE INTO SSA1                                     
520500     MOVE '  GE' TO GODK-STATUSKODER                                      
520600     CALL CBLTDLI USING GNP 6327-PCB DLI-IO-WDGX6328 SSA1                 
520700     MOVE 6327-STATUS-CODE TO STATUS-WS                                   
520800     PERFORM IMS-STATUSKONTROLL                                           
520900     .                                                                    
521000     EJECT                                                                
521100 IMS-GHU-WDGX4104 SECTION.                                                
521200                                                                          
521300     STRING 'WDR501  (WDGXKEY  =' W-WDGXKEY-4103-X ')'                    
521400          DELIMITED BY SIZE INTO SSA1                                     
521500     STRING 'WDGX4104(KEY4104  =' W-WDGXKEY-4104-X ')'                    
521600          DELIMITED BY SIZE INTO SSA2                                     
521700     MOVE '  GE' TO GODK-STATUSKODER                                      
521800     CALL CBLTDLI USING GHU 4103-PCB DLI-IO-WDGX4104 SSA1 SSA2            
521900     MOVE 4103-STATUS-CODE TO STATUS-WS                                   
522000     PERFORM IMS-STATUSKONTROLL                                           
522100     .                                                                    
522200     EJECT                                                                
522300 IMS-GHNP-WDGX4104-KVAL SECTION.                                          
522400                                                                          
522500     STRING 'WDGX4104*F(KEY4104  =' W-WDGXKEY-4104-X ')'                  
522600          DELIMITED BY SIZE INTO SSA1                                     
522700     MOVE '  GE' TO GODK-STATUSKODER                                      
522800     CALL CBLTDLI USING GHNP 4103-PCB DLI-IO-WDGX4104 SSA1                
522900     MOVE 4103-STATUS-CODE TO STATUS-WS                                   
523000     PERFORM IMS-STATUSKONTROLL                                           
523100     .                                                                    
523200     EJECT                                                                
523300 IMS-REPL-WDGX4104 SECTION.                                               
523400                                                                          
523500     MOVE '  ' TO GODK-STATUSKODER                                        
523600     CALL CBLTDLI USING REPL 4103-PCB DLI-IO-WDGX4104                     
523700     MOVE 4103-STATUS-CODE TO STATUS-WS                                   
523800     PERFORM IMS-STATUSKONTROLL                                           
523900     .                                                                    
524000     EJECT                                                                
524100 IMS-GU-WDGX4103 SECTION.                                                 
524200                                                                          
524300     STRING 'WDR501  (WDGXKEY  =' W-WDGXKEY-4103-X ')'                    
524400          DELIMITED BY SIZE INTO SSA1                                     
524500     MOVE '  GE' TO GODK-STATUSKODER                                      
524600     CALL CBLTDLI USING GU 4103-PCB DLI-IO-WDGX4103 SSA1                  
524700     MOVE 4103-STATUS-CODE TO STATUS-WS                                   
524800     PERFORM IMS-STATUSKONTROLL                                           
524900     .                                                                    
525000     EJECT                                                                
525100 IMS-GNP-WDGX4104-KVAL SECTION.                                           
525200                                                                          
525300     STRING 'WDGX4104(KEY4104  =' W-WDGXKEY-4104-X ')'                    
525400          DELIMITED BY SIZE INTO SSA1                                     
525500     MOVE '  GE' TO GODK-STATUSKODER                                      
525600     CALL CBLTDLI USING GNP 4103-PCB DLI-IO-WDGX4104 SSA1                 
525700     MOVE 4103-STATUS-CODE TO STATUS-WS                                   
525800     PERFORM IMS-STATUSKONTROLL                                           
525900     .                                                                    
526000     EJECT                                                                
526100 IMS-GHNP-WDGX4104 SECTION.                                               
526200                                                                          
526300     MOVE 'WDGX4104 ' TO SSA1                                             
526400     MOVE '  GE' TO GODK-STATUSKODER                                      
526500     CALL CBLTDLI USING GHNP 4103-PCB DLI-IO-WDGX4104 SSA1                
526600     MOVE 4103-STATUS-CODE TO STATUS-WS                                   
526700     PERFORM IMS-STATUSKONTROLL                                           
526800     .                                                                    
526900     EJECT                                                                
527000 IMS-DLET-WDGX4104 SECTION.                                               
527100                                                                          
527200     MOVE '  ' TO GODK-STATUSKODER                                        
527300     CALL CBLTDLI USING DLET 4103-PCB DLI-IO-WDGX4104                     
527400     MOVE 4103-STATUS-CODE TO STATUS-WS                                   
527500     PERFORM IMS-STATUSKONTROLL                                           
527600     .                                                                    
527700     EJECT                                                                
527800 IMS-ISRT-WDGX4103 SECTION.                                               
527900     MOVE 'WDR501  ' TO SSA1                                              
528000     MOVE '    ' TO GODK-STATUSKODER                                      
528100     CALL CBLTDLI USING ISRT 4103-PCB DLI-IO-WDGX4103 SSA1                
528200     MOVE 4103-STATUS-CODE TO STATUS-WS                                   
528300     PERFORM IMS-STATUSKONTROLL                                           
528400     .                                                                    
528500     EJECT                                                                
528600 IMS-ISRT-WDGX4104 SECTION.                                               
528700                                                                          
528800     STRING 'WDR501  (WDGXKEY  =' W-WDGXKEY-4103-X ')'                    
528900          DELIMITED BY SIZE INTO SSA1                                     
529000     MOVE 'WDGX4104 ' TO SSA2                                             
529100     MOVE '    ' TO GODK-STATUSKODER                                      
529200     CALL CBLTDLI USING ISRT 4103-PCB DLI-IO-WDGX4104 SSA1 SSA2           
529300     MOVE 4103-STATUS-CODE TO STATUS-WS                                   
529400     PERFORM IMS-STATUSKONTROLL                                           
529500     .                                                                    
529600     EJECT                                                                
529700 IMS-STATUSKONTROLL             SECTION.                                  
529800                                                                          
529900     SET STATUS-IX TO 1                                                   
530000     SEARCH GODK-STATUS                                                   
530100       AT END                                                             
530200         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
530300         DELIMITED BY SIZE INTO FELTEXT                                   
530400         CALL FELLOG                                                      
530500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
530600         CONTINUE                                                         
530700     END-SEARCH                                                           
530800     .                                                                    
