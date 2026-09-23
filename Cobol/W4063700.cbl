000100**********************************************************                
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W4063700.                                                
000400 AUTHOR.         MOGREN STINA.                                            
000500 DATE-WRITTEN.   02/02/19.                                                
000600 DATE-COMPILED.                                                           
000700*                                                                         
000700*                                                                         
000800*    FUNKTION:                                                            
000900*        PROGRAMMET ÄR EN BAKGRUNDS-MPP I BUILD-IT MODULEN                
001000*        SOM SKAPAR RADER ATT FAKTURERA I BILL-IT                         
001100*                                                                         
001200*        PROGRAMMET STARTAS AV:                                           
001300*        * 4636 (ADD-IT)                                                  
001400*        * EL. 4638 FÖR SÄNDNING NÄR PRIS INKOMMIT                        
001500*        * 4634 (ADD-IT) MEN BARA FÖR STUDS-FLÖDET (DUBBLA FAKT.)         
001600*                                                                         
001700*        RADERNA SOM SKA FAKTURERAS SÄNDES MED WZ01                       
001800*                                                                         
001900*        PROGRAMMET LÄSER      WDE2  TRANSPORTRELREG                      
002000*                                    FAKTURA - BILL-IT                    
002100*                              WDE1                                       
002200*                              WDE4                                       
002300*                              WDD3  BENÄMNING                            
002400*                              WDB2 / WDB1  KUNDREG                       
002500*                              WDR1 / 4735  LEV.VILLKOR-TEXT              
002600*                              WDB6  DC-REGISTER                          
002700*                              WDK7 DIST35-                               
002800*                              WDG2                                       
002900*                              WDK6                                       
003000*                              WDR2                                       
003100*                              WDL3                                       
003200*                                                                         
003300*    INDATA.                                                              
003400*        TRANSAKTION: W40637X                                             
003500*        MID:         W40637I1                                            
003600*                                                                         
003700*    UTDATA.                                                              
003800*        WZ01-RADER   TILL BILL-IT                                        
003900*                     ELLER PASS-IT  FÖR SATS-RADER                       
004000*                                                                         
004100*    CCID : 10275853, PREFERENTIAL COUNTRY OF ORIGIN                      
004200*    PBI  : 1574986 - FAKT. PER KUND USA->CA                              
004300*    PBI  : 1574986 - 20/12--> BACKA FAKT. PER KUND                       
004400*    STORY: 1791283 - NOV.'20 LYNK ARTNR. TILL BILLIT                     
004500*    STORY: 2496512 - NOV.'21 LEV.VILLKOR DISTR. 2688                     
004600*                                                                         
004700     SKIP3                                                                
004800 ENVIRONMENT DIVISION.                                                    
004900                                                                          
005000 DATA DIVISION.                                                           
005100     EJECT                                                                
005200 WORKING-STORAGE SECTION.                                                 
005300 77  IDPGM                       PIC X(08)   VALUE 'W4063700'.            
005601                                                                          
005500*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
005600 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
005602                                                                          
006502                                                                          
005900 77  JA                          PIC X       VALUE 'J'.                   
006000 77  YES                         PIC X       VALUE 'Y'.                   
006100 77  NEJ                         PIC X       VALUE 'N'.                   
006200*------                                                                   
006300 77  ANTAL-SEND                  PIC S9(4)   BINARY VALUE ZERO.           
057800                                                                          
006500*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
058000                                                                          
058200                                                                          
006800 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
006900     88  NYCKLAR-OK                          VALUE 'J'.                   
007000     88  NYCKLAR-FEL                         VALUE 'N'.                   
064000                                                                          
007200 77  ALLT-SW                     PIC X       VALUE 'J'.                   
007300     88  ALLT-OK                             VALUE 'J'.                   
066600                                                                          
007500 77  NYTT-SW                     PIC X       VALUE 'J'.                   
007600     88  EJ-NYTT                             VALUE 'N'.                   
007700     88  JA-NYTT                             VALUE 'J'.                   
067200                                                                          
007900 77  FRAKT-SW                    PIC X       VALUE 'J'.                   
008000     88  FRAKT-OK                            VALUE 'J'.                   
008100     88  FRAKT-SKRIV                         VALUE 'N'.                   
067400                                                                          
008300 77  SATS-SW                     PIC X       VALUE 'N'.                   
008400     88  SATS-JA                             VALUE 'J'.                   
008500     88  SATS-NEJ                            VALUE 'N'.                   
067600                                                                          
008700 77  SOFT-SPEC-SW                PIC X       VALUE 'N'.                   
008800     88  SOFT-SPEC                           VALUE 'J'.                   
008900     88  SOFT-SPEC-NEJ                       VALUE 'N'.                   
069000                                                                          
009100 77  VOR-CDC-11                  PIC X(01)   VALUE 'N'.                   
009200     88 VOR-OK                               VALUE 'J'.                   
070500                                                                          
009400 77  LYNK-DISTR-SW               PIC X(01)   VALUE 'N'.                   
009500     88 LYNK-DISTR                           VALUE 'J'.                   
076299                                                                          
009700 77  TRACK-SW                    PIC X       VALUE 'J'.                   
009800     88  TRACK-SKRIV                         VALUE 'J'.                   
076310                                                                          
010000 77  ADDL-COST-SW                PIC X       VALUE 'J'.                   
010100     88  ADDL-COST                           VALUE 'N'.                   
076700                                                                          
010300 77  ADDL-COST1-SW                PIC X      VALUE 'J'.                   
010400     88  ADDL-COST1                          VALUE 'N'.                   
076900                                                                          
010600 77  W-RAD-RAKNARE               PIC 9(5)    VALUE ZERO.                  
010700 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   VALUE +33  COMP-3.           
010800 77  KDRC-DISP                   PIC 9(4)    VALUE ZERO.                  
010900 77  WX-KDVALISO                 PIC X(3)    VALUE SPACE.                 
011000 77  W-SKRIV                     PIC S9(3)   VALUE ZERO COMP-3.           
011100 77  TEST-IDFKNGRP               PIC S9(5)   COMP-3.                      
011200     88  FKNGRP-VSA-IMP                      VALUE 1788.                  
011300     88  FKNGRP-EXT-WARRANTY                 VALUE 1728.                  
011400 77  POLESTAR                    PIC X(8)    VALUE 'POLESTAR'.            
011500 77  W-DATE-AAMM                 PIC 9(4)    VALUE ZERO.                  
011600 77  WS-KDVALISO-HUV             PIC X(3)    VALUE 'SEK'.                 
011700 77  WS-BELEVVIL                 PIC X(35)   VALUE SPACE.                 
077300                                                                          
011900 01  WS-PRAVCOST                 PIC S9(7)V9(2) VALUE ZERO COMP-3.        
077500                                                                          
012100 01  KONSTANTER.                                                          
012200     03   GEN-IDSTATNR           PIC S9(9) COMP-3 VALUE 87089997.         
012300 01  LAS-TAB.                                                             
012400     03  L0  PIC S9(3)   VALUE ZERO.                                      
012500     03  LL  OCCURS 50.                                                   
012600       05  L1   PIC 99.                                                   
012700       05  L2   PIC 9(5).                                                 
012800       05  L3   PIC 9(5).                                                 
080300                                                                          
013000 77  IX                          PIC S9(3)  VALUE ZERO COMP-3.            
013100 77  MAX-IX                      PIC S9(3)  VALUE +7   COMP-3.            
013200 77  WS-IDMARKBO                 PIC X      VALUE SPACE.                  
013300 77  WS-KDVALISO-MC              PIC X(3)   VALUE SPACE.                  
013400 77  WS-KDVALISO-COST            PIC X(3)   VALUE SPACE.                  
013500 77  WS-IDPARTNR-BOUNCE          PIC X(9)   VALUE SPACE.                  
013600 77  WS-IDDC-EXP-REC             PIC X(2)   VALUE SPACE.                  
013700 77  WS-KVAR                     PIC S9(7)  VALUE ZERO COMP-3.            
013800 77  TRACK-IX                    PIC S9(3)  VALUE ZERO COMP-3.            
013900 77  MAX-TRACK-IX                PIC S9(3)  VALUE +5   COMP-3.            
014000 77  WS-KVTRACK                  PIC 9(7)   VALUE ZERO.                   
014100 77  WS-IDTRACK                  PIC X(25)  VALUE SPACE.                  
014200 77  WS-FREIGHT                  PIC 9(7)V9(2) VALUE ZERO.                
014300 77  WS-INSURANCE                PIC 9(7)V9(2) VALUE ZERO.                
014400 77  WS-FREIGHT-SAVE             PIC 9(7)V9(2) VALUE ZERO.                
014500 77  WS-INSURANCE-SAVE           PIC 9(7)V9(2) VALUE ZERO.                
080700                                                                          
014700 01  FILLER                      PIC X(16)  VALUE 'WS-SEKTION'.           
014800 01  WS-SEKTION                  PIC X(30)  VALUE SPACE.                  
080900                                                                          
015000 01  WS-REDUIN                   PIC X(30)  VALUE SPACE.                  
015100 01  WS-REDUUT                   PIC X(30)  VALUE SPACE.                  
082000                                                                          
015300 01  WS-NUM11                    PIC 9(11).                               
015400 01  WS-NUM9                     PIC 9(9).                                
015500 01  WS-NUM7                     PIC 9(7).                                
015600 01  WS-NUM5                     PIC 9(5).                                
082900                                                                          
015800 01  WX-IDORDNR                  PIC X(8)   VALUE SPACE.                  
015900 01  WX-IDKUNDNR                 PIC X(8)   VALUE SPACE.                  
016000 01  WX-IDKOLLI                  PIC X(8)   VALUE SPACE.                  
016100 01  WX-IDPRODNR                 PIC X(8)   VALUE SPACE.                  
016200 01  WX-IDSHIPM                  PIC X(8)   VALUE SPACE.                  
083500                                                                          
016400 01  WS-IDARTNR-SOFTWARE         PIC X(50).                               
016500 01  FILLER                      REDEFINES WS-IDARTNR-SOFTWARE.           
016600     03  WS-IDARTNR              PIC X(10).                               
016700     03  WS-IDARBREF             PIC X(10).                               
016800     03  WS-IDBIL                PIC X(13).                               
016900     03  WS-IDVIN                PIC X(17).                               
084500                                                                          
017100 01  WS-IDARTNR-CNTRL            PIC X(2).                                
017200 01  FILLER                      REDEFINES WS-IDARTNR-CNTRL.              
017300     03  WS-REKSIFFR1            PIC X(1).                                
017400     03  WS-REKSIFFR2            PIC X(1).                                
085500                                                                          
017600 01  WS-IDARTNR-LYNK             PIC X(50).                               
017700 01  FILLER                      REDEFINES WS-IDARTNR-LYNK.               
017800     03  WS-IDARTNR-V            PIC X(10).                               
017900     03  WS-IDARTNR-L            PIC X(12).                               
018000     03  FILLER                  PIC X(28).                               
086700                                                                          
018200 01  WS-IDCOM                    PIC S9(9)   VALUE ZERO COMP-3.           
018300 01  FILLER                      PIC X(16)   VALUE 'WS-IDKOLLI'.          
018400 01  WS-IDKOLLI                  PIC S9(5)   VALUE ZERO COMP-3.           
087200                                                                          
018600 01  WS-IDPARTNR                 PIC X(9)    VALUE SPACE.                 
018700 01  FILLER                      REDEFINES WS-IDPARTNR.                   
018800     03  WS-INT                  PIC 9(2).                                
018900     03  FILLER                  PIC 9(7).                                
088100                                                                          
019100 01  WS-IDSKYLT                  PIC X(3)    VALUE SPACE.                 
019200 01  WS-KDSPRAK                  PIC S9      VALUE ZERO COMP-3.           
                                                                                
019400 01  WS-TIKLOCK                  PIC S9(9)   VALUE ZERO COMP-3.           
019500 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
                                                                                
019700 01  WS-DATUM                    PIC 9(8).                                
019800 01  FILLER                      REDEFINES WS-DATUM.                      
019900     03  WS-SEKEL                PIC 9(2).                                
020000     03  WS-AAMMDD               PIC 9(6).                                
                                                                                
020200 01  WS-DAINLEV                  PIC 9(16).                               
020300 01  FILLER                      REDEFINES WS-DAINLEV.                    
020400     03  WS-DADATUM              PIC 9(8).                                
020500     03  WS-HHMMSSTH             PIC 9(8).                                
                                                                                
020700 01  W-BETEXT                    PIC X(125) VALUE SPACES.                 
020800 01  FILLER                      REDEFINES   W-BETEXT.                    
020900     03  W-RAD1                  PIC X(30).                               
021000     03  W-RAD2                  PIC X(30).                               
021100     03  W-GATA                  PIC X(30).                               
021200     03  W-PADR                  PIC X(20).                               
021300     03  W-LAND                  PIC X(15).                               
                                                                                
021500 01  Y-BETEXT                    PIC X(125) VALUE SPACES.                 
021600 01  FILLER                      REDEFINES   Y-BETEXT.                    
021700     03  Y-RAD1                  PIC X(30).                               
021800     03  Y-RAD2                  PIC X(30).                               
021900     03  Y-GATA                  PIC X(30).                               
022000     03  Y-PADR                  PIC X(20).                               
022100     03  Y-LAND                  PIC X(15).                               
                                                                                
022300 01  Z-BETEXT                    PIC X(125) VALUE SPACES.                 
022400 01  FILLER                      REDEFINES   Z-BETEXT.                    
022500     03  Z-RAD1                  PIC X(30).                               
022600     03  Z-RAD2                  PIC X(30).                               
022700     03  Z-GATA                  PIC X(30).                               
022800     03  Z-PADR                  PIC X(35).                               
                                                                                
023000*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
023100 01  GENERELLA-SUBPROGRAM.                                                
023200     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
023300     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
023400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
023500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
023600     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
023700     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
023800     03  W009REDU                PIC X(8)    VALUE 'W009REDU'.            
023900     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
024000     03  WZ01RECV                PIC X(8)    VALUE 'WZ01RECV'.            
024100     03  W111PCOO                PIC X(8)    VALUE 'W111PCOO'.            
024200     03  W510AVG                 PIC X(8)    VALUE 'W510AVG '.            
024300     03  W510CURR                PIC X(8)    VALUE 'W510CURR'.            
024400     EJECT                                                                
024500                                                                          
024600*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
024700*01 -COPY WMEDAREA                                                        
024800     SKIP3                                                                
024900 01  MESSAGE-CODES.                                                       
025000     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
025100     EJECT                                                                
025200*    --- ARBETSFÄLT FÖR BERÄKNING AV DAT./TID                             
025300 77  WS-AAAAMMDD                 PIC 9(8)    VALUE ZERO.                  
025400 77  WS-TTMMSSTH                 PIC 9(8)    VALUE ZERO.                  
025500 77  WS-TTMMSS                   PIC 9(6)    VALUE ZERO.                  
025600                                                                          
025700*                                                                         
025800*      CONSTANTER                                                         
025900*01  -COPY WWDCKONS                                                       
026000                                                                          
026100*01  -COPY WWDC99                                                         
026200                                                                          
026300 01  TEST-IDDISTR                PIC 9(5)    COMP-3 VALUE ZERO.           
026400*01  FILLER   -COPY WWDIST03    -RED TEST-IDDISTR.                        
026500     EJECT                                                                
026600*01  FILLER   -COPY WWDIST07    -RED TEST-IDDISTR.                        
026700     EJECT                                                                
026800*01  FILLER   -COPY WWDIST18    -RED TEST-IDDISTR.                        
026900     EJECT                                                                
027000*01  FILLER   -COPY WWDIST19    -RED TEST-IDDISTR.                        
027100     EJECT                                                                
027200*01  FILLER   -COPY WWDIST25    -RED TEST-IDDISTR.                        
027300     EJECT                                                                
027310*01  FILLER   -COPY WWDIST35    -RED TEST-IDDISTR.                        
027320     EJECT                                                                
027400*01  FILLER   -COPY WWDIST38    -RED TEST-IDDISTR.                        
027500     EJECT                                                                
027600*01  FILLER   -COPY WWDIST41    -RED TEST-IDDISTR.                        
027700     EJECT                                                                
027800*01  FILLER   -COPY WWDIST42    -RED TEST-IDDISTR.                        
027900     EJECT                                                                
028000*01  FILLER   -COPY WWDIST74    -RED TEST-IDDISTR.                        
028100     EJECT                                                                
028200*01  FILLER   -COPY WWDIST76    -RED TEST-IDDISTR.                        
028300     EJECT                                                                
028400*01  FILLER   -COPY WWDIST79    -RED TEST-IDDISTR.                        
028500     EJECT                                                                
028600*01  FILLER   -COPY WWDIST90    -RED TEST-IDDISTR.                        
028700     EJECT                                                                
028800*01  FILLER   -COPY WWDIST92    -RED TEST-IDDISTR.                        
028900     EJECT                                                                
029000*01  FILLER   -COPY WWDIS105    -RED TEST-IDDISTR.                        
029100     EJECT                                                                
029200*01  FILLER   -COPY WWDIS134    -RED TEST-IDDISTR.                        
029300     EJECT                                                                
029400                                                                          
029500*01  -COPY WWPRODSL                                                       
029600     EJECT                                                                
029700                                                                          
029800*01  -COPY WDATAREA                                                       
029900     EJECT                                                                
030000*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
030100*                                                                         
030200 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
030300     SKIP3                                                                
030400*01 -COPY WMSGINIT                                                        
030500     EJECT                                                                
030600                                                                          
030700*    --- AREA FÖR W510AVG                                                 
030800 01  FILLER                    PIC X(16) VALUE 'W510AVGAREA*****'.        
030900                                                                          
031000*01  -COPY W510AVG                                                        
031100     EJECT                                                                
031200*    --- AREA FÖR W510CURR                                                
031300*01  -COPY W510CURR                                                       
031400     EJECT                                                                
031500                                                                          
031600 01  FILLER                      PIC X(16)   VALUE 'NOLL-FRAKT'.          
031700*    ---AREA MED FRAKT/FÖRSÄKRING  KUND=0  INGA KOLLI UNDER               
031800*01  -COPY WDE211    -PRE  NOLL-                                          
031900                                                                          
032000*                                                                         
032100*    --- AREOR FÖR ANROP TILL WZ01  ------                                
032200 01  FILLER                      PIC X(16)   VALUE 'WZ01-SEND'.           
032300*01  -COPY WZ01SEND                                                       
032400                                                                          
032500 01  UT-AREA.                                                             
032600*    03  FILLER -COPY WZ01REQU  -PRE UT-                                  
032700*    03  FILLER -COPY WF0201I1  -PRE WF-                                  
032800                                                                          
032900 01  4679-AREA.                                                           
033000     03  4679-IDSHIPM            PIC X(7).                                
033100     03  4679-KDCALL             PIC X       VALUE '1'.                   
033200                                                                          
033300*    --- AREOR FÖR ANROP FRÅN WZ01                                        
033400 01  FILLER                      PIC X(16)   VALUE 'WZ01-RECV '.          
033500*01  -COPY WZ01RECV                                                       
033600                                                                          
033700*    --- AREA FÖR ANROP TILL W111PCOO                                     
033800 01  FILLER                      PIC X(16)   VALUE 'W111PCOO  '.          
033900*01  -COPY W111PCOO                                                       
034000                                                                          
034100*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
034200*                                                                         
034300 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
034400     SKIP3                                                                
034500*01  MID -COPY W40637I1                                                   
034600     EJECT                                                                
034700                                                                          
034800 01  WY-WDE231.                                                           
034900*    03  -COPY WDE231   -PRE WY-                                          
035000                                                                          
035100 01  TEST-IDLANDX2          PIC X(2).                                     
035200*01  FILLER  -COPY WWLANDX2 -RED TEST-IDLANDX2.                           
035300     EJECT                                                                
035400*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
035500*                                                                         
035600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
035700     SKIP3                                                                
035800 01  NYCKLAR-TILL-DLI.                                                    
035900*                                                                         
036000     03  W-IDARTNR-WDK701-X.                                              
036100         05  W-IDARTNR-WDK701    PIC S9(9)   VALUE ZERO COMP-3.           
                                                                                
036300     03  W-IDDC-WDK711-X.                                                 
036400         05  W-IDDC-WDK711       PIC X(2)    VALUE SPACE.                 
                                                                                
036600     03  W-IDSHIPM-X.                                                     
036700         05  W-IDSHIPM           PIC  9(7)   VALUE ZERO.                  
                                                                                
036900     03  W-WDE211KY-X.                                                    
037000         05  W-IDDISTR           PIC S9(5)   VALUE ZERO COMP-3.           
037100         05  W-IDKUNDNR          PIC S9(7)   VALUE ZERO COMP-3.           
                                                                                
037300     03  W-WDE221KY-X.                                                    
037400         05  W-IDPRODNR          PIC S9(7)   VALUE ZERO COMP-3.           
037500         05  W-IDKOLLI           PIC S9(5)   VALUE ZERO COMP-3.           
                                                                                
037700     03  W-IDPURAD-X.                                                     
037800         05  W-IDPURAD           PIC S9(5)   VALUE ZERO COMP-3.           
                                                                                
038000     03  W-WDE111KY-X.                                                    
038100         05  W-IDDISTR-E1        PIC S9(5)   VALUE ZERO COMP-3.           
038200         05  W-IDKUNDNR-E1       PIC S9(7)   VALUE ZERO COMP-3.           
                                                                                
038400     03  W-WDE121KY-X.                                                    
038500         05  W-IDPRODNR-E1       PIC S9(7)   VALUE ZERO COMP-3.           
038600         05  W-IDKOLLI-E1        PIC S9(5)   VALUE ZERO COMP-3.           
                                                                                
038800     03  W-WDE4ESEQ-X.                                                    
038900         05  W-IDPRODNR-ESEQ     PIC S9(7)   VALUE ZERO COMP-3.           
                                                                                
039100     03  W-WDE4BSEQ-X.                                                    
039200         05  W-IDPRODNR-BSEQ     PIC S9(7)   VALUE ZERO COMP-3.           
039300         05  W-IDPURAD-BSEQ      PIC S9(5)   VALUE ZERO COMP-3.           
                                                                                
039500     03  W-WDE601-X.                                                      
039600         05  W-IDPRODNR-E6       PIC S9(7)   VALUE ZERO COMP-3.           
039700     03  W-WDE611-X.                                                      
039800         05  W-IDKOLLI-E6        PIC S9(5)   VALUE ZERO COMP-3.           
                                                                                
040000     03  W-WDD3BSEQ-X.                                                    
040100         05  W-IDARTNR-D3        PIC S9(9)   VALUE ZERO COMP-3.           
040200     03  W-IDSKYLT-X.                                                     
040300         05  W-IDSKYLT           PIC X(3)    VALUE 'S  '.                 
040400     03  W-IDARTNR-X.                                                     
040500         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
                                                                                
040700*    -NYCKLAR TIL WDB201                                                  
040800     03  W-IDGMT-X.                                                       
040900       05  W-IDDISTR-WDB2        PIC S9(5)   VALUE ZERO COMP-3.           
041000       05  W-IDKUNDNR-WDB2       PIC S9(7)   VALUE ZERO COMP-3.           
041100     03  W-IDGMT-MIN-X.                                                   
041200       05  W-IDDISTR-WDB2-MIN    PIC S9(5)   VALUE ZERO COMP-3.           
041300       05  W-IDKUNDNR-WDB2-MIN   PIC S9(7)   VALUE ZERO COMP-3.           
041400     03  W-IDGMT-MAX-X.                                                   
041500       05  W-IDDISTR-WDB2-MAX    PIC S9(5)   VALUE ZERO COMP-3.           
041600       05  W-IDKUNDNR-WDB2-MAX   PIC S9(7)   VALUE ZERO COMP-3.           
*                                                                               
041800*    -NYCKLAR TIL WDF502 --> LYNK-CROSS-DB                                
041900     03  W-IDARTNR-WDF502-X.                                              
042000         05  W-IDARTNR-F5        PIC S9(9)   VALUE ZERO COMP-3.           
                                                                                
042200* TILL WDB101                                                             
042300     03  W-WDB101KY-X.                                                    
042400       05  W-WDB1-IDPARTNR       PIC X(9)    VALUE SPACE.                 
042500       05  W-WDB1-IDFTG          PIC 9(2)    VALUE ZERO.                  
                                                                                
042700* TILL WDB601                                                             
042800     03  W-IDDC-B6-X.                                                     
042900         05 W-IDDC-B6            PIC X(2).                                
                                                                                
043100* TILL WDQ201                                                             
043200     03  W-IDORDER-X.                                                     
043300         05  W-IDORDER           PIC S9(7)   VALUE ZERO COMP-3.           
043400*                                                                         
043500 01  W-WDGXKEY-4735-X.                                                    
043600     03  IDHTYP-4735             PIC X(4)    VALUE '4735'.                
043700     03  KDLEVVIL-4735           PIC S9(3)   VALUE ZERO  COMP-3.          
043800     03  FILLER-4735             PIC X(24)   VALUE LOW-VALUE.             
043900*                                                                         
044000 01  W-WDGXKEY-4141-X.                                                    
044100     03  IDHTYP-4141             PIC X(4)    VALUE '4141'.                
044200     03  FILLER                  PIC X(26)   VALUE LOW-VALUE.             
044300                                                                          
044400 01  W-WDGXKEY-4591-X.                                                    
044500     03  IDHTYP-4591             PIC X(4)    VALUE '4591'.                
044600     03  IDDC-4591               PIC X(2)    VALUE SPACE.                 
044700     03  FILLER-4591             PIC X(24)   VALUE LOW-VALUE.             
044800                                                                          
044900 01  W-IDDISTR-4592-X.                                                    
045000     03  W-IDDISTR-4592          PIC S9(5)   VALUE ZERO  COMP-3.          
045100                                                                          
045200*    --- STATUS-KOD FRÅN IMS                                              
045300 01  STATUS-WS                   PIC XX.                                  
045400     88  SEGMENT-FINNS                       VALUE '  '.                  
045500     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
045600     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
045700     88  SEGMENT-SLUT                        VALUE 'GB'.                  
045800     SKIP2                                                                
045900 01  STATUS-WS-E1                PIC XX.                                  
046000     88  SEGMENT-FINNS-E1                    VALUE '  '.                  
046100     SKIP2                                                                
046200 01  GODK-STATUSKODER.                                                    
046300     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
046400     SKIP3                                                                
046500 01  SSA1                        PIC X(64).                               
046600 01  SSA2                        PIC X(64).                               
046700 01  SSA3                        PIC X(64).                               
046800 01  SSA4                        PIC X(64).                               
046900 01  SSA5                        PIC X(64).                               
047000     EJECT                                                                
047100*    --- IMS FUNKTIONSKODER                                               
047200*01  -COPY W0003                                                          
047300     EJECT                                                                
047400                                                                          
047500*    ---  DLI INPUT-OUTPUT AREA                                           
047600                                                                          
047700 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDK701'.           
047800 01  DLI-IO-WDK701.                                                       
047900*    03  -COPY WDK701                                                     
048000     EJECT                                                                
048100 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDK711'.           
048200 01  DLI-IO-WDK711.                                                       
048300*    03  -COPY WDK711                                                     
048400     EJECT                                                                
048500 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDK728'.           
048600 01  DLI-IO-WDK728.                                                       
048700*    03  -COPY WDK728                                                     
048800     EJECT                                                                
048900 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDE401'.           
049000 01  DLI-IO-WDE401.                                                       
049100*    03  -COPY WDE401                                                     
049200     EJECT                                                                
049300 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDE411'.           
049400 01  DLI-IO-WDE411.                                                       
049500*    03  -COPY WDE411                                                     
049600     EJECT                                                                
049700 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDE101'.           
049800 01  DLI-IO-WDE101.                                                       
049900*    03  -COPY WDE101                                                     
050000     EJECT                                                                
050100 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDE131'.           
050200 01  DLI-IO-WDE131.                                                       
050300*    03  -COPY WDE131                                                     
050400     EJECT                                                                
050500 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDE141'.           
050600 01  DLI-IO-WDE141.                                                       
050700*    03  -COPY WDE141                                                     
050800     EJECT                                                                
050900 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDE201'.           
051000 01  DLI-IO-WDE201.                                                       
051100*    03  -COPY WDE201                                                     
051200     EJECT                                                                
051300 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDE211'.           
051400 01  DLI-IO-WDE211.                                                       
051500*    03  -COPY WDE211                                                     
051600     EJECT                                                                
051700 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDE221'.           
051800 01  DLI-IO-WDE221.                                                       
051900*    03  -COPY WDE221                                                     
052000     EJECT                                                                
052100 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDE231'.           
052200 01  DLI-IO-WDE231.                                                       
052300*    03  -COPY WDE231                                                     
052400     EJECT                                                                
052500 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDE611'.           
052600 01  DLI-IO-WDE611.                                                       
052700*    03  -COPY WDE611                                                     
052800     EJECT                                                                
052900 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDD311'.           
053000 01  DLI-IO-WDD311.                                                       
053100*    03  -COPY WDD311                                                     
053200     EJECT                                                                
053300 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDB201'.           
053400 01  DLI-IO-WDB201.                                                       
053500*    03  -COPY WDB201.                                                    
053600     EJECT                                                                
053700 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDB101'.           
053800 01  DLI-IO-WDB101.                                                       
053900*    03  -COPY WDB101.                                                    
054000 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDR101'.           
054100 01  DLI-IO-WDGX4735.                                                     
054200*    03  -COPY WDGX4735.                                                  
054300                                                                          
054400 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDB601'.           
054500 01   DLI-IO-AREA-B601.                                                   
054600*    03  -COPY WDB601                                                     
054700 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDQ201'.           
054800 01  DLI-IO-WDQ201.                                                       
054900*    03  -COPY WDQ201.                                                    
055000 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDK601'.           
055100 01  DLI-IO-WDK601.                                                       
055200*    03  -COPY WDK601                                                     
055300     EJECT                                                                
055400 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDK611'.           
055500 01  DLI-IO-WDK611.                                                       
055600*    03  -COPY WDK611                                                     
055700     EJECT                                                                
055800 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDR201'.           
055900 01  DLI-IO-WDGX01.                                                       
056000*    03  -COPY WDGX01DC                                                   
056100                                                                          
056200 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDGX4592'.         
056300 01  DLI-IO-WDGX4592.                                                     
056400*     03  -COPY WDGX4592.                                                 
056500     EJECT                                                                
056600 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDF502'.           
056700*                              VOLVO-LYNK-ARTCROSS                        
056800 01  DLI-IO-WDF502.                                                       
056900*  03  -COPY WDF502                                                       
057000*                                                                         
057100     EJECT                                                                
057200 01  FILLER                    PIC X(16) VALUE 'WDL301'.                  
057300*                              LOGG-TRACKING                              
057400 01  DLI-IO-WDL301.                                                       
057500*  03  -COPY WDL301                                                       
057600     EJECT                                                                
057700                                                                          
057800                                                                          
057900 LINKAGE SECTION.                                                         
058000                                                                          
058100 01  IO-PCB        PIC X.                                                 
058200                                                                          
058300*01  -COPY W0009  -PRE  BILL-                                             
058400     EJECT                                                                
058500*01  -COPY W0009  -PRE  PASS-                                             
058600     EJECT                                                                
058700*01  -COPY W0009  -PRE  SHIP2TMS-                                         
058800     EJECT                                                                
058900*01  -COPY W0008  -PRE WDE2-                                              
059000     05  FILLER                  PIC X.                                   
059100*01  -COPY W0008  -PRE WDE4-                                              
059200     05  FILLER                  PIC X.                                   
059300*01  -COPY W0008  -PRE WDE4B-                                             
059400     05  FILLER                  PIC X.                                   
059500*01  -COPY W0008  -PRE WDD3-                                              
059600     05  FILLER                  PIC X.                                   
059700*01  -COPY W0008  -PRE WDB1-                                              
059800     05  FILLER                  PIC X.                                   
059900*01  -COPY W0008  -PRE WDB2-                                              
060000     05  FILLER                  PIC X.                                   
060100*01  -COPY W0008  -PRE 4735-                                              
060200     05  FILLER                  PIC X.                                   
060300*01  -COPY W0008  -PRE WDE6-                                              
060400     05  FILLER                  PIC X.                                   
060500     EJECT                                                                
060600*01  -COPY W0008  -PRE WDB6-                                              
060700     05  FILLER                  PIC X.                                   
060800     EJECT                                                                
060900*01  -COPY W0008  -PRE WDQ2-                                              
061000     05  FILLER                  PIC X.                                   
061100     EJECT                                                                
061200*01  -COPY W0008  -PRE WDE1-                                              
061300     05  FILLER                  PIC X.                                   
061400     EJECT                                                                
061500*01  -COPY W0008  -PRE WDK7-                                              
061600     05  FILLER                  PIC X.                                   
061700     EJECT                                                                
061800*01  -COPY W0008  -PRE WDG2-                                              
061900     05  FILLER                  PIC X.                                   
062000     EJECT                                                                
062100*01  -COPY W0008  -PRE WDK6-                                              
062200     05  FILLER                  PIC X.                                   
062300     EJECT                                                                
062400*01  -COPY W0008  -PRE WDR2-                                              
062500     05  FILLER                  PIC X.                                   
062600     EJECT                                                                
062700 01  PCOO-WDM1-PCB               PIC X.                                   
062800 01  PCOO-WDR2-PCB               PIC X.                                   
062900     EJECT                                                                
063000*01  -COPY W0008  -PRE 9305-                                              
063100     05  FILLER                  PIC X.                                   
063200*01  -COPY W0008  -PRE AVG-WDB6-                                          
063300     05  FILLER                  PIC X.                                   
063400     EJECT                                                                
063500*01  -COPY W0008  -PRE WDF5-                                              
063600     05  FILLER                  PIC X.                                   
063700     EJECT                                                                
063800*01  -COPY W0008  -PRE WDL3-                                              
063900     05  FILLER                  PIC X.                                   
                                                                                
064100 PROCEDURE DIVISION  USING          IO-PCB  BILL-PCB                      
064200                                            PASS-PCB                      
064300                                            SHIP2TMS-PCB                  
064400                                            WDE2-PCB                      
064500                                            WDE4-PCB                      
064600                                            WDE4B-PCB                     
064700                                            WDD3-PCB                      
064800                                            WDB1-PCB                      
064900                                            WDB2-PCB                      
065000                                            4735-PCB                      
065100                                            WDE6-PCB                      
065200                                            WDB6-PCB                      
065300                                            WDQ2-PCB                      
065400                                            WDE1-PCB                      
065500                                            WDK7-PCB                      
065600                                            WDG2-PCB                      
065700                                            WDK6-PCB                      
065800                                            WDR2-PCB                      
065900                                            PCOO-WDM1-PCB                 
066000                                            PCOO-WDR2-PCB                 
066100                                            9305-PCB                      
066200                                            AVG-WDB6-PCB                  
066300                                            WDF5-PCB                      
066400                                            WDL3-PCB.                     
066500 MAIN SECTION.                                                            
*                                                                               
066700     PERFORM A-INIT                                                       
066800     PERFORM UNTIL  RECV-KDRC > 0                                         
066900       PERFORM B-KOLLA-NYCKLAR                                            
067000       IF NYCKLAR-OK                                                      
067100         PERFORM E-LAES-GRUNDDATA                                         
                                                                                
067300         IF ALLT-OK                                                       
                                                                                
067500           PERFORM S01-OPEN-WZ01                                          
                                                                                
067700           PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                   
067800             IF EJ-NYTT                                                   
067900               PERFORM IMS-GNP-WDE211                                     
068000               MOVE BGMT-IDDISTR   TO W-IDDISTR                           
068100               MOVE BGMT-IDKUNDNR  TO W-IDKUNDNR                          
068200             END-IF                                                       
068300             PERFORM UNTIL SEGMENT-SAKNAS                                 
068400               IF EJ-NYTT                                                 
068500                PERFORM IMS-GHNP-WDE221                                   
068600                MOVE BKOLLI-IDPRODNR TO W-IDPRODNR                        
068700                MOVE BKOLLI-IDKOLLI TO W-IDKOLLI                          
068800               END-IF                                                     
068900               PERFORM UNTIL SEGMENT-SAKNAS                               
069000                                                                          
069100*CO-CD                                                                    
069200*ONLY WDE221 WITH FLCROSS = NO AND NO INVOICE CREATED BEFORE              
069300** BECAUSE THE CROSS-DOCK CASES ARE ALREADY INVOICE AND                   
069400** THEY SHOULD NOT BE INVOICED AGAIN.                                     
069500*CO-CD                                                                    
069600                 MOVE BKOLLI-IDPRODNR TO W-IDPRODNR-E6                    
069700                 MOVE BKOLLI-IDKOLLI  TO W-IDKOLLI-E6                     
069800                 PERFORM IMS-GU-WDE611                                    
069900*                                                                         
070000                 IF (BKOLLI-FLCROSS = SPACE OR                            
070010                     BKOLLI-FLCROSS = NEJ   OR                            
070020                     BKOLLI-FLCROSS = LOW-VALUE)                          
070100*                                   OR                                    
070200*                   (BKOLLI-FLCROSS = JA AND KOLLI-IDFAKT = ZERO)         
070300*COMMENTED ABOVE LINE BECAUSE FOR 778 DIST, THE CROSS DOC CASE IS         
070400*ALSO INVOICED.                                                           
070500                                                                          
070600                   PERFORM F-KOLLA-RADER                                  
070700                   IF ALLT-OK                                             
070800                    IF BILL-IDLEVNR = SPACE OR '00000'                    
070900                      IF DIST76-BRASIL   OR                               
071000                         DIST76-SAUDI    OR                               
071100                         DIST76-COLUMBIA OR                               
071200                         DIST76-OMAN     OR                               
071300                         DIST76-UKRAINE  OR                               
071400                         DIST76-GEORGIA                                   
071500                        MOVE BKOLLI-IDPRODNR TO W-IDPRODNR-E6             
071600                        MOVE BKOLLI-IDKOLLI TO W-IDKOLLI-E6               
071700                        PERFORM IMS-GU-WDE611                             
071800                      ELSE                                                
071900*---                  FÖR REFILL-EXP OCH VOR CDC->CN/IN/KR                
072000*---                  (FOR REFILL-EXP AND VOR CDC->CN/IN/KR)              
072100*                     OCH TILLÄGGSFAKT. NON VCC TO VCC EJ DC.11           
072200*                     (+ ADDITIONAL INV.NON VCC TO VCC NOT DC.11          
072300*                                                                         
072400                        IF BILL-IDDC-EXP > SPACE       OR                 
072500                         ((DIST35-NONVCC-VCC-REFILL OR                    
072600                           DIST35-NONVCC-VCC-TRANSFER) AND                
072700                           SHIP-KDFAKSTA-EXP = 3)                         
072800                          MOVE BKOLLI-IDPRODNR TO W-IDPRODNR-E6           
072900                          MOVE BKOLLI-IDKOLLI TO W-IDKOLLI-E6             
073000                          PERFORM IMS-GU-WDE611                           
073100                        ELSE                                              
073200                          CONTINUE                                        
073300                        END-IF                                            
073400                      END-IF                                              
073500                    ELSE                                                  
073600                      MOVE BKOLLI-IDPRODNR TO W-IDPRODNR-E6               
073700                      MOVE BKOLLI-IDKOLLI TO W-IDKOLLI-E6                 
073800                      PERFORM IMS-GU-WDE611                               
073900                    END-IF                                                
074000                    IF EJ-NYTT                                            
074100                     PERFORM IMS-GNP-WDE231                               
074200                    END-IF                                                
074300                    PERFORM UNTIL SEGMENT-SAKNAS                          
074400                     MOVE NEJ      TO NYTT-SW                             
074500                     IF WS-IDKOLLI = +0 OR                                
074600                        WS-IDKOLLI = BKOLLI-IDKOLLI                       
074700                       PERFORM G-SKAPA-SEND-DATA                          
074800                     END-IF                                               
074900                     PERFORM IMS-GNP-WDE231                               
075000                    END-PERFORM                                           
075100                    PERFORM H-UPPDAT-STATUS-WDE221                        
075200                    IF WS-IDKOLLI NOT = ZERO                              
075300                      MOVE 'GE'    TO STATUS-WS                           
075400*                    SLUT PÅ FILEN                                        
075500                    END-IF                                                
075600                   ELSE                                                   
075700                    MOVE 'GE'    TO STATUS-WS                             
075800                    IF JA-NYTT                                            
075900                      MOVE NEJ   TO NYTT-SW                               
076000                    END-IF                                                
076100                   END-IF                                                 
076200                   IF WS-IDKOLLI = ZERO                                   
076210                     PERFORM IMS-GHNP-WDE221                              
076220                     MOVE BKOLLI-IDPRODNR TO W-IDPRODNR                   
076230                     MOVE BKOLLI-IDKOLLI TO W-IDKOLLI                     
076240                   END-IF                                                 
076250                 ELSE                                                     
076251                   IF JA-NYTT                                             
076252                     MOVE NEJ   TO NYTT-SW                                
076253                   END-IF                                                 
076260                   PERFORM IMS-GHNP-WDE221                                
076270                   MOVE BKOLLI-IDPRODNR TO W-IDPRODNR                     
076280                   MOVE BKOLLI-IDKOLLI  TO W-IDKOLLI                      
076290                 END-IF                                                   
076291               END-PERFORM                                                
076292               IF WS-IDKOLLI = ZERO                                       
076293                PERFORM IMS-GNP-WDE211                                    
076294                MOVE BGMT-IDDISTR   TO W-IDDISTR                          
076295                MOVE BGMT-IDKUNDNR  TO W-IDKUNDNR                         
076296               END-IF                                                     
076297               MOVE NEJ      TO FRAKT-SW                                  
076298             END-PERFORM                                                  
                                                                                
076300           END-PERFORM                                                    
*                                                                               
076400           IF SEGMENT-FINNS-E1                                            
076500*LÄS OM E101 PGA ATT ISRT-WDE141 SABBAR PEKAREN I DB                      
076600*NEED A NEW IMS-CALL AS THE POINTER IS WRONT DUE TO 'ISRT-WDE141'         
                                                                                
076800             PERFORM IMS-GHU-WDE101                                       
                                                                                
077000             MOVE WS-BELEVVIL TO SHIP-BELEVVIL                            
077100             PERFORM IMS-REPL-WDE101                                      
077200           END-IF                                                         
077300                                                                          
077400           PERFORM S03-CLOSE-WZ01                                         
077500                                                                          
077600*IF ITS NOT A BOUNCE SHIPMENT, TRIGGER 4679 WHICH WILL CHECK              
077700*THE SHIPMENT AND SEND TO TMS SYSTEM.                                     
077800           IF SHIP-KDFAKSTA-EXP < 2                                       
077900             MOVE W-IDSHIPM      TO 4679-IDSHIPM                          
078000             PERFORM S31-OPEN-SHIP2TMS                                    
078100             PERFORM S32-PUT-SHIP2TMS                                     
078200             PERFORM S33-CLOSE-SHIP2TMS                                   
078300           END-IF                                                         
078400         ELSE                                                             
078500           MOVE 'SEGMENT SAKNAS'  TO FELTEXT                              
078600           CALL ABEND USING RKOD-ABEND-MED-DUMP                           
078700         END-IF                                                           
078800       END-IF                                                             
078900       PERFORM S12-RECV-MESSAGE                                           
079000     END-PERFORM                                                          
079100     IF RECV-KDRC > 1                                                     
079200       MOVE 'WZ01-RECV AVSLUTAS FEL..'  TO FELTEXT                        
079300       CALL FELLOG                                                        
079400     END-IF                                                               
079500*                                                                         
079600     PERFORM S13-RECV-CLOSE                                               
079700     MOVE ZERO TO RETURN-CODE                                             
079800     GOBACK                                                               
079900     .                                                                    
080000     EJECT                                                                
080100 A-INIT SECTION.                                                          
080200     MOVE 'A-INIT'              TO WS-SEKTION                             
                                                                                
080400     PERFORM S11-RECV-OPEN                                                
080500     PERFORM S12-RECV-MESSAGE                                             
080600     INITIALIZE                    WF-WF0201I1                            
                                                                                
080800     MOVE  LOW-VALUE            TO W-IDGMT-MIN-X                          
                                                                                
081000     MOVE HIGH-VALUE            TO W-IDGMT-MAX-X                          
081100     MOVE ZERO                  TO GMT-IDDISTR                            
081200                                   GMT-IDKUNDNR                           
081300                                   NOLL-BGMT-IDDISTR                      
081400                                   NOLL-BGMT-IDKUNDNR                     
081500     MOVE SPACE                 TO GMT-IDPARTNR                           
081600                                   GMT-FLCOD                              
081700     MOVE ZERO                  TO GMT-IDFTG                              
081800     MOVE ZERO                  TO NOLL-BGMT-PRFRAKT                      
081900                                   NOLL-BGMT-PRFOERS                      
082000                                                                          
082100     ACCEPT DAGENS-DATUM    FROM DATE                                     
082200     ACCEPT WS-TIKLOCK      FROM TIME                                     
082300     MOVE DAGENS-DATUM(1:2) TO W-DATE-AAMM(1:2)                           
082400     MOVE DAGENS-DATUM(3:2) TO W-DATE-AAMM(3:2)                           
082500     .                                                                    
082600     EJECT                                                                
082700 B-KOLLA-NYCKLAR SECTION.                                                 
082800     MOVE 'B-KOLLA-NYCKLAR'     TO WS-SEKTION                             
*                                                                               
083000     MOVE JA                    TO NYCKLAR-SW                             
083100                                   NYTT-SW                                
083200                                   FRAKT-SW                               
083300                                   ADDL-COST-SW                           
083400                                   ADDL-COST1-SW                          
                                                                                
083600     IF MID-IDSHIPM         = ALL '+'                                     
083700       MOVE NEJ TO NYCKLAR-SW                                             
083800     ELSE                                                                 
083900       IF MID-IDSHIPM NUMERIC                                             
084000         MOVE MID-IDSHIPM     TO W-IDSHIPM                                
084100       ELSE                                                               
084200         MOVE NEJ TO NYCKLAR-SW                                           
             END-IF                                                             
           END-IF                                                               
     .                                                                          
084600     IF MID-IDKOLLI         = ALL '+'                                     
084700       MOVE ZERO              TO WS-IDKOLLI                               
084800     ELSE                                                                 
084900       IF MID-IDKOLLI NUMERIC                                             
085000         MOVE MID-IDKOLLI     TO WS-IDKOLLI                               
085100       ELSE                                                               
085200         MOVE NEJ TO NYCKLAR-SW                                           
085300       END-IF                                                             
           END-IF                                                               
     .                                                                          
085600     IF WS-IDKOLLI NOT = ZERO                                             
085700       MOVE MID-IDDISTR         TO W-IDDISTR                              
085800       MOVE MID-IDKUNDNR        TO W-IDKUNDNR                             
085900       MOVE MID-IDPRODNR        TO W-IDPRODNR                             
086000       MOVE MID-IDKOLLI         TO W-IDKOLLI                              
086100     ELSE                                                                 
086200       MOVE ZERO                TO W-IDDISTR                              
086300                                   W-IDKUNDNR                             
086400                                   W-IDPRODNR                             
086500                                   W-IDKOLLI                              
086600     END-IF                                                               
086700                                                                          
086800     IF NYCKLAR-FEL                                                       
086900       STRING 'NYCKLAR FEL '                                              
087000            DELIMITED BY SIZE INTO FELTEXT                                
087100       CALL FELLOG                                                        
087200                                                                          
087300     END-IF                                                               
087400     MOVE ZERO                  TO NOLL-BGMT-IDDISTR                      
087500                                   NOLL-BGMT-IDKUNDNR                     
087600                                   W-RAD-RAKNARE                          
087700     .                                                                    
087800     EJECT                                                                
087900 E-LAES-GRUNDDATA SECTION.                                                
088000     MOVE 'E-LAES-GRUNDDATA'   TO WS-SEKTION                              
088100                                                                          
088200     MOVE NEJ TO ALLT-SW                                                  
088300     MOVE NEJ TO FRAKT-SW                                                 
088400     PERFORM IMS-GU-WDE201                                                
088500     IF SEGMENT-FINNS                                                     
088600       IF WS-IDKOLLI = ZERO                                               
088700         PERFORM IMS-GNP-WDE211                                           
088800         IF SEGMENT-FINNS                                                 
088900           MOVE BGMT-IDDISTR     TO W-IDDISTR                             
089000           MOVE BGMT-IDKUNDNR    TO W-IDKUNDNR                            
089100           PERFORM IMS-GHNP-WDE221                                        
089200           IF SEGMENT-SAKNAS AND BGMT-IDKUNDNR = ZERO AND                 
089300                                 BGMT-IDDISTR NOT = ZERO                  
089400              MOVE BGMT-WDE211   TO NOLL-BGMT-WDE211                      
089500              PERFORM IMS-GNP-WDE211                                      
089600              IF SEGMENT-FINNS                                            
089700                MOVE BGMT-IDDISTR     TO W-IDDISTR                        
089800                MOVE BGMT-IDKUNDNR    TO W-IDKUNDNR                       
089900                PERFORM IMS-GHNP-WDE221                                   
090000              END-IF                                                      
090100           END-IF                                                         
090200           IF SEGMENT-FINNS                                               
090300             MOVE BKOLLI-IDPRODNR  TO W-IDPRODNR                          
090400             MOVE BKOLLI-IDKOLLI   TO W-IDKOLLI                           
090500             PERFORM IMS-GNP-WDE231                                       
090600             MOVE JA TO ALLT-SW                                           
090700             IF BILL-KDFINDOC NOT = 'PROF'                                
090800               PERFORM IMS-GHU-WDE101                                     
090900             END-IF                                                       
091000           END-IF                                                         
091100         END-IF                                                           
091200       ELSE                                                               
091300         PERFORM IMS-GU-WDE211                                            
091400         IF SEGMENT-FINNS                                                 
091500           MOVE BGMT-IDDISTR       TO W-IDDISTR                           
091600           MOVE BGMT-IDKUNDNR      TO W-IDKUNDNR                          
091700           PERFORM IMS-GHU-WDE221                                         
091800           IF SEGMENT-FINNS                                               
091900             MOVE BKOLLI-IDPRODNR  TO W-IDPRODNR                          
092000             MOVE BKOLLI-IDKOLLI   TO W-IDKOLLI                           
092100             PERFORM IMS-GNP-WDE231                                       
092200             MOVE JA TO ALLT-SW                                           
092300             IF BILL-KDFINDOC NOT = 'PROF'                                
092400               PERFORM IMS-GHU-WDE101                                     
092500             END-IF                                                       
092600           END-IF                                                         
092700         END-IF                                                           
092800       END-IF                                                             
092900     END-IF                                                               
093000     MOVE BGMT-IDDISTR          TO TEST-IDDISTR                           
093100     .                                                                    
093200     EJECT                                                                
093300 F-KOLLA-RADER  SECTION.                                                  
093400     MOVE 'F-KOLLA-RADER'       TO WS-SEKTION                             
093500*                                                                         
093600*    KOLLA OM KOLLIT INNEHÅLLER RADER UTAN PRIS                           
093700*    KOLLI MED RAD UTAN PRIS SKALL EJ SKICKAS SÅVIDA DET                  
093800*    INTE ÄR EXPORT                                                       
093900                                                                          
094000     MOVE BKOLLI-IDPRODNR     TO W-IDPRODNR                               
094100     MOVE BKOLLI-IDKOLLI      TO W-IDKOLLI                                
                                                                                
094300     MOVE BGMT-IDDISTR        TO TEST-IDDISTR                             
094400                                 W-IDDISTR                                
094500     MOVE BGMT-IDKUNDNR       TO W-IDKUNDNR                               
094600     PERFORM FA-SOFTWARE                                                  
094700     IF DIST42-EJ-EU AND NOT SOFT-SPEC                                    
094800       MOVE JA                  TO ALLT-SW                                
094900     ELSE                                                                 
095000       MOVE JA                  TO ALLT-SW                                
095100       IF BKOLLI-KDPRSTA = 'K' OR 'A'                                     
095200         CONTINUE                                                         
095300       ELSE                                                               
095400         MOVE NEJ             TO ALLT-SW                                  
095500       END-IF                                                             
095600     END-IF                                                               
095700     IF ALLT-OK                                                           
095800       MOVE BKOLLI-IDPRODNR     TO W-IDPRODNR-ESEQ                        
095900                                   W-IDPRODNR-BSEQ                        
096000       PERFORM  IMS-GU-WDE401-ESEQ                                        
096100       IF SEGMENT-SAKNAS                                                  
096200         MOVE ZERO              TO KORD-TIORDREG                          
096300                                   KORD-KDFRAKT                           
096400       END-IF                                                             
096500       MOVE '  '                TO STATUS-WS                              
                                                                                
096700     ELSE                                                                 
096800       MOVE 'RAD UTAN PRIS'     TO FELTEXT                                
096900     END-IF                                                               
097000     MOVE BGMT-IDDISTR          TO W-IDDISTR                              
097100     MOVE BGMT-IDKUNDNR         TO W-IDKUNDNR                             
097200*****FIX VID FÖR MÅNGA RADER                                              
097300**   DELADES PÅ 2 KÖRNINGAR, EN PER DISTRIKT                              
097400*    IF BILL-IDSHIPM    = 653346 AND                                      
097500*       BGMT-IDDISTR    = 1378                                            
097600*       MOVE NEJ          TO ALLT-SW                                      
097700*    END-IF                                                               
097800***                                                                       
097900*    IF BILL-IDSHIPM    = 653346 AND                                      
098000*       BGMT-IDDISTR    = 1778  AND                                       
098100*       BGMT-IDKUNDNR   = 85537 AND                                       
098200*       BKOLLI-IDPRODNR = 78983 AND                                       
098300*       BKOLLI-IDKOLLI  = 2                                               
098400*       MOVE NEJ          TO ALLT-SW                                      
098500*    END-IF                                                               
098600***                                                                       
098700*    IF BILL-IDSHIPM    = 1775284                                         
098800**   DELADES PÅ 2 KÖRNINGAR (MED/UTAN  CONTINUE  ELSE)                    
098900*      IF BGMT-IDDISTR  = 778 AND                                         
099000*        (BGMT-IDKUNDNR = 579 OR 580 OR 581 OR 582 )                      
099100*        CONTINUE                                                         
099200*      ELSE                                                               
099300*        MOVE NEJ         TO ALLT-SW                                      
099400*      END-IF                                                             
099500*    END-IF                                                               
099600***FIXEND                                                                 
099700     MOVE SPACE                 TO WX-KDVALISO                            
099800     .                                                                    
099900     EJECT                                                                
100000 FA-SOFTWARE  SECTION.                                                    
100100*     KOLLA OM SOFTWARE-SKEPPNING                                         
100200     MOVE NEJ                   TO SOFT-SPEC-SW                           
100300     IF DIST42-SOFT                                                       
100400*      WDE101 ÄR REDAN LÄST I E-LAES-GRUNDDATA                            
100500       IF BILL-KDFINDOC NOT = 'PROF'                                      
100600         IF SEGMENT-FINNS-E1                                              
100700           IF SHIP-IDLBBET = 'SOFTAUTF' OR 'SOFTW   '                     
100800             MOVE JA            TO SOFT-SPEC-SW                           
100900           END-IF                                                         
101000         END-IF                                                           
101100       END-IF                                                             
101200     END-IF                                                               
101300     .                                                                    
101400     EJECT                                                                
101500 G-SKAPA-SEND-DATA  SECTION.                                              
101600     MOVE 'G-SKAPA-SEND-DATA'   TO WS-SEKTION                             
                                                                                
101800*--  OBS! I BÅDE GB-XX OCH GC-XX NEDAN GÖR MAN ÄVEN                       
101900*--       PERFORM GA-RADINFO!!!                                           
                                                                                
102100     IF FRAKT-SKRIV                                                       
102200        IF NOLL-BGMT-IDDISTR = ZERO                                       
102300          PERFORM GB-FRAKT-OSV                                            
102400        ELSE                                                              
102500          PERFORM GC-FRAKT-NOLL                                           
102600        END-IF                                                            
102700        MOVE JA                 TO FRAKT-SW                               
102800     END-IF                                                               
     .                                                                          
103000     PERFORM GA-RADINFO                                                   
103100     PERFORM S02-PUT-RAD-WZ01                                             
                                                                                
103300     IF SATS-JA AND NOT DIST19-SATS                                       
103400       OR SATS-NEJ AND DIST19-SATS                                        
103500       MOVE 'SKEPPNING MED BÅDE SATS/EJ SATS' TO FELTEXT                  
103600       CALL FELLOG                                                        
103700     END-IF                                                               
103800     .                                                                    
103900     EJECT                                                                
104000 GA-RADINFO  SECTION.                                                     
104100     MOVE 'GA-RADINFO'          TO WS-SEKTION                             
*                                                                               
104300     MOVE BGMT-IDDISTR          TO TEST-IDDISTR                           
104400     IF BILL-IDDC NOT = W-IDDC-B6                                         
104500       MOVE BILL-IDDC           TO W-IDDC-B6                              
104600                                   W-IDDC-WDK711                          
104700      PERFORM IMS-GU-WDB601                                               
104800     END-IF                                                               
104900*                                                                         
105000*--- VANLIGT FLÖDE FÖR INDIEN, CHINA, KOREA, TURKIET, MM                  
105100*    OCH USA (DET NYA FLÖDET)                                             
105200*    -EJ STUDS                                                            
105300*--- ÄVEN FÖR DISTR. NONVCC TILL VCC EJ DC.11 SOM BEHÖVER EN              
105400*    SPECIELL 'PAPPERS'FAKTURA UTAN TRANSAR TILLBAKA TILL W476XX          
105500*                                                                         
105600*--- (ORDINARY FLOWS FOR NONVCC DC'S WITH NO BOUNCE+USA NEW FLOW)         
105700*--- (EVEN TO CREATE A 'PAPPER'INVOICE NONVCC TO VCC NOT DC.11)           
105800*    (NO TRANSACTIONS BACK TO W476XX)                                     
105900*                                                                         
106000     IF (DCS-LAND-NON-VCC-OWNED AND SHIP-IDDC-EXP = SPACE)                
106100         OR                                                               
106200        (DIST35-NONVCC-VCC-REFILL   AND SHIP-KDFAKSTA-EXP = 3)            
106300         OR                                                               
106400        (DIST35-NONVCC-VCC-TRANSFER AND SHIP-KDFAKSTA-EXP = 3)            
106500         OR                                                               
106600        (DCS-USA   AND SHIP-IDDC-EXP = SPACE AND                          
106700         (DIST35-NDCUS-CDC-REFILL    OR                                   
106800          DIST35-NDCUS-JP-REFILL     OR                                   
106900          DIST35-NDCUS-AU-REFILL))                                        
             PERFORM GAA-RADINFO                                                
*                                                                               
107200     ELSE                                                                 
107300*---   STUDSFLÖDE FÖR:                                                    
107400*---   (BOUNCE FLOW FOR:)                                                 
107500*---   * GLOBAL EXP. (REFILL) FRÅN CHINA-> USA, USA -> CHINA, MM          
107600*---     (GLOBAL EXP. (REFILL) NONVCC TO NONVCC)                          
107700*                                                                         
107800*---   * VOR-FRÅN CDC TILL DEALER DISTR. INOM CN,KR,IN, MM                
107900*---     (VOR-FROM CDC TO DEALER DISTR. WITHIN NONVCC DC)                 
108000*                                                                         
108100*---   * LEV. FRÅN NDC(EJ FTG=57) -> IMPORTÖR                             
108200*---     (DEL. FROM NONVCC DC -> IMPORTER)                                
108300*---                                                                      
108400*---   I ALLA DESS FALL OVAN IDDC-EXP > SPACE                             
108500*---   (IN ALL THESE CASES ABOVE, THE IDDC-EXP > SPACE)                   
108600*                                                                         
108700       IF SHIP-IDDC-EXP > SPACE                                           
108800*                                                                         
108900*---     STUDSFLÖDE: FÖRSTA FAKTURAN (KDFAKSTA=1)                         
109000*---     (BOUNCE FLOW: THE FIRST INVOICE -> KDFAKSTA=1)                   
109100*                                                                         
109200         IF SHIP-KDFAKSTA-EXP = 1                                         
109300           IF SHIP-IDDC-EXP = WC-CDC-SE                                   
109400             PERFORM GAA-RADINFO                                          
109500           ELSE                                                           
109600*                                                                         
109700*---         OBS! GÄLLER BARA FÖR VOR FRÅN DC11                           
109800*---         OBS! (THIS CONDITION IS ONLY FOR VOR FROM DC 11)             
109900             IF SHIP-IDDC-EXP NOT = WC-CDC-SE                             
110000               PERFORM GAB-RADINFO                                        
110100             END-IF                                                       
110200           END-IF                                                         
110300*                                                                         
110400*---     STUDSFLÖDE:ANDRA FAKTURAN (KDFAKSTA=2)                           
110500*---     (BOUNCE FLOW: THE SECOND INVOICE -> KDFAKSTA=2)                  
110600*                                                                         
110700         ELSE                                                             
110800           IF SHIP-IDDC-EXP = WC-CDC-SE                                   
110900             PERFORM GAB-RADINFO                                          
111000*                                                                         
111100*---       OBS! GÄLLER BARA FÖR VOR FRÅN DC11                             
111200*---       OBS! (THIS CONDITION IS ONLY FOR VOR FROM DC 11)               
111300           ELSE                                                           
111400             IF SHIP-IDDC-EXP NOT = WC-CDC-SE                             
111500               MOVE SHIP-IDDC-EXP TO W-IDDC-B6                            
111600                                     W-IDDC-WDK711                        
111700               MOVE JA            TO VOR-CDC-11                           
111800               PERFORM IMS-GU-WDB601                                      
111900               PERFORM GAA-RADINFO                                        
112000             END-IF                                                       
112100           END-IF                                                         
112200         END-IF                                                           
112300*                                                                         
112400*---   VANLIGT FLÖDE FÖR ALLA ANDRA MARKNADER (EJ STUDS)                  
112500*---   (ORDINARY FLOW FOR ALL THE MARKETS THAT DON'T HAVE BOUNCE          
112600*---    AND THE IDFTG IS = 57)                                            
112700*                                                                         
112800       ELSE                                                               
               PERFORM GAB-RADINFO                                              
215500       END-IF                                                             
215600     END-IF                                                               
113200     .                                                                    
113300     EJECT                                                                
113400 GAA-RADINFO  SECTION.                                                    
113500     MOVE 'GAA-RADINFO'         TO WS-SEKTION                             
113600                                                                          
113700     MOVE 1                     TO UT-REQU-IDMSGVER                       
113800     MOVE SPACE                 TO UT-REQU-KDPGMACT                       
113900     MOVE 'W4063700'            TO UT-REQU-IDUSER                         
114000                                                                          
114100     MOVE DCS-IDLEGSEL          TO WF-IDLEGSEL                            
114200*                                                                         
114300     IF (DIST35-NONVCC-VCC-REFILL OR DIST35-NONVCC-VCC-TRANSFER)          
114400           AND                                                            
114500        SHIP-KDFAKSTA-EXP = '3'                                           
114600       MOVE 'VCCS'              TO WF-IDLEGSEL                            
114700     END-IF                                                               
114800*                                                                         
114900     MOVE BILL-IDSHIPM          TO WS-REDUIN                              
115000     PERFORM S05-NUM-TEXT                                                 
115100     MOVE WS-REDUUT             TO WF-IDBUNDLE                            
115200                                   WX-IDSHIPM                             
115300     MOVE BKOLLI-IDORDNR7       TO WS-REDUIN                              
115400     PERFORM S05-NUM-TEXT                                                 
115500     MOVE WS-REDUUT             TO WF-IDREF                               
115600                                   WX-IDORDNR                             
115700     MOVE WS-REDUUT             TO WF-IDSEQ(2)                            
115800                                                                          
115900     MOVE BGMT-IDKUNDNR         TO WS-NUM7                                
116000     MOVE WS-NUM7               TO WS-REDUIN                              
116100     PERFORM S05-NUM-TEXT                                                 
116200     MOVE WS-REDUUT             TO WX-IDKUNDNR                            
116300                                                                          
116400     MOVE BKOLLI-IDKOLLI        TO WS-NUM5                                
116500     MOVE WS-NUM5               TO WS-REDUIN                              
116600     PERFORM S05-NUM-TEXT                                                 
116700     MOVE WS-REDUUT             TO WX-IDKOLLI                             
116800                                                                          
116900     MOVE BKOLLI-IDPRODNR       TO WS-NUM7                                
117000     MOVE WS-NUM7               TO WS-REDUIN                              
117100     PERFORM S05-NUM-TEXT                                                 
117200     MOVE WS-REDUUT             TO WX-IDPRODNR                            
117300                                                                          
117400     MOVE KORD-TIORDREG         TO DAT-I-TIDATUM                          
117500                                   WS-AAMMDD                              
117600     MOVE 'AAMMDD'              TO DAT-KDDATFORM                          
117700                                                                          
117800     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
117900                     DAT-O-TIDATUM DAT-KDSVAR                             
118000                                                                          
118100     IF DAT-KDSVAR-OK                                                     
118200       MOVE DAT-TISEKEL         TO WS-SEKEL                               
118300     ELSE                                                                 
118400       MOVE 20                  TO WS-SEKEL                               
118500     END-IF                                                               
118600     MOVE WS-DATUM              TO WF-DAREFDAT                            
118700     IF DAGENS-DATUM < WS-AAMMDD                                          
118800     AND (DCS-AUSTRALIA OR DCS-JAPAN OR DCS-NDC-NA                        
118900     OR   DCS-LAND-NON-VCC-OWNED)                                         
119000       MOVE DAGENS-DATUM        TO WS-AAMMDD                              
119100       MOVE WS-DATUM            TO WF-DAREFDAT                            
119200     END-IF                                                               
119300*------------------------                                                 
119400     COMPUTE W-RAD-RAKNARE    = W-RAD-RAKNARE  +  1                       
119500     MOVE W-RAD-RAKNARE         TO WF-IDREFRAD                            
119600     MOVE BRAD-BERADREF         TO WF-BEVOLREF                            
119700     MOVE BGMT-IDPARTNR         TO WF-IDPARTNR                            
119800     PERFORM S04-DC-LAND-IDPARTNR                                         
119900**       (WF-IDLANDX3-SEND)                                               
120000                                                                          
120100**       (WF-IDLANDX3-REC)  = BLANK                                       
120200*                                                                         
120300     IF (DIST35-NONVCC-VCC-REFILL OR DIST35-NONVCC-VCC-TRANSFER)          
120400           AND                                                            
120500        SHIP-KDFAKSTA-EXP = '3'                                           
120600                                                                          
120700       IF DIST35-NDCCN-JP-REFILL OR                                       
120700          DIST35-NDCTH-JP-REFILL OR                                       
120800          DIST35-NONVCC-JP-TRANSFER                                       
120900         MOVE 'JP99999'         TO WF-IDPARTNR                            
121000       END-IF                                                             
121100       IF DIST35-NDCCN-AU-REFILL OR                                       
121100          DIST35-NDCTH-AU-REFILL OR                                       
121200          DIST35-NONVCC-AU-TRANSFER                                       
121300         MOVE 'AU99999'         TO WF-IDPARTNR                            
121400       END-IF                                                             
121500     END-IF                                                               
121600*                                                                         
121700     MOVE BGMT-IDDISTR          TO WS-NUM5                                
121800     MOVE WS-NUM5               TO WS-REDUIN                              
121900     PERFORM S05-NUM-TEXT                                                 
122000     MOVE WS-REDUUT             TO WF-IDEXCUST(1)                         
122100                                                                          
122200     MOVE BGMT-IDKUNDNR         TO WS-NUM7                                
122300     MOVE WS-NUM7               TO WS-REDUIN                              
122400     PERFORM S05-NUM-TEXT                                                 
122500     MOVE WS-REDUUT             TO WF-IDEXCUST(2)                         
122600     MOVE WS-REDUUT             TO WF-IDSEQ(1)                            
122700                                                                          
122800     MOVE BKOLLI-IDPRODNR       TO WS-NUM7                                
122900     MOVE WS-NUM7               TO WS-REDUIN                              
123000     PERFORM S05-NUM-TEXT                                                 
123100     MOVE WS-REDUUT             TO WF-IDOPTION(1)                         
123200                                                                          
123300     MOVE BKOLLI-IDKOLLI        TO WS-NUM5                                
123400     MOVE WS-NUM5               TO WS-REDUIN                              
123500     PERFORM S05-NUM-TEXT                                                 
123600     MOVE WS-REDUUT             TO WF-IDOPTION(2)                         
123700                                                                          
123800     MOVE BRAD-IDPURAD          TO WS-NUM5                                
123900                                   W-IDPURAD                              
124000                                   W-IDPURAD-BSEQ                         
124100     MOVE WS-NUM5               TO WS-REDUIN                              
124200     PERFORM S05-NUM-TEXT                                                 
124300     MOVE WS-REDUUT             TO WF-IDOPTION(3)                         
124400                                                                          
124500     MOVE BKOLLI-BEKUNDRF       TO WF-IDOPTION(5)                         
124600                                                                          
124700     MOVE BRAD-KDPRODSL         TO WF-IDAPPEND                            
124800     MOVE BRAD-IDARTNR          TO WS-NUM9                                
124900     MOVE WS-NUM9               TO WS-REDUIN                              
125000     PERFORM S05-NUM-TEXT                                                 
125100                                                                          
125200     MOVE WS-REDUUT             TO WF-IDARTNR-FINANCE                     
125300     MOVE WS-REDUUT             TO WF-IDSEQ(3)                            
125400                                                                          
125500     MOVE BRAD-IDSTATNR         TO WF-IDSTATNR                            
125600     IF BRAD-IDSTATNR = ZERO                                              
125700       MOVE GEN-IDSTATNR        TO WF-IDSTATNR                            
125800     END-IF                                                               
125900     MOVE BRAD-VKARTNTO         TO WF-VKARTNTO                            
126000                                                                          
126100     MOVE BILL-IDDC             TO WS-IDDC                                
126200     MOVE 'XX'                  TO WF-KDVAT                               
126300     IF NDC-TH                                                            
126400       MOVE 'XZ'                TO WF-KDVAT                               
126500     END-IF                                                               
126600*** FOR VOR 2ND INVOICE AND IF THAILAND, USE XZ VAT CODE                  
126700     IF SHIP-KDFAKSTA-EXP = '2'                                           
126800       IF SHIP-IDDC-EXP NOT = WC-CDC-SE                                   
126900         MOVE BILL-IDDC-EXP     TO WS-IDDC                                
127000         IF NDC-TH                                                        
127100           MOVE 'XZ'            TO WF-KDVAT                               
127200         END-IF                                                           
127300         MOVE BILL-IDDC         TO WS-IDDC                                
127400       END-IF                                                             
127500     END-IF                                                               
127600***                                                                       
127700                                                                          
127800     MOVE BILL-KDFINDOC         TO WF-KDFINDOC                            
127900     MOVE BGMT-IDPARTNR         TO WS-IDPARTNR                            
128000     IF BILL-KDFINDOC NOT = 'PROF'                                        
128100       IF WS-INT NOT NUMERIC                                              
128200          MOVE 'INT '           TO WF-KDFINDOC                            
128300       ELSE                                                               
128400          IF BILL-KDFINDOC = 'INT '                                       
128500            MOVE 'INV '         TO WF-KDFINDOC                            
128600          END-IF                                                          
128700       END-IF                                                             
128800     END-IF                                                               
128900*                                                                         
129000     IF (DIST35-NONVCC-VCC-REFILL OR DIST35-NONVCC-VCC-TRANSFER)          
129100           AND                                                            
129200        SHIP-KDFAKSTA-EXP = '3'                                           
129300       MOVE 'INT '              TO WF-KDFINDOC                            
129400     END-IF                                                               
129500*                                                                         
129600*    FOR CHINA                                                            
129700*    AND INDIA                                                            
129800*    AND KOREA                                                            
129900*    AND TURKIET                                                          
130000*    AND USA (NEW FLOW)                                                   
130100*    AND NDC (FTG NOT 57) TO IMPORTERS                                    
130200*    - THE FOLLOWING FINANCIAL DOCUMENTS HAVE AVERAGE COST                
130300*      . KDFAKTYP = N                                                     
130400*      . DEALER DISTRICTS                                                 
130500*      . TRANSFER DISTRICTS                                               
130600*      . BOUNCE FLOW WHEN CDC IS WHERE IT BOUNCE                          
130700     IF BKOLLI-KDFAKTYP = 'N'         OR                                  
130800        DIST07-NON-VCC-OWNED          OR                                  
130900        DIST35-CN-TRANSFER            OR                                  
131000        DIST35-CN-NDC-RETURNS         OR                                  
131100        DIST35-CN-CDC-RETURNS         OR                                  
131200        DIST35-REFILL-INOM-NONVCC-NDC OR                                  
131300        DIST35-CDC-RETURNS-NON-VCC    OR                                  
131400        DIST35-NONVCC-REFILL          OR                                  
131500        DIST35-NONVCC-VCC-TRANSFER    OR                                  
131600        DIST35-NONVCC-NONVCC-TRANSFER OR                                  
131700        DIS134-BYTESREN-CN            OR                                  
131800        XDC-NON-VCC-OWNED             OR                                  
131900        NDC-US                                                            
132000       MOVE BRAD-IDARTNR      TO W-IDARTNR-WDK701                         
132100       MOVE BILL-IDDC         TO W-IDDC-WDK711                            
132200       IF VOR-OK                                                          
132300         MOVE BILL-IDDC-EXP   TO W-IDDC-WDK711                            
132400       END-IF                                                             
132500       PERFORM IMS-GU-WDK701                                              
132600       PERFORM IMS-GNP-WDK711                                             
132700       MOVE SLAG-PRAVCOST     TO WF-PRARTNTO                              
132800                                 WF-PRARTBTO                              
132900       MOVE 04                TO WF-KDPRMOD                               
133000*                                                                         
133100***    TA FRAM TRACKING NO. FÖR DUBAI (TRACKING NO. FOR DUBAI)            
133200***    TA FRAM PIEDIMENTO NO. FÖR MEXICO (TRACKING NO. FOR MEXICO)        
133300*                                                                         
133400       IF TRACK-SKRIV AND                                                 
133500          (NDC-AE OR NDC-MX)                                              
133600*                                                                         
133700         MOVE BRAD-IDARTNR    TO LOGT-IDARTNR                             
133800         MOVE SLAG-KVLS       TO LOGT-KVLS                                
133900*                                                                         
134000*--      RESTEN AV NYCKLAR TILL WDE1                                      
134100         MOVE BKOLLI-IDDISTR  TO W-IDDISTR-E1                             
134200         MOVE BKOLLI-IDKUNDNR TO W-IDKUNDNR-E1                            
134300         MOVE BKOLLI-IDPRODNR TO W-IDPRODNR-E1                            
134400         MOVE BKOLLI-IDKOLLI  TO W-IDKOLLI-E1                             
134500         MOVE BRAD-IDPURAD    TO W-IDPURAD                                
134600*                                                                         
134700*--      LÄS TRACKINGSEG. WDK728                                          
134800*        MEN LÄS FÖRBI SEGMENT MED NOLL I TRCK-KVTRACK-KVAR               
134900*                                                                         
135000         PERFORM IMS-GHNP-WDK728                                          
135100         PERFORM UNTIL SEGMENT-SAKNAS OR TRCK-KVTRACK-KVAR > ZERO         
135200           PERFORM IMS-GHNP-WDK728                                        
135300         END-PERFORM                                                      
135400*                                                                         
135500*        KOLLA OM ANTAL PÅ WDK728 ÄR < = > ÄN KVLEVART                    
135600*                                                                         
135700         MOVE BRAD-KVLEVART   TO WS-KVAR                                  
135800         MOVE +1              TO TRACK-IX                                 
135900                                                                          
136000         PERFORM UNTIL SEGMENT-SAKNAS                                     
136100*                                                                         
136200           IF WS-KVAR <= TRCK-KVTRACK-KVAR                                
136300                                                                          
136400             SUBTRACT WS-KVAR FROM TRCK-KVTRACK-KVAR                      
136500             MOVE WS-KVAR           TO WS-KVTRACK                         
136600                                       TLEV-KVTRACK-LEV                   
136700                                       LOGT-KVART-SALDO                   
136800             MOVE TRCK-IDTRACK      TO WS-IDTRACK                         
136900                                       TLEV-IDTRACK                       
137000             MOVE TRCK-DAINLEV      TO WS-DAINLEV                         
137100             MOVE WS-DADATUM        TO TLEV-DADATUM                       
137200                                                                          
137300             PERFORM IMS-REPL-WDK728                                      
137400*                                                                         
137500             PERFORM IMS-ISRT-WDE141                                      
137600                                                                          
137700             PERFORM GAAC-SKAPA-SALDOLOGG                                 
137800                                                                          
137900*-- GE TILL STATUS-CODE FÖR ATT BRYTA LOOPEN PÅ K728                      
138000             MOVE 'GE'              TO STATUS-WS                          
138100           ELSE                                                           
138200*                                                                         
138300             COMPUTE WS-KVAR = WS-KVAR - TRCK-KVTRACK-KVAR                
138400                                                                          
138500             MOVE TRCK-KVTRACK-KVAR TO WS-KVTRACK                         
138600                                       TLEV-KVTRACK-LEV                   
138700                                       LOGT-KVART-SALDO                   
138800             MOVE TRCK-IDTRACK      TO WS-IDTRACK                         
138900                                       TLEV-IDTRACK                       
139000             MOVE ZERO              TO TRCK-KVTRACK-KVAR                  
139100             MOVE TRCK-DAINLEV      TO WS-DAINLEV                         
139200             MOVE WS-DADATUM        TO TLEV-DADATUM                       
139300             PERFORM IMS-REPL-WDK728                                      
139400*                                                                         
139500             PERFORM IMS-ISRT-WDE141                                      
139600                                                                          
139700             PERFORM GAAC-SKAPA-SALDOLOGG                                 
139800*                                                                         
139900             PERFORM IMS-GHNP-WDK728                                      
140000           END-IF                                                         
140100                                                                          
140200**         BILLIT LAGRAR BARA 5 FÖREKOMSTER AV IDTRACK                    
140300           IF TRACK-IX <= MAX-TRACK-IX                                    
140400             MOVE WS-KVTRACK  TO WF-KVTRACK-LEV (TRACK-IX)                
140500             MOVE WS-IDTRACK  TO WF-IDTRACK(TRACK-IX)                     
140600           END-IF                                                         
140700           ADD +1             TO TRACK-IX                                 
140800                                                                          
140900         END-PERFORM                                                      
141000                                                                          
141100         PERFORM UNTIL TRACK-IX > MAX-TRACK-IX                            
141200           MOVE ZERO        TO WF-KVTRACK-LEV (TRACK-IX)                  
141300           MOVE SPACE       TO WF-IDTRACK     (TRACK-IX)                  
141400           ADD +1           TO TRACK-IX                                   
141500         END-PERFORM                                                      
141600                                                                          
141700       ELSE                                                               
141800         MOVE +1              TO TRACK-IX                                 
141900         PERFORM UNTIL TRACK-IX > MAX-TRACK-IX                            
142000           MOVE ZERO          TO WF-KVTRACK-LEV (TRACK-IX)                
142100           MOVE SPACE         TO WF-IDTRACK     (TRACK-IX)                
142200           ADD +1             TO TRACK-IX                                 
142300         END-PERFORM                                                      
142400       END-IF                                                             
142500     ELSE                                                                 
142600       MOVE BRAD-PRARTNTO     TO WF-PRARTNTO                              
142700                                 WF-PRARTBTO                              
142800       MOVE 01                TO WF-KDPRMOD                               
142900     END-IF                                                               
143000                                                                          
143100     IF WF-PRARTBTO = ZERO                                                
143200       MOVE ZERO              TO WF-REARTRAB                              
143300     ELSE                                                                 
143400       MOVE BRAD-RERAB        TO WF-REARTRAB                              
143500     END-IF                                                               
143600     MOVE BRAD-KVBEART          TO WF-KVBEART                             
143700     MOVE BRAD-KVLEVART         TO WF-KVLEVART                            
143800     MOVE SPACE                 TO WF-BEART                               
143900     IF BRAD-BEART-VIPS NOT = SPACE                                       
144000       MOVE BRAD-BEART-VIPS     TO WF-BEART                               
144100     END-IF                                                               
144200     IF WF-BEART = SPACE                                                  
144300       PERFORM GAAA-HAMTA-ARTTEXT                                         
144400     END-IF                                                               
144500     IF WF-BEART = SPACE                                                  
144600       MOVE 'PART DESCRIPTION MISSING'  TO WF-BEART                       
144700     END-IF                                                               
144800     IF BRAD-KDSOFT > 0                                                   
144900       MOVE JA                  TO WF-FLSOFT                              
145000     ELSE                                                                 
145100       MOVE NEJ                 TO WF-FLSOFT                              
145200     END-IF                                                               
145300*                                                                         
145400     IF WF-FLSOFT = JA                                                    
145500       PERFORM IMS-GNP-WDE411                                             
145600       IF SEGMENT-FINNS                                                   
145700         MOVE WF-IDARTNR-FINANCE  TO WS-IDARTNR-SOFTWARE                  
145800         MOVE ORAD-IDARBREF       TO WS-IDARBREF                          
145900         MOVE ORAD-IDBIL          TO WS-IDBIL                             
146000         MOVE ORAD-IDVIN          TO WS-IDVIN                             
146100         MOVE WS-IDARTNR-SOFTWARE TO WF-IDARTNR-FINANCE                   
146200       END-IF                                                             
146300     END-IF                                                               
146400*                                                                         
146500*--     CHECK IF LYNK --> GET THE LYNK PART NO. AND                       
146600*                         SEND IT TO BILLIT                               
146700*      !BUT REFILL DISTRICT SHOULD NOT HAVE                               
146800*           THE LYNK PART NO.(15/5 '25)                                   
146900*                                                                         
147000     MOVE BRAD-KDPRODSL           TO TEST-KDPRODSL                        
147100                                                                          
147200     IF (SHIP-IDSYSTEM = 'LYNK' OR KDPRODSL-LYNK)                         
147300        AND NOT DIST35-REFILL                                             
147400*                                                                         
147500       MOVE BRAD-IDARTNR      TO W-IDARTNR-F5                             
147600       PERFORM IMS-GU-WDF502                                              
147700       IF SEGMENT-FINNS                                                   
147800         MOVE WF-IDARTNR-FINANCE TO WS-IDARTNR-LYNK                       
147900         MOVE XLEV-IDLEVART      TO WS-IDARTNR-L                          
148000         MOVE WS-IDARTNR-LYNK    TO WF-IDARTNR-FINANCE                    
148100       END-IF                                                             
148200                                                                          
148300       MOVE JA                   TO LYNK-DISTR-SW                         
148400     END-IF                                                               
148500*                                                                         
148600     IF BRAD-KDPRTYP = 'T'                                                
148700       MOVE JA                  TO WF-FLSPECPR                            
148800     ELSE                                                                 
148900       MOVE NEJ                 TO WF-FLSPECPR                            
149000     END-IF                                                               
149100     PERFORM S20-HAMTA-FLPCOO                                             
149200                                                                          
149300***** POLESTAR SHOULD BE SHOWN ON INVOICE                                 
149400     IF CLAG-IDPROJUP = POLESTAR                                          
149500       MOVE 'POLESTAR'          TO WF-IDEXCUST(3)                         
149600     ELSE                                                                 
149700       MOVE SPACE               TO WF-IDEXCUST(3)                         
149800     END-IF                                                               
149900                                                                          
150000     IF BKOLLI-KDFAKTYP = 'G'                                             
150100       MOVE JA                  TO WF-FLFREE                              
150200     ELSE                                                                 
150300       MOVE NEJ                 TO WF-FLFREE                              
150400     END-IF                                                               
150500     MOVE NEJ                   TO WF-FLPRIV                              
150600     MOVE 'SEK'                 TO WF-KDVALISO                            
150700     IF BGMT-FLCOD = JA         OR                                        
150800        BILL-KDFINDOC = 'PROF'  OR                                        
150900        DCS-DDC                 OR                                        
151000        DIST35-NONVCC-NONVCC-REFILL                                       
151100                                OR                                        
151200        DIST35-NONVCC-NONVCC-TRANSFER                                     
151300                                OR                                        
151400        XDC-NON-VCC-OWNED       OR                                        
151500        NDC-US                                                            
151600       MOVE 'NOW '              TO WF-KDINVFRQ                            
151700       IF BRAD-KDSOFT > 0                                                 
151800*             SOFTWARE                                                    
151900         MOVE 'DAY'             TO WF-KDINVFRQ                            
152000         IF DIST38-PER-FRQ                                                
152100           MOVE 'PER '          TO WF-KDINVFRQ                            
152200         END-IF                                                           
152300       END-IF                                                             
152400     ELSE                                                                 
152500       MOVE '    '              TO WF-KDINVFRQ                            
152600     END-IF                                                               
152700***** EXTENDED WARRANTY SHOULD BE INVOICED DAYLY                          
152800     MOVE ART-IDFKNGRP TO TEST-IDFKNGRP                                   
152900     IF FKNGRP-EXT-WARRANTY OR FKNGRP-VSA-IMP                             
153000       MOVE 'DAY'               TO WF-KDINVFRQ                            
153100     END-IF                                                               
153200     MOVE BGMT-IDPARTNR         TO WS-IDPARTNR                            
153300     MOVE SPACE                 TO WF-IDBREAK(1)                          
153400     IF BKOLLI-FLOVRLEV = JA  OR                                          
153500        BKOLLI-KDFAKTYP = 'N' OR                                          
153600        DIST18-SKROT          OR                                          
153700        DIST18-SCRAP-NDC                                                  
153800       MOVE BKOLLI-IDORDNR7     TO WS-REDUIN                              
153900       PERFORM S05-NUM-TEXT                                               
154000       MOVE WS-REDUUT           TO WF-IDBREAK(1)                          
154100                                                                          
154200     END-IF                                                               
154300     IF BGMT-FLSEPINV = JA   OR                                           
154400        NOLL-BGMT-FLSEPINV = JA                                           
154500        MOVE WF-IDBUNDLE         TO WF-IDBREAK(1)                         
154600     END-IF                                                               
154700     IF BGMT-FLCOD = JA                                                   
154800       MOVE SPACE                TO WF-IDBREAK(1)                         
154900       MOVE BGMT-IDKUNDNR        TO WS-NUM7                               
155000       MOVE WS-NUM7              TO WS-REDUIN                             
155100       PERFORM S05-NUM-TEXT                                               
155200       MOVE WS-REDUUT            TO WF-IDBREAK(1)                         
155300     END-IF                                                               
155400     MOVE SPACE                  TO WF-IDLEVNR                            
155500     IF BILL-IDLEVNR  = '00000' OR SPACE                                  
155600       MOVE BILL-IDSHIPM         TO WS-REDUIN                             
155700       PERFORM S05-NUM-TEXT                                               
155800       MOVE WS-REDUUT            TO WF-IDBREAK(1)                         
155900       IF BRAD-KDSOFT > 0                                                 
156000         MOVE SPACE              TO WF-IDBREAK(1)                         
156100       END-IF                                                             
156200*                                                                         
156300       IF BGMT-FLCOD = JA                                                 
156400        MOVE WF-IDBREAK(1)       TO WS-REDUUT                             
156500        MOVE 'C'                 TO WS-REDUUT(8:1)                        
156600        MOVE WS-REDUUT           TO WF-IDBREAK(1)                         
156700       END-IF                                                             
156800     ELSE                                                                 
156900       MOVE KOLLI-IDLEVNR        TO WF-IDLEVNR                            
157000       IF DCS-DDC AND                                                     
157100        ((DCS-NORWAY   AND NOT DIST03-NORGE) OR                           
157200         (DCS-BELGIUM AND DIST42-EJ-EU)      OR                           
157300         (DCS-GERMANY AND DIST42-EJ-EU)      OR                           
157400         (DCS-SWEDEN   AND DIST42-EJ-EU))                                 
157500***       (DDC-SE AND KDVIA NOT = '01' AND DIST42-EJ-EU)                  
157600***       MÅSTE ÄNDRAS TILL USA                                           
157700          MOVE SPACE                TO WF-IDBREAK(1)                      
157800          IF KOLLI-KDVIA = '01'                                           
157900            MOVE BILL-IDSHIPM       TO WS-REDUIN                          
158000            PERFORM S05-NUM-TEXT                                          
158100            MOVE WS-REDUUT          TO WF-IDBREAK(1)                      
158200            MOVE '00000'            TO WF-IDLEVNR                         
158300*          OM KDVIA = '01' SKA IDLEVNR VARA BLANK/00000                   
158400*          ANNARS SKICKAS MAIL TILL DIREKTLEVERANTÖREN I BILL-IT          
158500          ELSE                                                            
158600            IF BGMT-FLSEPINV = JA                                         
158700              MOVE WF-IDBUNDLE      TO WF-IDBREAK(1)                      
158800            ELSE                                                          
158900              MOVE BKOLLI-IDORDNR7  TO WS-REDUIN                          
159000              PERFORM S05-NUM-TEXT                                        
159100              MOVE WS-REDUUT        TO WF-IDBREAK(1)                      
159200            END-IF                                                        
159300          END-IF                                                          
159400        END-IF                                                            
159500        IF DCS-DDC                                                        
159600          CONTINUE                                                        
159700        ELSE                                                              
159800           MOVE '00000'             TO WF-IDLEVNR                         
159900        END-IF                                                            
160000     END-IF                                                               
160100     IF DIST38-FAKTURA-PER-SKEPPNING AND                                  
160200        BRAD-KDSOFT = 0                                                   
160300       MOVE BILL-IDSHIPM            TO WS-REDUIN                          
160400       PERFORM S05-NUM-TEXT                                               
160500       MOVE WS-REDUUT               TO WF-IDBREAK(1)                      
160600     END-IF                                                               
160700                                                                          
160800     IF ((SHIP-IDSYSTEM = 'LYNK' OR LYNK-DISTR)                           
160900        AND                                                               
161000        BRAD-KDSOFT = 0)                                                  
161100        AND NOT DIST35-REFILL                                             
161200*                                                                         
161300       MOVE BILL-IDSHIPM            TO WS-REDUIN                          
161400       PERFORM S05-NUM-TEXT                                               
161500       MOVE WS-REDUUT               TO WF-IDBREAK(1)                      
161600       MOVE NEJ                     TO LYNK-DISTR-SW                      
161700     END-IF                                                               
161800     IF DIST38-PER-FRQ AND                                                
161900        BRAD-KDSOFT > 0                                                   
162000       MOVE SPACE                   TO WF-IDBREAK(1)                      
162100       MOVE BGMT-IDKUNDNR           TO WS-NUM7                            
162200       MOVE WS-NUM7                 TO WS-REDUIN                          
162300       PERFORM S05-NUM-TEXT                                               
162400       MOVE WS-REDUUT               TO WF-IDBREAK(1)                      
162500     END-IF                                                               
162600     IF BGMT-FLCOD = JA                                                   
162700       MOVE WF-IDBREAK(1)       TO WS-REDUUT                              
162800       MOVE 'C'                 TO WS-REDUUT(8:1)                         
162900       MOVE WS-REDUUT           TO WF-IDBREAK(1)                          
163000     END-IF                                                               
163100     MOVE ART-IDFKNGRP TO TEST-IDFKNGRP                                   
163200     IF FKNGRP-VSA-IMP                                                    
163300       MOVE WF-IDBREAK(1)          TO WS-REDUUT                           
163400       MOVE 'SERVVSA'              TO WS-REDUUT(1:7)                      
163500       MOVE WS-REDUUT              TO WF-IDBREAK(1)                       
163600     END-IF                                                               
163700     MOVE SPACE                 TO WF-IDBREAK(2)                          
163800     MOVE BGMT-IDDISTR          TO WS-NUM5                                
163900     MOVE WS-NUM5               TO WS-REDUIN                              
164000     MOVE BILL-IDDC             TO WS-REDUIN(6:2)                         
164100     MOVE WS-REDUIN             TO WF-IDBREAK(2)                          
164200**** EXTENDED WARRANTY SHOULD BE ON OWN INVOICE                           
164300**** AND PER SHIPMENT                                                     
164400**** NEEDS TO BE AFTER THE DEFAULT SETTING OF BREAK(2)                    
164500     IF FKNGRP-EXT-WARRANTY                                               
164600       MOVE WF-IDBREAK(1)          TO WS-REDUUT                           
164700       MOVE 'SERVEXT'              TO WS-REDUUT(1:7)                      
164800       MOVE WS-REDUUT              TO WF-IDBREAK(1)                       
164900       MOVE BILL-IDSHIPM           TO WS-REDUIN                           
165000       PERFORM S05-NUM-TEXT                                               
165100       MOVE WS-REDUUT              TO WF-IDBREAK(2)                       
165200     END-IF                                                               
165300**** GROUP AS PER CUSTOMER NUMBERS                                        
165400     IF BGMT-IDPARTNR = 33186                                             
165500     OR BGMT-IDPARTNR = 27823                                             
165600     OR BGMT-IDPARTNR = 428324                                            
165700       MOVE SPACE               TO WF-IDBREAK(1)                          
165800                                   WF-IDBREAK(2)                          
165900       MOVE BGMT-IDPARTNR       TO WF-IDBREAK(1)                          
166000       MOVE BKOLLI-BEKUNDRF(1:8)   TO WF-IDBREAK(2)                       
166100       IF BKOLLI-BEKUNDRF(9:1) NOT = SPACE                                
166200         MOVE BKOLLI-BEKUNDRF(9:7) TO WF-IDBREAK(1)                       
166300       END-IF                                                             
166400     END-IF                                                               
166500**** GROUP AS PER ORDER NUMBER                                            
166600     IF BGMT-IDPARTNR = 1618                                              
166700     OR BGMT-IDPARTNR = 1680                                              
166800       MOVE SPACE               TO WF-IDBREAK(1)                          
166900                                   WF-IDBREAK(2)                          
167000       MOVE BKOLLI-IDORDNR7     TO WS-REDUIN                              
167100       PERFORM S05-NUM-TEXT                                               
167200       MOVE WS-REDUUT           TO WF-IDBREAK(1)                          
167300     END-IF                                                               
167400                                                                          
167500     MOVE BRAD-KDARTURS         TO WF-KDARTURS                            
167600     MOVE BILL-IDDC             TO WF-IDDC                                
167700     MOVE KORD-KDFRAKT          TO WF-KDFRAKT                             
167800     MOVE SPACES                TO WF-BELEVVIL                            
167900     PERFORM S06-HAEMTA-LEVVIL-TEXT                                       
168000*                                                                         
168100*                                                                         
168200*    SPECIAL DEL.TERMS FOR GLOBAL EXP - CN TO KR                          
168300*    FIRST INVOICE                                                        
168400     IF DIST41-DEL-TERMS-GE                                               
168500       MOVE 'FCA     '          TO WF-BELEVVIL                            
168600     END-IF                                                               
168700                                                                          
168800     MOVE WF-BELEVVIL           TO WS-BELEVVIL                            
168900                                                                          
169000*                                                                         
169100*    NO NEED TO SAVE THIS BELEVVIL AS IT IS JUST FOR A                    
169200*    'PAPPER'-VERSION                                                     
169300     IF (DIST35-NONVCC-VCC-REFILL OR DIST35-NONVCC-VCC-TRANSFER)          
169400           AND                                                            
169500        SHIP-KDFAKSTA-EXP = '3'                                           
169600       MOVE 'CIP '              TO WF-BELEVVIL                            
169700     END-IF                                                               
169800*                                                                         
169900*                                                                         
170000     MOVE BRAD-IDKONTO          TO WS-NUM11                               
170100     MOVE WS-NUM11              TO WS-REDUIN                              
170200     PERFORM S05-NUM-TEXT                                                 
170300     MOVE WS-REDUUT             TO WF-IDACCNT(1)                          
170400                                                                          
170500     MOVE BRAD-IDANALYS         TO WF-IDACCNT(2)                          
170600     MOVE BRAD-IDKST            TO WS-REDUIN                              
170700     PERFORM S05-NUM-TEXT                                                 
170800     MOVE WS-REDUUT             TO WF-IDACCNT(3)                          
170900     MOVE SPACE                 TO WF-BEANST                              
171000                                   WF-IDUSER                              
171100                                   WF-BETEXT                              
171200     IF BGMT-IDDISTR = 9211                                               
171300*                                                                         
171400*      SPECIAL GODSMOTTAGARE.ADRESS TILL USA -> SVERIGE (REFILL)          
171500*                                                                         
171600       IF BGMT-IDDISTR = GMT-IDDISTR AND                                  
171700          BGMT-IDKUNDNR = GMT-IDKUNDNR                                    
171800         CONTINUE                                                         
171900       ELSE                                                               
172000         MOVE BGMT-IDDISTR      TO W-IDDISTR-WDB2                         
172100         MOVE BGMT-IDKUNDNR     TO W-IDKUNDNR-WDB2                        
172200         PERFORM IMS-GU-WDB201                                            
172300       END-IF                                                             
172400       MOVE SPACES              TO W-BETEXT                               
172500                                   Y-BETEXT                               
172600                                   Z-BETEXT                               
172700       MOVE GMT-BEGMT-RAD1      TO W-RAD1                                 
172800                                   Y-RAD1                                 
172900       MOVE GMT-BEGMT-RAD2      TO W-RAD2                                 
173000                                   Y-RAD2                                 
173100       MOVE GMT-ADGMT-GATA      TO W-GATA                                 
173200                                   Y-GATA                                 
173300       MOVE GMT-ADGMT-PADR      TO W-PADR                                 
173400                                   Y-PADR                                 
173500       MOVE GMT-ADGMT-LAND      TO W-LAND                                 
173600                                   Y-LAND                                 
173700*       ÄR E401 INLÄST ?                                                  
173800       MOVE KORD-IDORDER        TO W-IDORDER                              
173900       PERFORM IMS-GU-WDQ201                                              
174000       IF SEGMENT-FINNS                                                   
174100         MOVE OHUV-BEGMT-RAD1   TO W-RAD1                                 
174200         MOVE OHUV-BEGMT-RAD2   TO W-RAD2                                 
174300         MOVE OHUV-ADGMT-GATA   TO W-GATA                                 
174400         MOVE OHUV-ADGMT-PADR   TO W-PADR                                 
174500         MOVE OHUV-ADGMT-LAND   TO W-LAND                                 
174600       END-IF                                                             
174700       MOVE W-BETEXT            TO WF-BETEXT                              
174800     END-IF                                                               
174900     MOVE BRAD-IDLEVNR-ART      TO WF-IDLEVNR-ART                         
175000                                                                          
175100*** ECOM CUSTOMER                                                         
175200*    IF GMT-KDKUNDKAT = 18                                                
175300*                                                                         
175400     IF DIST79-ECOM-PRICE                                                 
175500       MOVE 'ECOM'              TO WF-IDSYSTEM-SEND                       
175600       MOVE 'W476'              TO WF-IDSYSTEM-REC                        
175700     ELSE                                                                 
175800       MOVE 'W476'              TO WF-IDSYSTEM-SEND                       
175900                                   WF-IDSYSTEM-REC                        
176000     END-IF                                                               
176100*                                                                         
176200     IF (DIST35-NONVCC-VCC-REFILL OR DIST35-NONVCC-VCC-TRANSFER)          
176300           AND                                                            
176400        SHIP-KDFAKSTA-EXP = '3'                                           
176500       MOVE 'W476'              TO WF-IDSYSTEM-SEND                       
176600       MOVE SPACE               TO WF-IDSYSTEM-REC                        
176700     END-IF                                                               
176800*                                                                         
176900*    FOR CHINA                                                            
177000*    AND INDIA                                                            
177100*    AND KOREA                                                            
177200*    AND TURKIET                                                          
177300*    AND OTHER NONVCC DC'S                                                
177400*    AND USA (NEW BOUNCE FLOW)                                            
177500*    - THE FOLLOWING FINANCIAL DOCUMENTS HAVE CNY/INR/ETC...              
177600*      . KDFAKTYP = N IS THE SAME DISTRICT                                
177700*      . DEALER DISTRICTS                                                 
177800*      . TRANSFER DISTRICTS                                               
177900*      . NDC (FTG NOT 57) TO IMPORTERS                                    
178000     IF BKOLLI-KDFAKTYP = 'N'         OR                                  
178100        DIST07-NON-VCC-OWNED          OR                                  
178200        DIST35-CN-TRANSFER            OR                                  
178300        DIST35-CN-NDC-RETURNS         OR                                  
178400        DIST35-CN-CDC-RETURNS         OR                                  
178500        DIST35-REFILL-INOM-NONVCC-NDC OR                                  
178600        DIST35-CDC-RETURNS-NON-VCC    OR                                  
178700        DIST35-NONVCC-REFILL          OR                                  
178800        DIST35-NONVCC-VCC-TRANSFER    OR                                  
178900        DIST35-NONVCC-NONVCC-TRANSFER OR                                  
179000        DIS134-BYTESREN-CN            OR                                  
179100        XDC-NON-VCC-OWNED             OR                                  
179200        NDC-US                                                            
179300                                                                          
179400       IF BKOLLI-KDFAKTYP = 'N'  OR                                       
179500          DCS-LAND-NON-VCC-OWNED OR                                       
179600          NDC-US                 OR                                       
179700          DIST35-CDC-RETURNS-NON-VCC                                      
179800         MOVE DCS-KDVALISO        TO WF-KDVALISO                          
179900       END-IF                                                             
180000*                                                                         
180100       MOVE BILL-IDDC             TO WS-IDDC                              
180200*                                                                         
180300       IF DIST35-CN-TRANSFER         OR                                   
180400          DIST35-CN-NDC-RETURNS      OR                                   
180500          DIST35-REFILL-INOM-CN      OR                                   
180600          DIST35-NDCCN-CDC-REFILL    OR                                   
180700          DIST35-NDCCN-JP-REFILL     OR                                   
180800          DIST35-NDCCN-AU-REFILL     OR                                   
180900          DIST35-NDCCN-NONVCC-REFILL OR                                   
181000          DIST07-KINA                OR                                   
181100          DIS134-BYTESREN-CN                                              
181200         MOVE 'CNY'             TO WF-KDVALISO                            
181300       END-IF                                                             
181400                                                                          
181500       IF DIST07-INDIEN                                                   
181600         MOVE 'INR'             TO WF-KDVALISO                            
181700       END-IF                                                             
181800                                                                          
181900       IF DIST07-KOREA                                                    
182000         MOVE 'KRW'             TO WF-KDVALISO                            
182100       END-IF                                                             
182200                                                                          
182300       IF DIST07-TURKEY                                                   
182400         MOVE 'TRY'             TO WF-KDVALISO                            
182500       END-IF                                                             
182600                                                                          
182700       IF DIST07-MALAYSIA OR                                              
182800          NDC-MY                                                          
182900         MOVE 'MYR'             TO WF-KDVALISO                            
183000       END-IF                                                             
183100                                                                          
183200       IF DIST07-THAILAND            OR                                   
183300          DIST35-REFILL-INOM-TH      OR                                   
                DIST35-NDCTH-JP-REFILL     OR                                   
                DIST35-NDCTH-AU-REFILL     OR                                   
183400          NDC-TH                                                          
183500         MOVE 'THB'             TO WF-KDVALISO                            
183600       END-IF                                                             
183700                                                                          
183800       IF DIST07-TAIWAN         OR                                        
183900          NDC-TW                                                          
184000         MOVE 'TWD'             TO WF-KDVALISO                            
184100       END-IF                                                             
184200                                                                          
184300       IF DIST07-MEXICO                                                   
184400         MOVE 'MXN'             TO WF-KDVALISO                            
184500       END-IF                                                             
184510                                                                          
184511       IF DIST07-BRAZIL                                                   
184512         MOVE 'BRL'             TO WF-KDVALISO                            
184513       END-IF                                                             
184514                                                                          
184520       IF DIST07-S-AFRICA                                                 
184530         MOVE 'ZAR'             TO WF-KDVALISO                            
184540       END-IF                                                             
184600*                                                                         
184700       IF DIST35-NDCUS-NONVCC-REFILL OR                                   
184800          DIST35-NDCUS-CDC-REFILL    OR                                   
184900          DIST35-NDCUS-JP-REFILL     OR                                   
185000          DIST35-NDCUS-AU-REFILL                                          
185100         MOVE 'USD'             TO WF-KDVALISO                            
185200       END-IF                                                             
185300                                                                          
185400**** RETURER FRÅN KINA/INDIA/KOREA/TURKIET TILL CDC                       
185500**** SKALL SENARE KREDITERAS I VENDOR PRICE                               
185600       IF DIST35-CDC-RETURNS-NON-VCC                                      
185700         CONTINUE                                                         
185800       ELSE                                                               
185900**** BEHÖVER SPARA AKTUELLT AVCOST SOM VI HÄMTAT                          
186000         PERFORM GAAE-SPARA-AVCOST-WDE411                                 
186100       END-IF                                                             
186200     END-IF                                                               
186300*                                                                         
186400*--- 1:A FAKTURANR-ET TILL BILLIT --> VOR FRÅN DC11                       
186500*--- (1:ST INVOICE TO BILLIT --> VOR FROM DC11)                           
186600     IF SHIP-IDDC-EXP > SPACE                                             
186700       IF DIST35-NONVCC-NONVCC-REFILL                                     
186800                                OR                                        
186900          DIST35-NONVCC-NONVCC-TRANSFER                                   
187000         CONTINUE                                                         
187100       ELSE                                                               
187200         IF VOR-OK                                                        
187300           MOVE SHIP-IDDC-EXP   TO WF-IDDC                                
187400           MOVE KOLLI-IDFAKT    TO WF-IDFAKREF                            
187500         END-IF                                                           
187600       END-IF                                                             
187700     END-IF                                                               
187800*                                                                         
187900*--- 1:A FAKTURANR-ET TILL BILLIT --> TILLÄGGSFAKT. FÖR PROCESSEN         
188000*                                     EJ VCC TILL VCC, EJ DC.11           
188100*--- (1:ST INVOICE TO BILLIT --> ADDITIONAL INV. FOR THE FLOW             
188200*                                NON VCC TO VCC, NOT DC.11                
188300     IF (DIST35-NONVCC-VCC-REFILL OR DIST35-NONVCC-VCC-TRANSFER)          
188400           AND                                                            
188500        SHIP-KDFAKSTA-EXP = '3'                                           
188600       MOVE KOLLI-IDFAKT        TO WF-IDFAKREF                            
188700     END-IF                                                               
188800     .                                                                    
188900     EJECT                                                                
189000 GAAA-HAMTA-ARTTEXT SECTION.                                              
189100     MOVE 'GAAA-HAMTA-ARTTEXT'  TO WS-SEKTION                             
189200                                                                          
189300     MOVE BRAD-IDARTNR          TO W-IDARTNR                              
189400                                   W-IDARTNR-D3                           
189500                                   W-IDARTNR-WDK701                       
189600     MOVE WS-IDSKYLT            TO W-IDSKYLT                              
189700     IF BGMT-IDDISTR NOT = GMT-IDDISTR                                    
189800       MOVE SPACE               TO WS-IDSKYLT                             
189900     END-IF                                                               
190000     IF WS-IDSKYLT = SPACE                                                
190100       PERFORM GAAAB-HAMTA-KDVALISO                                       
190200     END-IF                                                               
190300                                                                          
190400     PERFORM IMS-GU-WDD311                                                
190500     IF SEGMENT-FINNS                                                     
190600       MOVE TEXT-BEART          TO WF-BEART                               
190700     END-IF                                                               
190800     .                                                                    
190900     EJECT                                                                
191000 GAAAB-HAMTA-KDVALISO SECTION.                                            
191100     MOVE 'GAAAB-HAMTA-KDVALISO' TO WS-SEKTION                            
191200                                                                          
191300     IF BGMT-IDDISTR = GMT-IDDISTR AND                                    
191400        BGMT-IDKUNDNR = GMT-IDKUNDNR                                      
191500        MOVE BET-KDVALISO       TO WF-KDVALISO                            
191600        MOVE GMT-IDSKYLT        TO WS-IDSKYLT                             
191700                                   W-IDSKYLT                              
191800*                                                                         
191900*       FÖR 'STUDS' REFILLDISTR. GÄLLER SÄNDANDE DC'T                     
192000*       VALUTAKOD (I FÖRSTA FLÖDET)                                       
192100*                                                                         
192200*       (FOR THE BOUNCE REFILL DISTR. THE KDVALISO IS                     
192300*       FROM THE SENDING DC IN THE FIRST FLOW)                            
192400*                                                                         
192500        IF DIST35-NONVCC-NONVCC-REFILL                                    
192600                                OR                                        
192700           DIST35-NONVCC-NONVCC-TRANSFER                                  
192800          MOVE DCS-KDVALISO     TO WF-KDVALISO                            
192900        END-IF                                                            
193000     ELSE                                                                 
193100        MOVE BGMT-IDDISTR       TO W-IDDISTR-WDB2                         
193200                                   W-IDDISTR-WDB2-MIN                     
193300                                   W-IDDISTR-WDB2-MAX                     
193400        MOVE BGMT-IDKUNDNR      TO W-IDKUNDNR-WDB2                        
193500        PERFORM IMS-GU-GMTA-WDB201                                        
193600        IF SEGMENT-FINNS                                                  
193700          CONTINUE                                                        
193800        ELSE                                                              
193900          PERFORM IMS-GU-WDB201                                           
194000        END-IF                                                            
194100        MOVE GMT-IDPARTNR       TO W-WDB1-IDPARTNR                        
194200        MOVE GMT-IDFTG          TO W-WDB1-IDFTG                           
194300        MOVE GMT-IDSKYLT        TO WS-IDSKYLT                             
194400                                   W-IDSKYLT                              
194500        PERFORM IMS-GU-WDB1-WDB101                                        
194600        IF SEGMENT-FINNS                                                  
194700          MOVE BET-KDVALISO     TO WF-KDVALISO                            
194800          MOVE BET-IDMARKBO     TO WS-IDMARKBO                            
194900        ELSE                                                              
195000          MOVE 'SEK'            TO WF-KDVALISO                            
195100        END-IF                                                            
195200*                                                                         
195300*       FÖR 'STUDS' REFILLDISTR. GÄLLER SÄNDANDE DC'T                     
195400*       VALUTAKOD (I FÖRSTA FLÖDET)                                       
195500*                                                                         
195600*       (FOR THE BOUNCE REFILL DISTR. THE KDVALISO IS                     
195700*       FROM THE SENDING DC IN THE FIRST FLOW)                            
195800*                                                                         
195900        IF DIST35-NONVCC-NONVCC-REFILL                                    
196000                                OR                                        
196100           DIST35-NONVCC-NONVCC-TRANSFER                                  
196200          MOVE DCS-KDVALISO     TO WF-KDVALISO                            
196300        END-IF                                                            
196400     END-IF                                                               
196500     .                                                                    
196600     EJECT                                                                
196700 GAAC-SKAPA-SALDOLOGG SECTION.                                            
196800     MOVE 'GAAC-SKAPA-SALDOLOGG'  TO WS-SEKTION                           
196900                                                                          
197000     MOVE FUNCTION CURRENT-DATE (1:8) TO WS-AAAAMMDD                      
197100     COMPUTE LOGT-DAREGDAT-9KOMPL  = 99999999                             
197200                                   - WS-AAAAMMDD                          
197300     MOVE FUNCTION CURRENT-DATE (9:8) TO WS-TTMMSSTH                      
197400     COMPUTE LOGT-TIKLOCK-9KOMPL   = 999999999                            
197500                                   - WS-TTMMSSTH                          
197600     MOVE 9                       TO LOGT-IDSEKVNR                        
197700     MOVE 'OUTB'                  TO LOGT-IDHUVTYP                        
197800     MOVE 'INV'                   TO LOGT-IDSUBTYP                        
197900     MOVE 'W4063700'              TO LOGT-IDPGM                           
198000     MOVE 'W406'                  TO LOGT-IDTRANS                         
198100     MOVE 'W4063700'              TO LOGT-IDUSER                          
198200     MOVE +0                      TO LOGT-IDKUNDNR                        
198300     MOVE '00000000'              TO LOGT-DAREGDAT-LADD                   
198400     MOVE BILL-IDDC               TO LOGT-IDDC                            
198500     MOVE BGMT-IDDISTR            TO LOGT-IDDISTR                         
198600     MOVE BGMT-IDKUNDNR           TO LOGT-IDKUNDNR                        
198700     MOVE BKOLLI-IDKUNDRF(1:8)    TO LOGT-IDKUNDRF(3:8)                   
198800     MOVE '00'                    TO LOGT-IDKUNDRF(1:2)                   
198900                                                                          
199000     MOVE ' '                     TO LOGT-IDTECKEN-KVLS                   
199100     MOVE '-'                     TO LOGT-IDTECKEN-KVTRACK-KVAR           
199200     MOVE TRCK-KVTRACK-KVAR       TO LOGT-KVTRACK-KVAR                    
199300     MOVE TRCK-IDTRACK            TO LOGT-IDTRACK                         
199400                                                                          
199500     PERFORM IMS-ISRT-WDL301                                              
199600     PERFORM UNTIL SEGMENT-FINNS                                          
199700       SUBTRACT 1 FROM LOGT-IDSEKVNR                                      
199800       PERFORM IMS-ISRT-WDL301                                            
199900     END-PERFORM                                                          
200000     .                                                                    
200100     EJECT                                                                
200200 GAAE-SPARA-AVCOST-WDE411  SECTION.                                       
200300     MOVE 'GAAE-SPARA-AVCOST-WDE411' TO WS-SEKTION                        
200400                                                                          
200500     MOVE ZERO                    TO W-SKRIV                              
200600     MOVE BRAD-IDPURAD            TO W-IDPURAD-BSEQ                       
200700     PERFORM IMS-GHU-WDE411-BSEQ                                          
200800     IF SEGMENT-FINNS                                                     
200900       IF SLAG-PRAVCOST = ZERO                                            
201000*        OM PRAVCOST SAKNADES PÅ WDK711 -->                               
201100*        --> TAS SAMMA SOM REDAN FINNS PÅ ORDERRADEN                      
201200         MOVE ORAD-PRAVCOST       TO WF-PRARTNTO                          
201300                                     WF-PRARTBTO                          
201400         MOVE 04                  TO WF-KDPRMOD                           
201500       END-IF                                                             
201600       COMPUTE ORAD-PRAVCOST = WF-PRARTNTO                                
201700       MOVE +1                    TO W-SKRIV                              
201800                                                                          
201900     END-IF                                                               
202000                                                                          
202100     IF W-SKRIV = +1                                                      
202200       PERFORM IMS-REPL-WDE411                                            
202300     END-IF                                                               
202400     .                                                                    
202500     EJECT                                                                
202600 GAB-RADINFO  SECTION.                                                    
202700     MOVE 'GAB-RADINFO'         TO WS-SEKTION                             
202800                                                                          
202900     MOVE BGMT-IDDISTR          TO TEST-IDDISTR                           
203000                                                                          
203100     MOVE 1                     TO UT-REQU-IDMSGVER                       
203200     MOVE SPACE                 TO UT-REQU-KDPGMACT                       
203300     MOVE 'W4063700'            TO UT-REQU-IDUSER                         
203400                                                                          
203500     MOVE 'VCCS'                TO WF-IDLEGSEL                            
203600     MOVE BILL-IDSHIPM          TO WS-REDUIN                              
203700     PERFORM S05-NUM-TEXT                                                 
203800     MOVE WS-REDUUT             TO WF-IDBUNDLE                            
203900                                   WX-IDSHIPM                             
204000     MOVE BKOLLI-IDORDNR7       TO WS-REDUIN                              
204100     PERFORM S05-NUM-TEXT                                                 
204200     MOVE WS-REDUUT             TO WF-IDREF                               
204300                                   WX-IDORDNR                             
204400     MOVE WS-REDUUT             TO WF-IDSEQ(2)                            
204500                                                                          
204600     MOVE BGMT-IDKUNDNR         TO WS-NUM7                                
204700     MOVE WS-NUM7               TO WS-REDUIN                              
204800     PERFORM S05-NUM-TEXT                                                 
204900     MOVE WS-REDUUT             TO WX-IDKUNDNR                            
205000                                                                          
205100     MOVE BKOLLI-IDKOLLI        TO WS-NUM5                                
205200     MOVE WS-NUM5               TO WS-REDUIN                              
205300     PERFORM S05-NUM-TEXT                                                 
205400     MOVE WS-REDUUT             TO WX-IDKOLLI                             
205500                                                                          
205600     MOVE BKOLLI-IDPRODNR       TO WS-NUM7                                
205700     MOVE WS-NUM7               TO WS-REDUIN                              
205800     PERFORM S05-NUM-TEXT                                                 
205900     MOVE WS-REDUUT             TO WX-IDPRODNR                            
215700                                                                          
206100     MOVE KORD-TIORDREG         TO DAT-I-TIDATUM                          
206200                                   WS-AAMMDD                              
206300     MOVE 'AAMMDD'              TO DAT-KDDATFORM                          
215900                                                                          
206500     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
206600                     DAT-O-TIDATUM DAT-KDSVAR                             
216600                                                                          
206800     IF DAT-KDSVAR-OK                                                     
206900       MOVE DAT-TISEKEL         TO WS-SEKEL                               
           ELSE                                                                 
207100       MOVE 20                  TO WS-SEKEL                               
207200     END-IF                                                               
207300     MOVE WS-DATUM              TO WF-DAREFDAT                            
207400     IF DAGENS-DATUM < WS-AAMMDD                                          
207500     AND (DCS-AUSTRALIA OR DCS-JAPAN OR DCS-LAND-NON-VCC-OWNED)           
207600       MOVE DAGENS-DATUM        TO WS-AAMMDD                              
207700       MOVE WS-DATUM            TO WF-DAREFDAT                            
218800     END-IF                                                               
207900*------------------------                                                 
208000     COMPUTE W-RAD-RAKNARE    = W-RAD-RAKNARE  +  1                       
208100     MOVE W-RAD-RAKNARE         TO WF-IDREFRAD                            
208200     MOVE BRAD-BERADREF         TO WF-BEVOLREF                            
208300     MOVE BGMT-IDPARTNR         TO WF-IDPARTNR                            
208400     PERFORM S04-DC-LAND-IDPARTNR                                         
208500**       (WF-IDLANDX3-SEND)                                               
218900                                                                          
208700**       (WF-IDLANDX3-REC)  = BLANK                                       
208800     MOVE BGMT-IDDISTR          TO WS-NUM5                                
208900     MOVE WS-NUM5               TO WS-REDUIN                              
209000     PERFORM S05-NUM-TEXT                                                 
209100     MOVE WS-REDUUT             TO WF-IDEXCUST(1)                         
209200                                                                          
209300     MOVE BGMT-IDKUNDNR         TO WS-NUM7                                
209400     MOVE WS-NUM7               TO WS-REDUIN                              
209500     PERFORM S05-NUM-TEXT                                                 
209600     MOVE WS-REDUUT             TO WF-IDEXCUST(2)                         
209700     MOVE WS-REDUUT             TO WF-IDSEQ(1)                            
209800                                                                          
209900     MOVE BKOLLI-IDPRODNR       TO WS-NUM7                                
210000     MOVE WS-NUM7               TO WS-REDUIN                              
210100     PERFORM S05-NUM-TEXT                                                 
210200     MOVE WS-REDUUT             TO WF-IDOPTION(1)                         
210300                                                                          
210400     MOVE BKOLLI-IDKOLLI        TO WS-NUM5                                
210500     MOVE WS-NUM5               TO WS-REDUIN                              
210600     PERFORM S05-NUM-TEXT                                                 
210700     MOVE WS-REDUUT             TO WF-IDOPTION(2)                         
210800                                                                          
210900     MOVE BRAD-IDPURAD          TO WS-NUM5                                
211000                                   W-IDPURAD                              
211100                                   W-IDPURAD-BSEQ                         
211200     MOVE WS-NUM5               TO WS-REDUIN                              
211300     PERFORM S05-NUM-TEXT                                                 
211400     MOVE WS-REDUUT             TO WF-IDOPTION(3)                         
211500                                                                          
211600     MOVE BKOLLI-BEKUNDRF       TO WF-IDOPTION(5)                         
228400                                                                          
211800*    MOVE BKOLLI-KDORDKL        TO WF-IDAPPEND                            
211900     MOVE BRAD-KDPRODSL         TO WF-IDAPPEND                            
212000*                                                                         
212100     MOVE BRAD-IDARTNR          TO WS-NUM9                                
212200     MOVE WS-NUM9               TO WS-REDUIN                              
212300     PERFORM S05-NUM-TEXT                                                 
240801                                                                          
212500     MOVE SPACE                 TO WS-REKSIFFR1                           
212600     MOVE SPACE                 TO WS-REKSIFFR2                           
212700     IF ((DCS-CDC OR DCS-DDC) AND DIST07-NA-CUSTOMERS)       OR           
212800        ((DCS-CDC OR DCS-DDC) AND DIST07-PACIFIC)            OR           
212900         (DCS-CDC             AND DIST35-NDCCN-NDCUS-REFILL) OR           
213000          DIST35-REFILL-NA                                   OR           
213100          DIST35-REFILL-NA-JAP                               OR           
213200          DIST74-DUBAI-TRADING                                            
213300       MOVE '-'                 TO WS-REKSIFFR1                           
213400       MOVE BRAD-REKSIFFR       TO WS-REKSIFFR2                           
213500       MOVE WS-IDARTNR-CNTRL    TO WF-IDARTNR-CNTRL                       
240802     END-IF                                                               
213700     MOVE WS-REDUUT             TO WF-IDARTNR-FINANCE                     
213800     MOVE WS-REDUUT             TO WF-IDSEQ(3)                            
267700                                                                          
214000     MOVE BRAD-IDSTATNR         TO WF-IDSTATNR                            
214100     IF BRAD-IDSTATNR = ZERO                                              
214200       MOVE GEN-IDSTATNR        TO WF-IDSTATNR                            
           END-IF                                                               
214400     MOVE BRAD-VKARTNTO         TO WF-VKARTNTO                            
214500     IF DIST79-DEALER-PRICE                                               
214600       MOVE BRAD-KDVAT          TO WF-KDVAT                               
214700     ELSE                                                                 
214800       MOVE BRAD-KDVAT          TO WF-KDVAT                               
214900     END-IF                                                               
215000     IF WF-KDVAT = SPACE                                                  
215100       IF DIST42-EJ-EU                                                    
215200         MOVE '90'              TO WF-KDVAT                               
215300       ELSE                                                               
215400         MOVE '70'              TO WF-KDVAT                               
215500       END-IF                                                             
215600     END-IF                                                               
268000                                                                          
215800     PERFORM S20-HAMTA-FLPCOO                                             
268500                                                                          
216000***** POLESTAR SHOULD BE SHOWN ON INVOICE                                 
216100     IF CLAG-IDPROJUP = POLESTAR                                          
216200       MOVE 'POLESTAR'          TO WF-IDEXCUST(3)                         
216300     ELSE                                                                 
216400       MOVE SPACE               TO WF-IDEXCUST(3)                         
216500     END-IF                                                               
                                                                                
216700***** SERVICES SHOULD HAVE A SERVICE VAT                                  
216800***** SOFTWARE ORDERS SHOULD BE A SERVICE VAT                             
216900     MOVE ART-IDFKNGRP TO TEST-IDFKNGRP                                   
217000     IF FKNGRP-EXT-WARRANTY OR FKNGRP-VSA-IMP                             
217100     OR BRAD-KDSOFT > 0                                                   
217200***** OUTSIDE EU                                                          
217300       IF DIST42-EJ-EU                                                    
217400         MOVE '80'              TO WF-KDVAT                               
217500**** SOFTWARE RUSSIA SHOULD HAVE 20% VAT                                  
217600         IF DIST42-RU-SOFT                                                
217700           MOVE 'RU'            TO WF-KDVAT                               
217800         END-IF                                                           
217900       ELSE                                                               
218000***** SWEDEN                                                              
218100         IF DIST42-EJ-EU-PLUS-SE                                          
218200           MOVE '21'            TO WF-KDVAT                               
218300         ELSE                                                             
218400***** EU                                                                  
218500           MOVE '60'            TO WF-KDVAT                               
218600         END-IF                                                           
218700       END-IF                                                             
218800     END-IF                                                               
                                                                                
219000     IF DIST79-DEALER-PRICE                                               
219100       MOVE BRAD-WDE231         TO WY-BRAD-WDE231                         
219200**FIX  FÖR ATT TA HAND OM GROSS < NET PRISER ***                          
219300       IF BRAD-PRARTNTO-LOC > BRAD-PRARTBTO-LOC                           
219400         MOVE BRAD-PRARTNTO-LOC TO BRAD-PRARTBTO-LOC                      
219500         MOVE ZERO              TO BRAD-RERAB                             
219600       END-IF                                                             
219700**END-FIX                                                                 
219800       MOVE BRAD-PRARTBTO-LOC   TO WF-PRARTBTO                            
219900       MOVE BRAD-PRARTNTO-LOC   TO WF-PRARTNTO                            
220000       MOVE 02                  TO WF-KDPRMOD                             
220100       IF BRAD-KDPRTYP = 'T'                                              
220200         MOVE BRAD-PRARTNTO-LOC                                           
220300                                TO WF-PRARTNTO                            
220400         MOVE 02                TO WF-KDPRMOD                             
220500       END-IF                                                             
220600       IF BRAD-PRARTNTO-LOC = ZERO AND                                    
220700          BRAD-PRARTNTO-LOCPREL NOT = ZERO                                
220800          MOVE BRAD-PRARTNTO-LOCPREL                                      
220900                                TO WF-PRARTNTO                            
221000                                   WF-PRARTBTO                            
221100          MOVE 02               TO WF-KDPRMOD                             
221200          MOVE ZERO             TO BRAD-RERAB                             
221300          MOVE 'T'              TO BRAD-KDPRTYP                           
221400       END-IF                                                             
221500       MOVE BRAD-RERAB          TO WF-REARTRAB                            
221600       IF WF-PRARTBTO = ZERO                                              
221700         MOVE BRAD-PRARTNTO-LOC TO WF-PRARTBTO                            
221800         MOVE ZERO              TO BRAD-RERAB                             
221900       END-IF                                                             
222000     ELSE                                                                 
222100       IF DIST79-ECOM-PRICE                                               
222200         MOVE BRAD-WDE231       TO WY-BRAD-WDE231                         
222300         MOVE BRAD-PRARTNTO-LOC TO BRAD-PRARTBTO-LOC                      
222400         MOVE ZERO              TO BRAD-RERAB                             
222500         MOVE BRAD-PRARTBTO-LOC TO WF-PRARTBTO                            
222600         MOVE BRAD-PRARTNTO-LOC TO WF-PRARTNTO                            
222700         MOVE 02                TO WF-KDPRMOD                             
222800         MOVE BRAD-RERAB        TO WF-REARTRAB                            
222900       ELSE                                                               
223000*        MOVE BRAD-PRARTBTO-EXP TO WF-PRARTBTO                            
223100         MOVE BRAD-PRARTNTO     TO WF-PRARTNTO                            
223200                                   WF-PRARTBTO                            
223300         MOVE 01                TO WF-KDPRMOD                             
223400         IF WF-PRARTBTO = ZERO                                            
223500           MOVE BRAD-PRARTNTO   TO WF-PRARTBTO                            
223600           MOVE ZERO            TO WF-REARTRAB                            
223700         ELSE                                                             
223800           MOVE BRAD-RERAB      TO WF-REARTRAB                            
223900         END-IF                                                           
224000       END-IF                                                             
224100     END-IF                                                               
224200     PERFORM GABC-ATERSKRIV-WDE411                                        
224300     MOVE BRAD-KVBEART          TO WF-KVBEART                             
224400     MOVE BRAD-KVLEVART         TO WF-KVLEVART                            
224500     MOVE SPACE                 TO WF-BEART                               
224600     IF DIST79-DEALER-PRICE AND BRAD-BEART-VIPS NOT = SPACE               
224700       MOVE BRAD-BEART-VIPS     TO WF-BEART                               
224800     END-IF                                                               
224900     IF WF-BEART = SPACE                                                  
225000       PERFORM GABA-HAMTA-ARTTEXT                                         
225100     END-IF                                                               
225200     IF DIST38-EJ-EXCH OR DIST38-EJ-KIT-TW                                
225300       MOVE BRAD-BEART-VIPS     TO WF-BEART                               
225400     END-IF                                                               
225500     IF WF-BEART = SPACE                                                  
225600       MOVE BRAD-BEART-VIPS     TO WF-BEART                               
225700       IF WF-BEART = SPACE                                                
225800         MOVE 'PART DESCRIPTION MISSING'  TO WF-BEART                     
225900       END-IF                                                             
226000     END-IF                                                               
226100     IF BRAD-KDSOFT > 0                                                   
226200       MOVE JA                  TO WF-FLSOFT                              
226300     ELSE                                                                 
226400       MOVE NEJ                 TO WF-FLSOFT                              
226500     END-IF                                                               
226600*                                                                         
226700     IF WF-FLSOFT = JA                                                    
226800       PERFORM IMS-GNP-WDE411                                             
226900       IF SEGMENT-FINNS                                                   
227000         MOVE WF-IDARTNR-FINANCE  TO WS-IDARTNR-SOFTWARE                  
227100         MOVE ORAD-IDARBREF       TO WS-IDARBREF                          
227200         MOVE ORAD-IDBIL          TO WS-IDBIL                             
227300         MOVE ORAD-IDVIN          TO WS-IDVIN                             
227400         MOVE WS-IDARTNR-SOFTWARE TO WF-IDARTNR-FINANCE                   
227500       END-IF                                                             
227600     END-IF                                                               
227700*                                                                         
227800*--     CHECK IF LYNK --> GET THE LYNK PART NO. AND                       
227900*                         SEND IT TO BILLIT                               
228000*      !BUT REFILL DISTRICT SHOULD NOT HAVE                               
228100*           THE LYNK PART NO.(15/5 '25)                                   
228200*                                                                         
228300     MOVE BRAD-KDPRODSL           TO TEST-KDPRODSL                        
*                                                                               
228500     IF (SHIP-IDSYSTEM = 'LYNK' OR KDPRODSL-LYNK)                         
228600        AND NOT DIST35-REFILL                                             
228700*                                                                         
228800       MOVE BRAD-IDARTNR         TO W-IDARTNR-F5                          
228900       PERFORM IMS-GU-WDF502                                              
229000       IF SEGMENT-FINNS                                                   
229100         MOVE WF-IDARTNR-FINANCE TO WS-IDARTNR-LYNK                       
229200         MOVE XLEV-IDLEVART      TO WS-IDARTNR-L                          
229300         MOVE WS-IDARTNR-LYNK    TO WF-IDARTNR-FINANCE                    
229400       END-IF                                                             
*                                                                               
229600       MOVE JA                   TO LYNK-DISTR-SW                         
229700     END-IF                                                               
229800*                                                                         
229900     IF BRAD-KDPRTYP = 'T'                                                
230000       MOVE JA                  TO WF-FLSPECPR                            
230100     ELSE                                                                 
230200       MOVE NEJ                 TO WF-FLSPECPR                            
230300     END-IF                                                               
230400     IF BKOLLI-KDFAKTYP = 'G'                                             
230500       MOVE JA                  TO WF-FLFREE                              
230600     ELSE                                                                 
230700       MOVE NEJ                 TO WF-FLFREE                              
230800     END-IF                                                               
230900     MOVE NEJ                   TO WF-FLPRIV                              
231000     IF DIST79-DEALER-PRICE OR                                            
231100        DIST79-ECOM-PRICE                                                 
231200       MOVE BRAD-KDVALISO       TO WF-KDVALISO                            
231300     ELSE                                                                 
231400       MOVE 'SEK'               TO WF-KDVALISO                            
231500     END-IF                                                               
231600     IF WF-KDVALISO = SPACE                                               
231700       PERFORM GABAA-HAMTA-KDVALISO                                       
231800     END-IF                                                               
231900     IF WX-KDVALISO = SPACE                                               
232000       MOVE WF-KDVALISO         TO WX-KDVALISO                            
232100     END-IF                                                               
232200     IF WX-KDVALISO NOT = WF-KDVALISO                                     
232300       MOVE 'OLIKA VALUTOR PÅ KOLLI' TO FELTEXT                           
232400       CALL FELLOG                                                        
232500     END-IF                                                               
232600     IF DIST79-DEALER-PRICE                                               
232700       IF WF-KDVALISO = 'SEK'                                             
232800         IF BGMT-IDDISTR = 778                                            
232900           CONTINUE                                                       
233000         ELSE                                                             
233100           MOVE 'FEL VALUTA SEK' TO FELTEXT                               
233200           CALL FELLOG                                                    
233300         END-IF                                                           
233400       END-IF                                                             
233500     END-IF                                                               
233600     IF DIST42-EJ-EU  OR  BGMT-FLCOD = JA  OR                             
233700        BILL-KDFINDOC = 'PROF'             OR                             
233800        DCS-DDC                            OR                             
233900        DIST35-NONVCC-NONVCC-REFILL        OR                             
234000        DIST35-NONVCC-NONVCC-TRANSFER                                     
234100       MOVE 'NOW '              TO WF-KDINVFRQ                            
234200       IF (DIST92-ITALY AND DCS-DDC)                                      
234300*           DIR.LEVERANSER                                                
234400         MOVE 'DAY'             TO WF-KDINVFRQ                            
234500       END-IF                                                             
234600       IF BRAD-KDSOFT > 0                                                 
234700*             SOFTWARE                                                    
234800         MOVE 'DAY'             TO WF-KDINVFRQ                            
234900         IF DIST38-PER-FRQ                                                
235000           MOVE 'PER '          TO WF-KDINVFRQ                            
235100         END-IF                                                           
235200       END-IF                                                             
235300     ELSE                                                                 
235400       MOVE '    '              TO WF-KDINVFRQ                            
235500       IF BRAD-KDSOFT > 0 AND DIST92-ITALY                                
235600         MOVE 'DAY'             TO WF-KDINVFRQ                            
235700*      ELSE                                                               
235800*        IF DIST92-ITALY AND DCS-DDC                                      
235900*          IF BRAD-KDSOFT = 0                                             
236000*           PERFORM IMS-GNP-WDE411                                        
236100*          END-IF                                                         
236200*          IF ORAD-FLDIRLEV = JA                                          
236300*            MOVE 'DAY'         TO WF-KDINVFRQ                            
236400*          END-IF                                                         
236500*        END-IF                                                           
236600       END-IF                                                             
236700       IF DIST19-SATS                                                     
236800          OR BGMT-IDPARTNR = '3636     '                                  
236900*           RENAULT 1479-4417                                             
237000         MOVE 'NOW '            TO WF-KDINVFRQ                            
237100       END-IF                                                             
237200     END-IF                                                               
237300***** EXTENDED WARRANTY SHOULD BE INVOICED DAYLY                          
237400     MOVE ART-IDFKNGRP TO TEST-IDFKNGRP                                   
237500     IF FKNGRP-EXT-WARRANTY OR FKNGRP-VSA-IMP                             
237600       MOVE 'DAY'               TO WF-KDINVFRQ                            
237700     END-IF                                                               
237800     MOVE BILL-KDFINDOC         TO WF-KDFINDOC                            
237900     MOVE BGMT-IDPARTNR         TO WS-IDPARTNR                            
238000     IF BILL-KDFINDOC NOT = 'PROF'                                        
238100       IF WS-INT NOT NUMERIC                                              
238200          MOVE 'INT '           TO WF-KDFINDOC                            
238300       ELSE                                                               
238400          IF BILL-KDFINDOC = 'INT '                                       
238500            MOVE 'INV '         TO WF-KDFINDOC                            
238600          END-IF                                                          
238700       END-IF                                                             
238800     END-IF                                                               
238900     IF  DCS-NDC-NA                AND                                    
239000        (DIST07-USA-RETAILER       OR                                     
239100         DIST07-CAN-RETAILER       OR                                     
239200         DIST18-SCRAP-NDC          OR                                     
239300         DIST18-SCRAP-NDC-SC-LOCAL OR                                     
239400         DIST35-NA-CDC-RETURN      OR                                     
239500         DIST35-NA-NDC-RETURNS     OR                                     
239600         DIST35-NA-TRANSFER        OR                                     
239700         DIST35-REFILL-INOM-NA     OR                                     
239800         DIS105-NA-SPEC)                                                  
239900        MOVE 'DUMM'             TO WF-KDFINDOC                            
240000        MOVE 'XX'               TO WF-KDVAT                               
240100     END-IF                                                               
240200     MOVE SPACE                 TO WF-IDBREAK(1)                          
240300     IF BKOLLI-FLOVRLEV = JA  OR                                          
240400        BKOLLI-KDFAKTYP = 'N'  OR                                         
240500        DIST18-SKROT  OR   DIST18-SCRAP-NDC                               
240600       MOVE BKOLLI-IDORDNR7     TO WS-REDUIN                              
240700       PERFORM S05-NUM-TEXT                                               
240800       MOVE WS-REDUUT           TO WF-IDBREAK(1)                          
*                                                                               
*          END-IF                                                               
241100     IF DIST92-ITALY-SEP AND BRAD-KDSOFT = 0 AND NOT DCS-DDC              
241200*       FÖR ATT FÅ EN FAKTURA PER IDSHIPM                                 
241300       MOVE JA                  TO BGMT-FLSEPINV                          
*          END-IF                                                               
241500     IF ((DCS-CDC OR DCS-DDC) AND DIST07-NA-CUSTOMERS)  OR                
241600        ((DCS-CDC OR DCS-DDC) AND DIST07-PACIFIC)       OR                
241700        ((DCS-CDC OR DCS-DDC) AND DIST07-NON-VCC-OWNED) OR                
241800        DIST35-REFILL-NA                                OR                
241900        DIST35-REFILL-NA-JAP                            OR                
242000        DIST35-CDC-NONVCC-REFILL                        OR                
242100        DIST35-VCC-NONVCC-REFILL                        OR                
242200        DIST35-NONVCC-REFILL                            OR                
242300        DIST35-NONVCC-VCC-TRANSFER                      OR                
242400        DIST35-NONVCC-NONVCC-TRANSFER                   OR                
242500        (SHIP-KDFAKSTA-EXP > ZERO)                                        
242600       MOVE JA                   TO BGMT-FLSEPINV                         
242700     END-IF                                                               
242800     IF BGMT-FLSEPINV = JA   OR                                           
242900        NOLL-BGMT-FLSEPINV = JA                                           
243000        MOVE WF-IDBUNDLE         TO WF-IDBREAK(1)                         
243100     END-IF                                                               
243200     IF BGMT-FLCOD = JA                                                   
243300       IF DCS-SDC AND DCS-AUSTRIA AND                                     
243400          DIST90-AUSTRIA-BALKAN                                           
243500         MOVE SPACE              TO WF-IDBREAK(1)                         
243600         MOVE BGMT-IDDISTR       TO WS-NUM5                               
243700         MOVE WS-NUM5            TO WS-REDUIN                             
243800         PERFORM S05-NUM-TEXT                                             
243900         MOVE WS-REDUUT          TO WF-IDBREAK(1)                         
244000       ELSE                                                               
244100         MOVE SPACE              TO WF-IDBREAK(1)                         
244200         MOVE BGMT-IDKUNDNR      TO WS-NUM7                               
244300         MOVE WS-NUM7            TO WS-REDUIN                             
244400         PERFORM S05-NUM-TEXT                                             
244500         MOVE WS-REDUUT          TO WF-IDBREAK(1)                         
244600       END-IF                                                             
244700     END-IF                                                               
244800     MOVE SPACE                  TO WF-IDLEVNR                            
244900     IF BILL-IDLEVNR  = '00000' OR SPACE                                  
245000       IF DIST42-EJ-EU AND                                                
245100         (DCS-CDC OR DCS-SDC AND NOT DCS-SWEDEN)                          
245200           MOVE BILL-IDSHIPM        TO WS-REDUIN                          
245300           PERFORM S05-NUM-TEXT                                           
245400           MOVE WS-REDUUT           TO WF-IDBREAK(1)                      
245500           IF BRAD-KDSOFT > 0                                             
245600             MOVE SPACE             TO WF-IDBREAK(1)                      
245700           END-IF                                                         
245800*                                                                         
245900          IF BGMT-FLCOD = JA                                              
246000**         MOVE SPACE              TO WF-IDBREAK(1)                       
246100**         MOVE BGMT-IDKUNDNR      TO WS-NUM7                             
246200**         MOVE WS-NUM7            TO WS-REDUIN                           
246300**         PERFORM S05-NUM-TEXT                                           
246400**         MOVE WS-REDUUT          TO WF-IDBREAK(1)                       
246500           MOVE WF-IDBREAK(1)      TO WS-REDUUT                           
246600           MOVE 'C'                TO WS-REDUUT(8:1)                      
246700           MOVE WS-REDUUT          TO WF-IDBREAK(1)                       
246800          END-IF                                                          
246900       END-IF                                                             
247000     ELSE                                                                 
247100*       MOVE BILL-IDLEVNR           TO WF-IDLEVNR                         
247200        MOVE KOLLI-IDLEVNR          TO WF-IDLEVNR                         
247300        IF DCS-DDC AND                                                    
247400         ((DCS-NORWAY  AND NOT DIST03-NORGE) OR                           
247500          (DCS-BELGIUM AND DIST42-EJ-EU)     OR                           
247600          (DCS-GERMANY AND DIST42-EJ-EU)     OR                           
247700          (DCS-SWEDEN  AND DIST42-EJ-EU))                                 
247800***        (DDC-SE AND KDVIA NOT = '01' AND DIST42-EJ-EU)                 
247900***        MÅSTE ÄNDRAS TILL USA                                          
248000           MOVE SPACE               TO WF-IDBREAK(1)                      
248100           IF KOLLI-KDVIA = '01'                                          
248200             MOVE BILL-IDSHIPM        TO WS-REDUIN                        
248300             PERFORM S05-NUM-TEXT                                         
248400             MOVE WS-REDUUT           TO WF-IDBREAK(1)                    
248500             MOVE '00000'             TO WF-IDLEVNR                       
248600*           OM KDVIA = '01' SKA IDLEVNR VARA BLANK/00000                  
248700*           ANNARS SKICKAS MAIL TILL DIREKTLEVERANTÖREN I BILL-IT         
248800           ELSE                                                           
248900             IF BGMT-FLSEPINV = JA                                        
249000               MOVE WF-IDBUNDLE       TO WF-IDBREAK(1)                    
249100             ELSE                                                         
249200               MOVE BKOLLI-IDORDNR7   TO WS-REDUIN                        
249300               PERFORM S05-NUM-TEXT                                       
249400               MOVE WS-REDUUT         TO WF-IDBREAK(1)                    
249500             END-IF                                                       
249600           END-IF                                                         
249700        END-IF                                                            
249800        IF DCS-DDC                                                        
249900          CONTINUE                                                        
250000        ELSE                                                              
250100           MOVE '00000'           TO WF-IDLEVNR                           
250200        END-IF                                                            
250300     END-IF                                                               
250400     IF  DCS-NDC-NA  AND                                                  
250500        (DIST07-USA-RETAILER      OR                                      
250600         DIST07-CAN-RETAILER      OR                                      
250700         DIST18-SCRAP-NDC         OR                                      
250800         DIST18-SCRAP-NDC-SC-LOCAL OR                                     
250900         DIST35-NA-CDC-RETURN     OR                                      
251000         DIST35-NA-NDC-RETURNS    OR                                      
251100         DIST35-NA-TRANSFER       OR                                      
251200         DIST35-REFILL-INOM-NA    OR                                      
251300         DIST35-PACIFIC-TRANSFER  OR                                      
251400         DIST35-REFILL-INOM-JP    OR                                      
251500         DIST35-NDCUS-CDC-REFILL  OR                                      
251600         DIS105-NA-SPEC)                                                  
251700       MOVE BILL-IDSHIPM          TO WS-REDUIN                            
251800       PERFORM S05-NUM-TEXT                                               
251900       MOVE WS-REDUUT             TO WF-IDBREAK(1)                        
252000     END-IF                                                               
252100     IF  DCS-CHINA                AND                                     
252200         (DIST35-NDCCN-CDC-REFILL OR                                      
252300          DIST35-NONVCC-VCC-REFILL)                                       
252400         MOVE BILL-IDSHIPM        TO WS-REDUIN                            
252500       PERFORM S05-NUM-TEXT                                               
252600       MOVE WS-REDUUT             TO WF-IDBREAK(1)                        
252700     END-IF                                                               
252701*CCID 4665421 - REMOVE DIST35-CDC-RETURNS-NON-VCC IN BELOW IF             
252702*BECAUSE NON VCC RETURNS CROSS 750 LINES IN A SHIPMENT RESULTING          
252703*RESULTING IN MISSING LINES IN DR                                         
252800     IF  DCS-LAND-NON-VCC-OWNED   AND                                     
252900        (DIST07-NON-VCC-OWNED     OR                                      
253000         DIST18-SCRAP-NDC         )                                       
253100*        DIST35-CDC-RETURNS-NON-VCC)                                      
253200       MOVE BILL-IDSHIPM          TO WS-REDUIN                            
253300       PERFORM S05-NUM-TEXT                                               
253400       MOVE WS-REDUUT             TO WF-IDBREAK(1)                        
253500     END-IF                                                               
253510*CCID 4665421 - INSTEAD CREATE INVOICE FOR AN ORDER FOR                   
253520*'NDC RETURNS TO VCC'                                                     
253530     IF DCS-LAND-NON-VCC-OWNED AND                                        
253540        DIST35-CDC-RETURNS-NON-VCC                                        
253550       MOVE BKOLLI-IDORDNR7       TO WS-REDUIN                            
253560       PERFORM S05-NUM-TEXT                                               
253570       MOVE WS-REDUUT             TO WF-IDBREAK(1)                        
253580     END-IF                                                               
253600     IF DIST38-FAKTURA-PER-SKEPPNING AND                                  
253700        BRAD-KDSOFT = 0                                                   
253800       MOVE BILL-IDSHIPM          TO WS-REDUIN                            
253900       PERFORM S05-NUM-TEXT                                               
254000       MOVE WS-REDUUT             TO WF-IDBREAK(1)                        
254100     END-IF                                                               
254200     IF ((SHIP-IDSYSTEM = 'LYNK' OR LYNK-DISTR)                           
254300         AND                                                              
254400         BRAD-KDSOFT = 0)                                                 
254500         AND NOT DIST35-REFILL                                            
254600*                                                                         
254700       MOVE BILL-IDSHIPM          TO WS-REDUIN                            
254800       PERFORM S05-NUM-TEXT                                               
254900       MOVE WS-REDUUT             TO WF-IDBREAK(1)                        
255000       MOVE NEJ                   TO LYNK-DISTR-SW                        
255100     END-IF                                                               
255200     IF DIST38-PER-FRQ AND                                                
255300        BRAD-KDSOFT > 0                                                   
255400       MOVE SPACE                 TO WF-IDBREAK(1)                        
255500       MOVE BGMT-IDKUNDNR         TO WS-NUM7                              
255600       MOVE WS-NUM7               TO WS-REDUIN                            
255700       PERFORM S05-NUM-TEXT                                               
255800       MOVE WS-REDUUT             TO WF-IDBREAK(1)                        
255900     END-IF                                                               
256000     IF DCS-NDC-PF                AND                                     
256100       (DIST07-PACIFIC            OR                                      
256200        DIST18-SCRAP-NDC          OR                                      
256300        DIST35-JPAU-CDC-RETUR     OR                                      
256400        DIST35-JPAU-CDC-RETUR-Q   OR                                      
256500        DIST35-PACIFIC-TRANSFER   OR                                      
256600        DIST35-REFILL-INOM-JP)                                            
256700       MOVE BILL-IDSHIPM          TO WS-REDUIN                            
256800       PERFORM S05-NUM-TEXT                                               
256900       MOVE WS-REDUUT             TO WF-IDBREAK(1)                        
257000*                                                                         
257100       IF BGMT-FLCOD = JA                                                 
257200         MOVE WF-IDBREAK(1)       TO WS-REDUUT                            
257300         MOVE 'C'                 TO WS-REDUUT(8:1)                       
257400         MOVE WS-REDUUT           TO WF-IDBREAK(1)                        
257500       END-IF                                                             
257600     END-IF                                                               
257700     IF DCS-DDC AND BGMT-IDDISTR = 778 AND                                
257800        WF-IDBREAK(1) = SPACE                                             
257900       MOVE SPACE                 TO WF-KDINVFRQ                          
258000       MOVE 'DDGS'                TO WF-IDBREAK(1)                        
258100     END-IF                                                               
258200     IF ( WF-IDBREAK(1) = SPACES AND BGMT-IDDISTR = 1558 )                
258300     OR ( WF-IDBREAK(1) = SPACES AND BGMT-IDDISTR = 1578 )                
258400       MOVE BGMT-IDKUNDNR         TO WS-NUM7                              
258500       MOVE WS-NUM7               TO WS-REDUIN                            
258600       PERFORM S05-NUM-TEXT                                               
258700       MOVE WS-REDUUT             TO WF-IDBREAK(1)                        
258800     END-IF                                                               
258900     IF BGMT-FLCOD = JA                                                   
259000       MOVE WF-IDBREAK(1)         TO WS-REDUUT                            
259100       MOVE 'C'                   TO WS-REDUUT(8:1)                       
259200       MOVE WS-REDUUT             TO WF-IDBREAK(1)                        
259300     END-IF                                                               
259400     MOVE ART-IDFKNGRP TO TEST-IDFKNGRP                                   
259500     IF FKNGRP-VSA-IMP                                                    
259600       MOVE WF-IDBREAK(1)         TO WS-REDUUT                            
259700       MOVE 'SERVVSA'             TO WS-REDUUT(1:7)                       
259800       MOVE WS-REDUUT             TO WF-IDBREAK(1)                        
259900     END-IF                                                               
260000     MOVE SPACE                   TO WF-IDBREAK(2)                        
260100     MOVE BGMT-IDDISTR            TO WS-NUM5                              
260200     MOVE WS-NUM5                 TO WS-REDUIN                            
260300     MOVE BILL-IDDC               TO WS-REDUIN(6:2)                       
260400**** EXTENDED WARRANTY SHOULD BE ON OWN INVOICE                           
260500**** AND PER SHIPMENT                                                     
260600**** NEEDS TO BE AFTER THE DEFAULT SETTING OF BREAK(2)                    
260700     IF FKNGRP-EXT-WARRANTY                                               
260800       MOVE WF-IDBREAK(1)         TO WS-REDUUT                            
260900       MOVE 'SERVEXT'             TO WS-REDUUT(1:7)                       
261000       MOVE WS-REDUUT             TO WF-IDBREAK(1)                        
261100       MOVE BILL-IDSHIPM          TO WS-REDUIN                            
261200       PERFORM S05-NUM-TEXT                                               
261300       MOVE WS-REDUUT             TO WF-IDBREAK(2)                        
261400     END-IF                                                               
261500     IF (DIST35-NONVCC-NONVCC-REFILL                                      
261600                                  OR                                      
261700         DIST35-NONVCC-NONVCC-TRANSFER)                                   
261800                                  AND                                     
261900         SHIP-IDDC-EXP > SPACE                                            
262000       MOVE SHIP-IDDC-EXP         TO WS-REDUIN(6:2)                       
262100     END-IF                                                               
262200     MOVE WS-REDUIN               TO WF-IDBREAK(2)                        
262300**      WF-IDBREAK(2)  DISTRIKT+DC                                        
262400**      WF-IDSEQ(1)    IDKUNDNR  DDI: IDORDNR                             
262500**      WF-IDSEQ(2)    IDORDNR        IDKOLLI                             
262600**      WF-IDSEQ(3)    ARTNR ?        IDPRODNR                            
262700**                                                                        
262800**** GROUP AS PER CUSTOMER NUMBERS                                        
262900     IF BGMT-IDPARTNR = 33186                                             
263000     OR BGMT-IDPARTNR = 27823                                             
263100     OR BGMT-IDPARTNR = 428324                                            
263200       MOVE SPACE               TO WF-IDBREAK(1)                          
263300                                   WF-IDBREAK(2)                          
263400       MOVE BGMT-IDPARTNR       TO WF-IDBREAK(1)                          
263500       MOVE BKOLLI-BEKUNDRF(1:8)   TO WF-IDBREAK(2)                       
263600       IF BKOLLI-BEKUNDRF(9:1) NOT = SPACE                                
263700         MOVE BKOLLI-BEKUNDRF(9:7) TO WF-IDBREAK(1)                       
263800       END-IF                                                             
           END-IF                                                               
264000     MOVE BRAD-KDARTURS         TO WF-KDARTURS                            
264100     MOVE BILL-IDDC             TO WF-IDDC                                
*                                                                               
264300**** GROUP AS PER ORDER NUMBER                                            
264400     IF BGMT-IDPARTNR = 1618                                              
264500     OR BGMT-IDPARTNR = 1680                                              
264600       MOVE SPACE               TO WF-IDBREAK(1)                          
264700                                   WF-IDBREAK(2)                          
264800       MOVE BKOLLI-IDORDNR7     TO WS-REDUIN                              
             PERFORM S05-NUM-TEXT                                               
             MOVE WS-REDUUT           TO WF-IDBREAK(1)                          
265100     END-IF                                                               
265200*                                                                         
265300*--  STUDSFLÖDE --> RÄTT DC OCH FAKT.NR.1 I REF.                          
265400*                                                                         
265500*--  (BOUNCE FLOW -> CORRECT DC + THE CORRECT INV.NO. IN IDFAKREF)        
265600*                                                                         
265700     IF ((DIST35-NONVCC-NONVCC-REFILL OR                                  
265800          DIST35-NONVCC-NONVCC-TRANSFER)                                  
265900              AND                                                         
266000         SHIP-IDDC-EXP > SPACE)                                           
266100              OR                                                          
266200        (SHIP-IDDC-EXP > SPACE  AND                                       
266300         SHIP-IDDC-EXP = WC-CDC-SE)                                       
266400                                                                          
266500       MOVE SHIP-IDDC-EXP       TO WF-IDDC                                
266600       MOVE KOLLI-IDFAKT-EXP    TO WF-IDFAKREF                            
266700     END-IF                                                               
266800     MOVE KORD-KDFRAKT          TO WF-KDFRAKT                             
266900     MOVE SPACES                TO WF-BELEVVIL                            
267000     PERFORM S06-HAEMTA-LEVVIL-TEXT                                       
267100*                                                                         
267200*    SPECIAL DEL.TERMS FOR GLOBAL EXP - CN TO KR                          
267300*    SECOND INVOICE                                                       
267400     IF DIST41-DEL-TERMS-GE                                               
267500       MOVE 'CIP     '          TO WF-BELEVVIL                            
267600     END-IF                                                               
267700                                                                          
267800     MOVE WF-BELEVVIL           TO WS-BELEVVIL                            
267900*                                                                         
268000                                                                          
268100     MOVE BRAD-IDKONTO          TO WS-NUM11                               
268200     MOVE WS-NUM11              TO WS-REDUIN                              
268300     PERFORM S05-NUM-TEXT                                                 
268400     MOVE WS-REDUUT             TO WF-IDACCNT(1)                          
268500                                                                          
268600     MOVE BRAD-IDANALYS         TO WF-IDACCNT(2)                          
268700     MOVE BRAD-IDKST            TO WS-REDUIN                              
268800     PERFORM S05-NUM-TEXT                                                 
268900     MOVE WS-REDUUT             TO WF-IDACCNT(3)                          
269000     MOVE SPACE                 TO WF-BEANST                              
269100                                   WF-IDUSER                              
269200                                   WF-BETEXT                              
269300     IF BGMT-IDDISTR = 2615 OR 2616 OR                                    
269400                       7490 OR                                            
269500                       6561                                               
269600*      SPECIAL GODSMOTTAGARE.ADRESS TILL UKRAINA,                         
269700*                                        GUATEMALA,                       
269800*                                        MEXICO                           
269900       IF BGMT-IDDISTR = GMT-IDDISTR AND                                  
270000          BGMT-IDKUNDNR = GMT-IDKUNDNR                                    
270100         CONTINUE                                                         
270200       ELSE                                                               
270300         MOVE BGMT-IDDISTR      TO W-IDDISTR-WDB2                         
270400         MOVE BGMT-IDKUNDNR     TO W-IDKUNDNR-WDB2                        
270500         PERFORM IMS-GU-WDB201                                            
270600       END-IF                                                             
270700       MOVE SPACES              TO W-BETEXT                               
270800                                   Y-BETEXT                               
270900                                   Z-BETEXT                               
271000       MOVE GMT-BEGMT-RAD1      TO W-RAD1                                 
271100                                   Y-RAD1                                 
271200       MOVE GMT-BEGMT-RAD2      TO W-RAD2                                 
271300                                   Y-RAD2                                 
271400       MOVE GMT-ADGMT-GATA      TO W-GATA                                 
271500                                   Y-GATA                                 
271600       MOVE GMT-ADGMT-PADR      TO W-PADR                                 
271700                                   Y-PADR                                 
271800       MOVE GMT-ADGMT-LAND      TO W-LAND                                 
271900                                   Y-LAND                                 
272000       IF BGMT-IDDISTR = 7490                                             
272100        MOVE GMT-BEGMT-RAD1     TO Z-RAD1                                 
272200        MOVE GMT-ADGMT-GATA     TO Z-RAD2                                 
272300        MOVE GMT-ADGMT-PADR     TO Z-GATA                                 
272400        MOVE GMT-ADGMT-LAND     TO Z-PADR                                 
272500       END-IF                                                             
272600*       ÄR E401 INLÄST ?                                                  
272700       MOVE KORD-IDORDER        TO W-IDORDER                              
272800       PERFORM IMS-GU-WDQ201                                              
272900       IF SEGMENT-FINNS                                                   
273000         MOVE OHUV-BEGMT-RAD1   TO W-RAD1                                 
273100         MOVE OHUV-BEGMT-RAD2   TO W-RAD2                                 
273200         MOVE OHUV-ADGMT-GATA   TO W-GATA                                 
273300         MOVE OHUV-ADGMT-PADR   TO W-PADR                                 
273400         MOVE OHUV-ADGMT-LAND   TO W-LAND                                 
273500         IF BGMT-IDDISTR = 7490                                           
273600          MOVE OHUV-BEGMT-RAD1  TO Z-RAD1                                 
273700          MOVE OHUV-ADGMT-GATA  TO Z-RAD2                                 
273800          MOVE OHUV-ADGMT-PADR  TO Z-GATA                                 
273900          MOVE OHUV-ADGMT-LAND  TO Z-PADR                                 
274000         END-IF                                                           
274100       END-IF                                                             
274200       MOVE W-BETEXT            TO WF-BETEXT                              
274300       IF BGMT-IDDISTR = 7490                                             
274400         MOVE Z-BETEXT          TO WF-BETEXT                              
274500       END-IF                                                             
274600       MOVE SPACE               TO WF-IDBREAK(1)                          
274700       IF Y-BETEXT = W-BETEXT                                             
274800         MOVE BGMT-IDKUNDNR     TO WS-NUM7                                
274900         MOVE WS-NUM7           TO WS-REDUIN                              
275000         PERFORM S05-NUM-TEXT                                             
275100         MOVE WS-REDUUT         TO WF-IDBREAK(1)                          
275101       ELSE                                                               
275102         MOVE BKOLLI-IDORDNR7   TO WS-REDUIN                              
275103         PERFORM S05-NUM-TEXT                                             
275104         MOVE WS-REDUUT         TO WF-IDBREAK(1)                          
275105       END-IF                                                             
275106     END-IF                                                               
275107                                                                          
275900     MOVE BRAD-IDLEVNR-ART      TO WF-IDLEVNR-ART                         
276000                                                                          
276100*** ECOM CUSTOMER                                                         
276200*    IF GMT-KDKUNDKAT = 18                                                
276300*                                                                         
276400     IF DIST79-ECOM-PRICE                                                 
276500       MOVE 'ECOM'              TO WF-IDSYSTEM-SEND                       
276600       MOVE 'W476'              TO WF-IDSYSTEM-REC                        
276700     ELSE                                                                 
276800       MOVE 'W476'              TO WF-IDSYSTEM-SEND                       
276900                                   WF-IDSYSTEM-REC                        
277000     END-IF                                                               
277100*                                                                         
277200*     OM ITALIEN OCH DISTR 71,81,90                                       
277300*     SKALL DE HA SVERIGES FAKTURANR-SERIE                                
277400     IF DIST03-ITALIEN                                                    
277500       IF DCS-SDC AND DCS-ITALY                                           
277600         MOVE 'W47X'            TO WF-IDSYSTEM-SEND                       
277700       END-IF                                                             
277800     END-IF                                                               
277900*                                                                         
278000*--- VOR FRÅN DC11 (DETTA ÄR FÖRSTA FAKTURAN)                             
278100*    GÄLLER FÖR CHINA, INDIEN, KOREA, TURKIET                             
278200*          -> * RÄKNA FRAM OCH SPARA PRAVCOST                             
278300*               REDAN VID FÖRSTA FAKTURAN                                 
278400*    - THE FOLLOWING FINANCIAL DOCUMENTS HAVE AVERAGE COST                
278500*      . 'STUDS' FLÖDE                                                    
278600*      . KDFAKTYP = N                                                     
278700*      . DEALER DISTRICTS                                                 
278800*                                                                         
278900     IF SHIP-IDDC-EXP > SPACE                                             
279000       IF BKOLLI-KDFAKTYP = 'N'  OR                                       
279100          DIST07-NON-VCC-OWNED                                            
**                                                                              
                                                                                
279400         MOVE BRAD-IDARTNR TO W-IDARTNR-WDK701                            
279500         MOVE SHIP-IDDC-EXP TO W-IDDC-WDK711                              
279600         PERFORM IMS-GU-WDK701                                            
279700         PERFORM IMS-GNP-WDK711                                           
279101                                                                          
279102                                                                          
280000*---     VOR DC11 --> CN/IN/KR/TR/MY                                      
280100         IF ((DCS-CDC OR DCS-DDC) AND DIST07-NON-VCC-OWNED)               
280200           PERFORM GABD-RAEKNA-AVERAGE-COST                               
280300           PERFORM IMS-GU-WDK701                                          
280400           PERFORM IMS-GHNP-WDK711                                        
280500           MOVE WS-PRAVCOST TO SLAG-PRAVCOST                              
280600           PERFORM IMS-REPL-WDK711                                        
279702                                                                          
280800           PERFORM GABE-SPARA-AVCOST-WDE411                               
280900           MOVE ORAD-PRARTNTO TO WF-PRARTNTO                              
281000           MOVE 01            TO WF-KDPRMOD                               
281100         END-IF                                                           
             END-IF                                                             
           END-IF                                                               
281400     IF (SHIP-IDDC-EXP = WC-CDC-SE) AND                                   
281500        (SHIP-IDDC     = WC-NDC-AE OR WC-NDC-MX)                          
281600         AND                                                              
281700         TRACK-SKRIV                                                      
281800       MOVE 1                  TO TRACK-IX                                
281900*                                                                         
282000*--    NYCKLAR TILL WDE1                                                  
282100       MOVE BKOLLI-IDDISTR     TO W-IDDISTR-E1                            
282200       MOVE BKOLLI-IDKUNDNR    TO W-IDKUNDNR-E1                           
282300       MOVE BKOLLI-IDPRODNR    TO W-IDPRODNR-E1                           
282400       MOVE BKOLLI-IDKOLLI     TO W-IDKOLLI-E1                            
282500       MOVE BRAD-IDPURAD       TO W-IDPURAD                               
282600*                                                                         
282700       PERFORM IMS-GU-WDE131                                              
282800       PERFORM IMS-GNP-WDE141                                             
282900       PERFORM UNTIL SEGMENT-SAKNAS OR TRACK-IX > MAX-TRACK-IX            
283000         MOVE TLEV-IDTRACK     TO WF-IDTRACK     (TRACK-IX)               
283100         MOVE TLEV-KVTRACK-LEV TO WF-KVTRACK-LEV (TRACK-IX)               
283200         ADD +1              TO TRACK-IX                                  
283300         PERFORM IMS-GNP-WDE141                                           
283400       END-PERFORM                                                        
283500                                                                          
283600       PERFORM UNTIL TRACK-IX > MAX-TRACK-IX                              
283700         MOVE SPACE          TO WF-IDTRACK     (TRACK-IX)                 
283800         MOVE ZERO           TO WF-KVTRACK-LEV (TRACK-IX)                 
283900         ADD +1              TO TRACK-IX                                  
284000       END-PERFORM                                                        
284100     ELSE                                                                 
284200       MOVE +1                 TO TRACK-IX                                
284300       PERFORM UNTIL TRACK-IX > MAX-TRACK-IX                              
284400         MOVE SPACE            TO WF-IDTRACK     (TRACK-IX)               
284500         MOVE ZERO             TO WF-KVTRACK-LEV (TRACK-IX)               
284600         ADD +1                TO TRACK-IX                                
284700       END-PERFORM                                                        
284800     END-IF                                                               
284900     .                                                                    
285000     EJECT                                                                
285100 GABA-HAMTA-ARTTEXT SECTION.                                              
285200     MOVE 'GABA-HAMTA-ARTTEXT'  TO WS-SEKTION                             
285300                                                                          
285400     MOVE BRAD-IDARTNR          TO W-IDARTNR                              
285500                                   W-IDARTNR-D3                           
285600                                   W-IDARTNR-WDK701                       
285700     MOVE WS-IDSKYLT            TO W-IDSKYLT                              
285800     IF BGMT-IDDISTR NOT = GMT-IDDISTR                                    
285900       MOVE SPACE               TO WS-IDSKYLT                             
286000     END-IF                                                               
286100     IF WS-IDSKYLT = SPACE                                                
286200       PERFORM GABAA-HAMTA-KDVALISO                                       
           END-IF                                                               
****                                                                            
286500     PERFORM IMS-GU-WDD311                                                
286600     IF SEGMENT-FINNS                                                     
286700       MOVE TEXT-BEART          TO WF-BEART                               
           END-IF                                                               
286900     .                                                                    
287000     EJECT                                                                
287100 GABAA-HAMTA-KDVALISO SECTION.                                            
287200     MOVE 'GABAA-HAMTA-KDVALISO'  TO WS-SEKTION                           
287300                                                                          
287400     IF BGMT-IDDISTR = GMT-IDDISTR AND                                    
287500        BGMT-IDKUNDNR = GMT-IDKUNDNR                                      
287600       MOVE BET-KDVALISO        TO WF-KDVALISO                            
287700       MOVE GMT-IDSKYLT         TO WS-IDSKYLT                             
287800                                   W-IDSKYLT                              
287900*                                                                         
288000*      'SEK' I KDVALISO (ANDRA FLÖDET) GÄLLER FÖR:                        
288100*      *STUDS-REFILLDISTR                                                 
288200*      *STUDS-IMPORTÖRER                                                  
288300*                                                                         
288400       IF (DIST35-NONVCC-NONVCC-REFILL AND                                
288500           SHIP-KDFAKSTA-EXP = 2)                                         
288600                OR                                                        
288700          (SHIP-IDDC-EXP = WC-CDC-SE   AND                                
288800           SHIP-KDFAKSTA-EXP = 2)                                         
288900                                                                          
289000         MOVE 'SEK'             TO WF-KDVALISO                            
             END-IF                                                             
           ELSE                                                                 
289300       MOVE BGMT-IDDISTR        TO W-IDDISTR-WDB2                         
289400                                   W-IDDISTR-WDB2-MIN                     
289500                                   W-IDDISTR-WDB2-MAX                     
289600       MOVE BGMT-IDKUNDNR       TO W-IDKUNDNR-WDB2                        
289700       PERFORM IMS-GU-GMTA-WDB201                                         
289800       IF SEGMENT-FINNS                                                   
289900         CONTINUE                                                         
290000       ELSE                                                               
290100         PERFORM IMS-GU-WDB201                                            
             END-IF                                                             
290300       MOVE GMT-IDPARTNR        TO W-WDB1-IDPARTNR                        
290400       MOVE GMT-IDFTG           TO W-WDB1-IDFTG                           
290500       MOVE GMT-IDSKYLT         TO WS-IDSKYLT                             
290600                                   W-IDSKYLT                              
290700       PERFORM IMS-GU-WDB1-WDB101                                         
290800       IF SEGMENT-FINNS                                                   
290900         MOVE BET-KDVALISO      TO WF-KDVALISO                            
291000         MOVE BET-IDMARKBO      TO WS-IDMARKBO                            
291100       ELSE                                                               
291200         MOVE 'SEK'             TO WF-KDVALISO                            
291300       END-IF                                                             
291400*                                                                         
291500*      'SEK' I KDVALISO (ANDRA FLÖDET) GÄLLER FÖR:                        
291600*      *STUDS-REFILLDISTR                                                 
291700*      *STUDS-IMPORTÖRER                                                  
291800*                                                                         
291900       IF (DIST35-NONVCC-NONVCC-REFILL AND                                
292000           SHIP-KDFAKSTA-EXP = 2)                                         
292100                OR                                                        
292200          (SHIP-IDDC-EXP = WC-CDC-SE   AND                                
292300           SHIP-KDFAKSTA-EXP = 2)                                         
292400                                                                          
292500         MOVE 'SEK'             TO WF-KDVALISO                            
             END-IF                                                             
           END-IF                                                               
292800                                                                          
292900     .                                                                    
293000     EJECT                                                                
293100 GABC-ATERSKRIV-WDE411  SECTION.                                          
293200     MOVE 'GABC-ATERSKRIV-WDE411'   TO WS-SEKTION                         
293300                                                                          
293400     IF DIST79-DEALER-PRICE                                               
293500       MOVE ZERO              TO W-SKRIV                                  
293600       MOVE BRAD-IDPURAD      TO W-IDPURAD-BSEQ                           
293700       PERFORM IMS-GHU-WDE411-BSEQ                                        
293800       IF SEGMENT-FINNS                                                   
293900         IF WF-PRARTNTO NOT = WY-BRAD-PRARTNTO-LOC                        
294000           MOVE WF-PRARTNTO   TO ORAD-PRARTNTO-LOC                        
294100           MOVE ZERO          TO ORAD-PRARTNTO-LOCPREL                    
294200           MOVE +1            TO W-SKRIV                                  
294300         END-IF                                                           
294400         IF ORAD-PRARTNTO-LOC = ZERO                                      
294500           MOVE WF-PRARTNTO   TO ORAD-PRARTNTO-LOC                        
294600           MOVE ZERO          TO ORAD-PRARTNTO-LOCPREL                    
294700           MOVE +1            TO W-SKRIV                                  
294800         END-IF                                                           
294900         IF WF-PRARTBTO NOT = WY-BRAD-PRARTBTO-LOC                        
295000           MOVE WF-PRARTBTO   TO ORAD-PRARTBTO-LOC                        
295100           MOVE +1            TO W-SKRIV                                  
295200         END-IF                                                           
295300         IF WF-REARTRAB NOT = WY-BRAD-RERAB                               
295400           MOVE WF-REARTRAB   TO ORAD-RERAB                               
295500           MOVE +1            TO W-SKRIV                                  
295600         END-IF                                                           
295700         IF WF-KDVAT    NOT = ORAD-KDVAT                                  
295800           MOVE WF-KDVAT      TO ORAD-KDVAT                               
295900           MOVE +1            TO W-SKRIV                                  
296000         END-IF                                                           
296100         IF BRAD-KDPRTYP   NOT = ORAD-KDPRTYP                             
296200           IF BRAD-KDPRTYP = 'T'                                          
296300             MOVE BRAD-KDPRTYP  TO ORAD-KDPRTYP                           
296400             MOVE +1            TO W-SKRIV                                
296500           END-IF                                                         
296600         END-IF                                                           
             END-IF                                                             
           ELSE                                                                 
296900       IF DIST79-ECOM-PRICE                                               
297000         MOVE ZERO            TO W-SKRIV                                  
297100         MOVE BRAD-IDPURAD    TO W-IDPURAD-BSEQ                           
297200         PERFORM IMS-GHU-WDE411-BSEQ                                      
297300         IF SEGMENT-FINNS                                                 
297400           IF WF-PRARTNTO NOT = WY-BRAD-PRARTNTO-LOC                      
297500             MOVE WF-PRARTNTO TO ORAD-PRARTNTO-LOC                        
297600             MOVE ZERO        TO ORAD-PRARTNTO-LOCPREL                    
297700             MOVE +1          TO W-SKRIV                                  
297800           END-IF                                                         
297900           IF ORAD-PRARTNTO-LOC = ZERO                                    
298000             MOVE WF-PRARTNTO TO ORAD-PRARTNTO-LOC                        
298100             MOVE ZERO        TO ORAD-PRARTNTO-LOCPREL                    
298200             MOVE +1          TO W-SKRIV                                  
298300           END-IF                                                         
298400                                                                          
298500         END-IF                                                           
298600       ELSE                                                               
298700         MOVE ZERO        TO W-SKRIV                                      
298800         MOVE BRAD-IDPURAD TO W-IDPURAD-BSEQ                              
298900         PERFORM IMS-GHU-WDE411-BSEQ                                      
299000         IF SEGMENT-FINNS                                                 
299100           IF WF-KDVAT NOT = ORAD-KDVAT                                   
299200             MOVE WF-KDVAT TO ORAD-KDVAT                                  
299300             MOVE +1      TO W-SKRIV                                      
299400           END-IF                                                         
299500         END-IF                                                           
299600       END-IF                                                             
           END-IF                                                               
299800**** THIS IS DONE BOTH FOR DNI AND OTHERS                                 
299900     IF W-SKRIV = +1                                                      
300000       PERFORM IMS-REPL-WDE411                                            
300100     END-IF                                                               
300200     .                                                                    
300300     EJECT                                                                
300400 GABD-RAEKNA-AVERAGE-COST SECTION.                                        
300500     MOVE 'GABD-RAEKNA-AVERAGE-COST' TO WS-SEKTION                        
300600                                                                          
300700     IF SHIP-IDDC-EXP = WC-NDC-IN                                         
300800       MOVE SHIP-IDDC-EXP          TO AVG-IDDC                            
300900       MOVE 012                    TO AVG-KDCALL                          
           ELSE                                                                 
301100       MOVE SHIP-IDDC-EXP          TO AVG-IDDC                            
301200       MOVE 011                    TO AVG-KDCALL                          
           END-IF                                                               
301400**** AVERAGE COST BERÄKNING SKALL TA HÄNSYN TILL EFR                      
301500     COMPUTE AVG-KVLS-OLD = SLAG-KVLS + SLAG-KVEFRS                       
****                                                                            
301700     MOVE BRAD-PRARTNTO          TO AVG-PRARTNTO                          
301800     MOVE BRAD-KVLEVART          TO AVG-KVANTMOT                          
301900     MOVE +0                     TO AVG-PRKURS                            
302000     MOVE SLAG-PRAVCOST          TO AVG-PRAVCOST-OLD                      
302100     MOVE SPACE                  TO AVG-KDVALISO                          
302200     MOVE +0                     TO AVG-KVLEVART                          
302300     MOVE CLAG-KDPSLLOC          TO AVG-KDPSLLOC                          
302400     MOVE ART-IDFKNGRP           TO AVG-IDFKNGRP                          
302500     MOVE AVG-KDPRODSL           TO AVG-KDPRODSL                          
302600     MOVE +0                     TO AVG-PRAVCOST-NEW                      
302700     MOVE SPACE                  TO AVG-KDSVAR                            
302800     MOVE DAGENS-DATUM(1:2)      TO AVG-TIAA                              
302900     MOVE DAGENS-DATUM(3:2)      TO AVG-TIMM                              
****                                                                            
303100     CALL W510AVG USING AVG-W510AVG 9305-PCB                              
303200                        AVG-WDB6-PCB                                      
303300     IF AVG-KDSVAR = SPACE                                                
303400       MOVE AVG-PRAVCOST-NEW     TO WS-PRAVCOST                           
           ELSE                                                                 
303600       IF AVG-KDSVAR = '4'                                                
303700         MOVE SLAG-PRAVCOST      TO WS-PRAVCOST                           
303800       ELSE                                                               
303900         STRING 'FEL FRÅN W510AVG ' AVG-KDSVAR                            
304000          DELIMITED BY SIZE INTO FELTEXT                                  
304100           CALL FELLOG                                                    
             END-IF                                                             
           END-IF                                                               
304400     .                                                                    
304500     EJECT                                                                
304600 GABE-SPARA-AVCOST-WDE411  SECTION.                                       
304700     MOVE 'GABE-SPARA-AVCOST-WDE' TO WS-SEKTION                           
304800                                                                          
304900     MOVE ZERO                    TO W-SKRIV                              
305000     MOVE BRAD-IDPURAD            TO W-IDPURAD-BSEQ                       
305100     PERFORM IMS-GHU-WDE411-BSEQ                                          
305200     IF SEGMENT-FINNS                                                     
305300*      IF SLAG-PRAVCOST = ZERO                                            
305400*        OM PRAVCOST SAKNADES PÅ WDK711 -->                               
305500*        --> TAS SAMMA SOM REDAN FINNS PÅ ORDERRADEN                      
305600*        MOVE ORAD-PRAVCOST       TO WF-PRARTNTO                          
305700*                                    WF-PRARTBTO                          
305800*      END-IF                                                             
305900       COMPUTE ORAD-PRAVCOST = WS-PRAVCOST                                
306000       MOVE +1                    TO W-SKRIV                              
306100     END-IF                                                               
306200     IF W-SKRIV = +1                                                      
306300       PERFORM IMS-REPL-WDE411                                            
306400     END-IF                                                               
306500     .                                                                    
306600     EJECT                                                                
306700                                                                          
306800 GB-FRAKT-OSV  SECTION.                                                   
306900     MOVE 'GB-FRAKT-OSV'         TO WS-SEKTION                            
307000                                                                          
307100     IF DCS-IDDC NOT = BILL-IDDC                                          
307200       MOVE BILL-IDDC            TO W-IDDC-B6                             
307300                                    W-IDDC-WDK711                         
307400       PERFORM IMS-GU-WDB601                                              
307500     END-IF                                                               
307600     IF (DIST35-NONVCC-NONVCC-REFILL AND SHIP-KDFAKSTA-EXP = 2)           
307700         OR                                                               
307800        (SHIP-IDDC-EXP = WC-CDC-SE AND SHIP-KDFAKSTA-EXP = 2)             
307900       IF  BGMT-PRFRAKT  = ZERO  AND                                      
308000           BGMT-PRFOERS  = ZERO  AND                                      
308100           BGMT-PRLEGKST = ZERO  AND                                      
308200           BGMT-PREMBHNT = ZERO  AND                                      
308300           BGMT-PRAVDRAG = ZERO                                           
308400         CONTINUE                                                         
308500       ELSE                                                               
308600*---     HÄMTA KURS FÖR OMRÄKNING TILL SEK -> DETTA BEHÖVS FÖR            
308700*        DISTRIKT MED 'STUDS'FLÖDE (DUBBELFAKT) EFTERSOM FAKT.            
308800*        NR 2 ÄR I SEK OCH TILLÄGGSK. ÄR I EN ANNAN VALUTA                
308900*        IMPORTÖRSTUDS SKALL HA SÄNDANDE DC VALUTA                        
309000*        * GÄLLER EJ VOR-STUDS FRÅN DC 11 TILL CN/IN/KR MM                
309100*                                                                         
309200*---     FETCH CURRRENCY FOR CONV. TO SEK -> THIS IS NEDEED FOR           
309300*        DISTR. WITH BOUNCE FLOW (DUBBEL INV) BECAUSE THE 2:ND            
309400*        INV. IS IN SEK AND THE ADDITINAL COSTS ARE IN OTHER CURR.        
309500*        IMPORTERS BOUNCE SHOULD HAVE THE SENDING DC CURR.                
309600*        * NOT VALID FOR VOR FROM DC 11 TO  CN/IN/KR, ETC                 
309700*                                                                         
309800         IF SHIP-KDFAKSTA-EXP = 2                                         
309900           IF SHIP-IDDC-EXP = WC-CDC-SE                                   
310000             MOVE DCS-KDVALISO          TO CURR-KDVALISO-ROW              
310100           END-IF                                                         
310200         END-IF                                                           
310300         IF DIST35-NDCCN-NONVCC-REFILL                                    
310400           MOVE 'CNY'                  TO CURR-KDVALISO-ROW               
310500         END-IF                                                           
310600         IF DIST35-NDCUS-NONVCC-REFILL                                    
310700           MOVE 'USD'                  TO CURR-KDVALISO-ROW               
310800         END-IF                                                           
310900         IF DIST35-NDCKR-NDC-REFILL                                       
311000           MOVE 'KRW'                  TO CURR-KDVALISO-ROW               
311100         END-IF                                                           
311200         IF DIST35-NDCIN-NDC-REFILL                                       
311300           MOVE 'INR'                  TO CURR-KDVALISO-ROW               
311400         END-IF                                                           
311500         IF DIST35-NDCMY-NDC-REFILL                                       
311600           MOVE 'MYR'                  TO CURR-KDVALISO-ROW               
311700         END-IF                                                           
311800         IF DIST35-NDCTH-NDC-REFILL                                       
311900           MOVE 'THB'                  TO CURR-KDVALISO-ROW               
312000         END-IF                                                           
312100         IF DIST35-NDCTW-NDC-REFILL                                       
312200           MOVE 'TWD'                  TO CURR-KDVALISO-ROW               
312300         END-IF                                                           
312400                                                                          
312500         MOVE W-DATE-AAMM               TO CURR-TIAAMM                    
312600         MOVE WS-KDVALISO-HUV           TO CURR-KDVALISO-HUV              
312700         MOVE 'M'                       TO CURR-KDVALTYP                  
312800                                                                          
312900         CALL W510CURR USING CURR-W510CURR WDG2-PCB                       
313000         IF CURR-KDSVAR = ' '                                             
313100            CONTINUE                                                      
313200         ELSE                                                             
313300            MOVE 1                      TO CURR-PRKURS-NEW                
313400         END-IF                                                           
313500                                                                          
             END-IF                                                             
           END-IF                                                               
313800*                                                                         
313900     IF BGMT-PRFRAKT NOT = ZERO                                           
314000       IF DCS-NDC-NA        AND NOT                                       
314100          DIST35-NONVCC-REFILL AND                                        
314200         (SHIP-KDFAKSTA-EXP = SPACE OR ZERO)                              
314300*                                                                         
314400*         *FRAKTBELOPP FÖR USA I DET VANLIGA FLÖDET ÄR I DOLLAR           
314500*          FÖR USA ÄR I DOLLAR SKALL EJ TILL BILLIT                       
314600*         *GÄLLER EJ:                                                     
314700*          -REFILLDISTR. MED 'STUDS' FLÖDE (DUBBELFAKT.)                  
314800*          -FLÖDET USA --> CDC                                            
314900*          -FLÖDET USA --> IMPORTÖR                                       
315000         CONTINUE                                                         
315100       ELSE                                                               
315200         IF (SHIP-KDFAKSTA-EXP = 2) AND                                   
315300            (SHIP-IDDC-EXP NOT = WC-CDC-SE)                               
315400*            TILLÄGGSKOSTNADER FÖR VOR DC11->CN/IN/KR                     
315500*            SKALL INTE SKICKAS I 2-A FAKT. FRÅN DCXX->DEALER             
315600           CONTINUE                                                       
315700         ELSE                                                             
315800           MOVE NEJ              TO TRACK-SW                              
315900           PERFORM GA-RADINFO                                             
316000           MOVE BGMT-PRFRAKT     TO WF-PRARTNTO                           
316100                                    WF-PRARTBTO                           
316200* TO REPLACE WITH THIS 88-LEVEL WHEN ALL THE DC'S ARE                     
316300* IN THE SCOPE.                                                           
316400*          IF (DIST35-NONVCC-NONVCC-REFILL AND                            
316500*                                                                         
316600           IF ((DIST35-NDCCN-NONVCC-REFILL OR                             
316700                DIST35-NDCUS-NONVCC-REFILL OR                             
316800                DIST35-NDCKR-NDC-REFILL    OR                             
316900                DIST35-NDCIN-NDC-REFILL    OR                             
317000                DIST35-NDCMY-NDC-REFILL    OR                             
317100                DIST35-NDCTH-NDC-REFILL    OR                             
317200                DIST35-NDCTW-NDC-REFILL)                                  
317300                AND                                                       
317400                SHIP-KDFAKSTA-EXP = 2)                                    
317500                                                                          
317600                       OR                                                 
317700              (SHIP-KDFAKSTA-EXP = 2       AND                            
317800               SHIP-IDDC-EXP = WC-CDC-SE)                                 
317900*                                                                         
318000             COMPUTE WF-PRARTNTO ROUNDED = BGMT-PRFRAKT *                 
318100                                           CURR-PRKURS-NEW                
318200             MOVE WF-PRARTNTO    TO WF-PRARTBTO                           
318300           END-IF                                                         
318400           MOVE 'FREIGHT'        TO WF-BEART                              
318500           MOVE ZERO             TO WF-IDSTATNR                           
318600                                       WF-VKARTNTO                        
318700           MOVE +1               TO WF-KVBEART                            
318800                                       WF-KVLEVART                        
318900           MOVE SPACES           TO WF-KDARTURS                           
319000                                    WF-IDARTNR-FINANCE                    
319100*                                   WF-IDEXCUST(2)                        
319200                                    WF-IDOPTION(2)                        
319300                                    WF-IDOPTION(3)                        
319400                                    WF-IDOPTION(4)                        
319500                                    WF-IDOPTION(5)                        
319600                                    WF-IDLEVNR-ART                        
319700           MOVE NEJ              TO WF-FLSPECPR                           
319800           MOVE NEJ              TO WF-FLPCOO                             
319900           MOVE '999999'         TO WF-IDSEQ(1)                           
320000           MOVE '999999'         TO WF-IDSEQ(2)                           
320100           MOVE '99999999'       TO WF-IDSEQ(3)                           
320200                                                                          
320300           PERFORM S02-PUT-RAD-WZ01                                       
320400           MOVE JA               TO TRACK-SW                              
320500         END-IF                                                           
320600       END-IF                                                             
320700     END-IF                                                               
320800*                                                                         
320900     IF BGMT-PRFOERS NOT = ZERO                                           
321000       IF (SHIP-KDFAKSTA-EXP = 2) AND                                     
321100          (SHIP-IDDC-EXP NOT = WC-CDC-SE)                                 
321200*          TILLÄGGSKOSTNADER FÖR VOR DC11->CN/IN/KR                       
321300*          SKALL INTE SKICKAS I 2.A FAKT. FRÅN DCXX->DEALER               
321400         CONTINUE                                                         
321500       ELSE                                                               
321600         MOVE NEJ                TO TRACK-SW                              
321700         PERFORM GA-RADINFO                                               
321800         MOVE BGMT-PRFOERS       TO WF-PRARTNTO                           
321900                                    WF-PRARTBTO                           
322000* TO REPLACE WITH THIS 88-LEVEL WHEN ALL THE DC'S ARE                     
322100* IN THE SCOPE.                                                           
322200*          IF (DIST35-NONVCC-NONVCC-REFILL AND                            
322300*                                                                         
322400         IF ((DIST35-NDCCN-NONVCC-REFILL OR                               
322500              DIST35-NDCUS-NONVCC-REFILL OR                               
322600              DIST35-NDCKR-NDC-REFILL    OR                               
322700              DIST35-NDCIN-NDC-REFILL    OR                               
322800              DIST35-NDCMY-NDC-REFILL    OR                               
322900              DIST35-NDCTH-NDC-REFILL    OR                               
323000              DIST35-NDCTW-NDC-REFILL)                                    
323100              AND                                                         
323200              SHIP-KDFAKSTA-EXP = 2)                                      
323300                                                                          
323400                   OR                                                     
323500            (SHIP-KDFAKSTA-EXP = 2       AND                              
323600             SHIP-IDDC-EXP = WC-CDC-SE)                                   
323700                                                                          
323800           COMPUTE WF-PRARTNTO ROUNDED = BGMT-PRFOERS *                   
323900                                         CURR-PRKURS-NEW                  
324000           MOVE WF-PRARTNTO      TO WF-PRARTBTO                           
324100         END-IF                                                           
324200         MOVE 'INSURANCE'        TO WF-BEART                              
324300         MOVE ZERO               TO WF-IDSTATNR                           
324400                                      WF-VKARTNTO                         
324500         MOVE +1                 TO WF-KVBEART                            
324600                                      WF-KVLEVART                         
324700         MOVE SPACES             TO WF-KDARTURS                           
324800                                    WF-IDARTNR-FINANCE                    
324900                                    WF-IDOPTION(2)                        
325000                                    WF-IDOPTION(3)                        
325100                                    WF-IDOPTION(4)                        
325200                                    WF-IDOPTION(5)                        
325300                                    WF-IDLEVNR-ART                        
325400         MOVE NEJ                TO WF-FLSPECPR                           
325500         MOVE NEJ                TO WF-FLPCOO                             
325600         MOVE '999999'           TO WF-IDSEQ(1)                           
325700         MOVE '999999'           TO WF-IDSEQ(2)                           
325800         MOVE '99999999'         TO WF-IDSEQ(3)                           
325900                                                                          
326000         PERFORM S02-PUT-RAD-WZ01                                         
326100         MOVE JA                 TO TRACK-SW                              
326200       END-IF                                                             
326300     END-IF                                                               
326400*                                                                         
326500     IF BGMT-PRLEGKST NOT = ZERO                                          
326600       IF (SHIP-KDFAKSTA-EXP = 2) AND                                     
326700          (SHIP-IDDC-EXP NOT = WC-CDC-SE)                                 
326800*          TILLÄGGSKOSTNADER FÖR VOR DC11->CN/IN/KR                       
326900*          SKALL INTE SKICKAS I 2:A FAKT. FRÅN DCXX->DEALER               
327000         CONTINUE                                                         
327100       ELSE                                                               
327200         MOVE NEJ                    TO TRACK-SW                          
327300         PERFORM GA-RADINFO                                               
327400         MOVE BGMT-PRLEGKST          TO WF-PRARTNTO                       
327500                                        WF-PRARTBTO                       
327600* TO REPLACE WITH THIS 88-LEVEL WHEN ALL THE DC'S ARE                     
327700* IN THE SCOPE.                                                           
327800*          IF (DIST35-NONVCC-NONVCC-REFILL AND                            
327900*                                                                         
328000         IF ((DIST35-NDCCN-NONVCC-REFILL OR                               
328100              DIST35-NDCUS-NONVCC-REFILL OR                               
328200              DIST35-NDCKR-NDC-REFILL    OR                               
328300              DIST35-NDCIN-NDC-REFILL    OR                               
328400              DIST35-NDCMY-NDC-REFILL    OR                               
328500              DIST35-NDCTH-NDC-REFILL    OR                               
328600              DIST35-NDCTW-NDC-REFILL)                                    
328700              AND                                                         
328800              SHIP-KDFAKSTA-EXP = 2)                                      
328900                                                                          
329000                    OR                                                    
329100            (SHIP-KDFAKSTA-EXP = 2       AND                              
329200             SHIP-IDDC-EXP = WC-CDC-SE)                                   
329300                                                                          
329400           COMPUTE WF-PRARTNTO ROUNDED = BGMT-PRLEGKST *                  
329500                                         CURR-PRKURS-NEW                  
329600           MOVE WF-PRARTNTO          TO WF-PRARTBTO                       
329700         END-IF                                                           
329800         MOVE 'LEGAL'                TO WF-BEART                          
329900         MOVE ZERO                   TO WF-IDSTATNR                       
330000                                        WF-VKARTNTO                       
330100         MOVE +1                     TO WF-KVBEART                        
330200                                        WF-KVLEVART                       
330300         MOVE SPACES                 TO WF-KDARTURS                       
330400                                        WF-IDARTNR-FINANCE                
330500                                        WF-IDOPTION(2)                    
330600                                        WF-IDOPTION(3)                    
330700                                        WF-IDOPTION(4)                    
330800                                        WF-IDOPTION(5)                    
330900                                        WF-IDLEVNR-ART                    
331000         MOVE NEJ                    TO WF-FLSPECPR                       
331100         MOVE NEJ                    TO WF-FLPCOO                         
331200         MOVE '999999'               TO WF-IDSEQ(1)                       
331300         MOVE '999999'               TO WF-IDSEQ(2)                       
331400         MOVE '99999999'             TO WF-IDSEQ(3)                       
331500                                                                          
331600         PERFORM S02-PUT-RAD-WZ01                                         
331700         MOVE JA                     TO TRACK-SW                          
331800       END-IF                                                             
331900     END-IF                                                               
332000                                                                          
332100     IF BGMT-PREMBHNT NOT = ZERO                                          
332200       IF (SHIP-KDFAKSTA-EXP = 2) AND                                     
332300          (SHIP-IDDC-EXP NOT = WC-CDC-SE)                                 
332400*          TILLÄGGSKOSTNADER FÖR VOR DC11->CN/IN/KR                       
332500*          SKALL INTE SKICKAS I 2:A FAKT. FRÅN DCXX->DEALER               
332600         CONTINUE                                                         
332700       ELSE                                                               
332800         MOVE NEJ                    TO TRACK-SW                          
332900         PERFORM GA-RADINFO                                               
333000         MOVE BGMT-PREMBHNT          TO WF-PRARTNTO                       
333100                                        WF-PRARTBTO                       
333200* TO REPLACE WITH THIS 88-LEVEL WHEN ALL THE DC'S ARE                     
333300* IN THE SCOPE.                                                           
333400*          IF (DIST35-NONVCC-NONVCC-REFILL AND                            
333500*                                                                         
333600         IF ((DIST35-NDCCN-NONVCC-REFILL OR                               
333700              DIST35-NDCUS-NONVCC-REFILL OR                               
333800              DIST35-NDCKR-NDC-REFILL    OR                               
333900              DIST35-NDCIN-NDC-REFILL    OR                               
334000              DIST35-NDCMY-NDC-REFILL    OR                               
334100              DIST35-NDCTH-NDC-REFILL    OR                               
334200              DIST35-NDCTW-NDC-REFILL)                                    
334300              AND                                                         
334400              SHIP-KDFAKSTA-EXP = 2)                                      
334500                    OR                                                    
334600             (SHIP-IDDC-EXP = WC-CDC-SE                                   
334700              AND                                                         
334800              SHIP-KDFAKSTA-EXP = 2)                                      
334900                                                                          
335000                                                                          
335100           COMPUTE WF-PRARTNTO ROUNDED = BGMT-PREMBHNT *                  
335200                                         CURR-PRKURS-NEW                  
335300           MOVE WF-PRARTNTO          TO WF-PRARTBTO                       
335400**** CHECK IF WE SHOULD ADD FREIGHT ON THE SECOND INVOICE                 
335500*THIS FUNCTION IS VALID FOR CHINA NOW.OTHER FLOWS MAY BE INCLUDED         
335600*IN FUTURE                                                                
335700*FREIGHT & INSURANCE WILL BE ONLY FOR 1ST CUSTOMER IN AN INVOICE,         
335800*IF SHIPMENT HAS MORE THAN 1 CUSTOMER.(CONTROLLED BY ADDL-COST-SW)        
335900           IF DIST35-NDCCN-NONVCC-REFILL AND                              
336000             (ADDL-COST-SW = JA)                                          
336100             PERFORM S09-FREIGHT-ON-SECOND-INVOICE                        
336200           END-IF                                                         
336300         END-IF                                                           
336400                                                                          
336500**** LYNK EMB SHOULD HAVE SERVICE FEE AS TEXT                             
336600*        IF SHIP-IDSYSTEM = 'LYNK' --> FUNGERAR EJ I MIXADE SKEPP.        
336700*                                  --> MÅSTE TÄNKA UT ANNAT!              
336800*                                  --> MEN DIST79 FUNKAR JUST NU.         
336900*        IF DIST79-NON-SEK                                                
337000*          MOVE 'SERVICE FEE'        TO WF-BEART                          
337100*        ELSE                                                             
337200           MOVE 'PACKING & HANDLING' TO WF-BEART                          
337300*        END-IF                                                           
337400         MOVE ZERO                   TO WF-IDSTATNR                       
337500                                        WF-VKARTNTO                       
337600         MOVE +1                     TO WF-KVBEART                        
337700                                        WF-KVLEVART                       
337800         MOVE SPACES                 TO WF-KDARTURS                       
337900                                        WF-IDARTNR-FINANCE                
338000                                        WF-IDOPTION(2)                    
338100                                        WF-IDOPTION(3)                    
338200                                        WF-IDOPTION(4)                    
338300                                        WF-IDOPTION(5)                    
338400                                        WF-IDLEVNR-ART                    
338500         MOVE NEJ                    TO WF-FLSPECPR                       
338600         MOVE NEJ                    TO WF-FLPCOO                         
338700         MOVE '999999'               TO WF-IDSEQ(1)                       
338800         MOVE '999999'               TO WF-IDSEQ(2)                       
338900         MOVE '99999999'             TO WF-IDSEQ(3)                       
339000                                                                          
339100         PERFORM S02-PUT-RAD-WZ01                                         
339200         MOVE JA                     TO TRACK-SW                          
339300**** IF WE SHOULD ADD FREIGHT OR INSURANCE ON THE SECOND INVOICE          
339400         IF ADDL-COST-SW = JA                                             
339500            IF WS-FREIGHT > ZERO                                          
339600              COMPUTE W-RAD-RAKNARE    = W-RAD-RAKNARE  +  1              
339700              MOVE W-RAD-RAKNARE         TO WF-IDREFRAD                   
339800              MOVE WS-FREIGHT           TO WF-PRARTNTO                    
339900              MOVE WF-PRARTNTO          TO WF-PRARTBTO                    
340000              MOVE 'FREIGHT'            TO WF-BEART                       
340100              PERFORM S02-PUT-RAD-WZ01                                    
340200              MOVE WS-FREIGHT           TO WS-FREIGHT-SAVE                
340300              MOVE ZERO                 TO WS-FREIGHT                     
340400            END-IF                                                        
340500            IF WS-INSURANCE > ZERO                                        
340600              COMPUTE W-RAD-RAKNARE    = W-RAD-RAKNARE  +  1              
340700              MOVE W-RAD-RAKNARE         TO WF-IDREFRAD                   
340800              MOVE WS-INSURANCE         TO WF-PRARTNTO                    
340900              MOVE WF-PRARTNTO          TO WF-PRARTBTO                    
341000              MOVE 'INSURANCE'          TO WF-BEART                       
341100              PERFORM S02-PUT-RAD-WZ01                                    
341200              MOVE WS-INSURANCE         TO WS-INSURANCE-SAVE              
341300              MOVE ZERO                 TO WS-INSURANCE                   
341400            END-IF                                                        
341500         END-IF                                                           
341600*AFTER THE FRIEGHT & INSURANCE ARE SENT TO BILL-IT FOR THE 1ST            
341700*CUSTOMER, WE STOP SENDING FOR OTHER CUSTOMERS                            
341800         IF ADDL-COST-SW = JA     AND                                     
341900           (WS-FREIGHT-SAVE   > ZERO   OR                                 
342000            WS-INSURANCE-SAVE > ZERO )                                    
342100             MOVE NEJ                TO ADDL-COST-SW                      
342200         END-IF                                                           
342300       END-IF                                                             
342400     END-IF                                                               
342500*                                                                         
342600     IF BGMT-PRAVDRAG NOT = ZERO                                          
342700       IF (SHIP-KDFAKSTA-EXP = 2) AND                                     
342800          (SHIP-IDDC-EXP NOT = WC-CDC-SE)                                 
342900*          TILLÄGGSKOSTNADER FÖR VOR DC11->CN/IN/KR                       
343000*          SKALL INTE SKICKAS I ANDRA FAKT. FRÅN DCXX->DEALER             
343100         CONTINUE                                                         
343200       ELSE                                                               
343300         MOVE NEJ                    TO TRACK-SW                          
343400         PERFORM GA-RADINFO                                               
343500         IF BGMT-PRAVDRAG > ZERO                                          
343600            COMPUTE BGMT-PRAVDRAG =                                       
343700                    0 - BGMT-PRAVDRAG                                     
343800         END-IF                                                           
343900         MOVE BGMT-PRAVDRAG          TO WF-PRARTNTO                       
344000                                        WF-PRARTBTO                       
344100* TO REPLACE WITH THIS 88-LEVEL WHEN ALL THE DC'S ARE                     
344200* IN THE SCOPE.                                                           
344300*          IF (DIST35-NONVCC-NONVCC-REFILL AND                            
344400*                                                                         
344500         IF ((DIST35-NDCCN-NONVCC-REFILL OR                               
344600              DIST35-NDCUS-NONVCC-REFILL OR                               
344700              DIST35-NDCKR-NDC-REFILL    OR                               
344800              DIST35-NDCIN-NDC-REFILL    OR                               
344900              DIST35-NDCMY-NDC-REFILL    OR                               
345000              DIST35-NDCTH-NDC-REFILL    OR                               
345100              DIST35-NDCTW-NDC-REFILL)                                    
345200              AND                                                         
345300              SHIP-KDFAKSTA-EXP = 2)                                      
345400                                                                          
345500                    OR                                                    
345600             (SHIP-KDFAKSTA-EXP = 2      AND                              
345700              SHIP-IDDC-EXP = WC-CDC-SE)                                  
345800                                                                          
345900           COMPUTE WF-PRARTNTO ROUNDED = BGMT-PRAVDRAG *                  
346000                                         CURR-PRKURS-NEW                  
346100           MOVE WF-PRARTNTO          TO WF-PRARTBTO                       
346200         END-IF                                                           
346300         MOVE 'REDUCTION'            TO WF-BEART                          
346400         MOVE ZERO                   TO WF-IDSTATNR                       
346500                                        WF-VKARTNTO                       
346600         MOVE +1                     TO WF-KVBEART                        
346700                                        WF-KVLEVART                       
346800         MOVE SPACES                 TO WF-KDARTURS                       
346900                                        WF-IDARTNR-FINANCE                
347000                                        WF-IDOPTION(2)                    
347100                                        WF-IDOPTION(3)                    
347200                                        WF-IDOPTION(4)                    
347300                                        WF-IDOPTION(5)                    
347400                                        WF-IDLEVNR-ART                    
347500         MOVE NEJ                    TO WF-FLSPECPR                       
347600         MOVE NEJ                    TO WF-FLPCOO                         
347700         MOVE '999999'               TO WF-IDSEQ(1)                       
347800         MOVE '999999'               TO WF-IDSEQ(2)                       
347900         MOVE '99999999'             TO WF-IDSEQ(3)                       
348000                                                                          
348100         PERFORM S02-PUT-RAD-WZ01                                         
348200         MOVE JA                     TO TRACK-SW                          
348300       END-IF                                                             
348400     END-IF                                                               
348500     .                                                                    
348600     EJECT                                                                
348700 GC-FRAKT-NOLL  SECTION.                                                  
348800     MOVE 'GC-FRAKT-NOLL'            TO WS-SEKTION                        
348900                                                                          
349000     IF DCS-IDDC NOT = BILL-IDDC                                          
349100       MOVE BILL-IDDC                TO W-IDDC-B6                         
349200                                        W-IDDC-WDK711                     
349300       PERFORM IMS-GU-WDB601                                              
349400     END-IF                                                               
349500     IF (DIST35-NONVCC-NONVCC-REFILL AND SHIP-KDFAKSTA-EXP = 2)           
349600         OR                                                               
349700        (SHIP-IDDC-EXP = WC-CDC-SE   AND SHIP-KDFAKSTA-EXP = 2)           
349800                                                                          
349900       IF  NOLL-BGMT-PRFRAKT  = ZERO  AND                                 
350000           NOLL-BGMT-PRFOERS  = ZERO  AND                                 
350100           NOLL-BGMT-PRLEGKST = ZERO  AND                                 
350200           NOLL-BGMT-PREMBHNT = ZERO  AND                                 
350300           NOLL-BGMT-PRAVDRAG = ZERO                                      
350400         CONTINUE                                                         
350500       ELSE                                                               
350600*---     HÄMTA KURS FÖR OMRÄKNING TILL SEK -> DETTA BEHÖVS FÖR            
350700*        DISTRIKT MED 'STUDS'FLÖDE (DUBBELFAKT) EFTERSOM FAKT.            
350800*        NR 2 ÄR I SEK OCH TILLÄGGSK. ÄR I EN ANNAN VALUTA                
350900*        * GÄLLER EJ VOR-STUDS FRÅN DC 11 TILL CN/IN/KR                   
351000*                                                                         
351100*---     FETCH CURRRENCY FOR CONV. TO SEK -> THIS IS NEDEED FOR           
351200*        DISTR. WITH BOUNCE FLOW (DUBBEL INV) BECAUSE THE 2:ND            
351300*        INV. IS IN SEK AND THE ADDITINAL COSTS ARE IN OTHER CURR.        
351400*        IMPORTERS BOUNCE SHOULD HAVE THE SENDING DC CURR.                
351500*        * NOT VALID FOR VOR FROM DC 11 TO  CN/IN/KR, ETC                 
351600*                                                                         
351700         IF SHIP-KDFAKSTA-EXP = 2                                         
351800           IF SHIP-IDDC-EXP = WC-CDC-SE                                   
351900             MOVE DCS-KDVALISO          TO CURR-KDVALISO-ROW              
352000           END-IF                                                         
352100         END-IF                                                           
352200         IF DIST35-NDCCN-NONVCC-REFILL                                    
352300           MOVE 'CNY'                  TO CURR-KDVALISO-ROW               
352400         END-IF                                                           
352500         IF DIST35-NDCUS-NONVCC-REFILL                                    
352600           MOVE 'USD'                  TO CURR-KDVALISO-ROW               
352700         END-IF                                                           
352800         IF DIST35-NDCKR-NDC-REFILL                                       
352900           MOVE 'KRW'                  TO CURR-KDVALISO-ROW               
353000         END-IF                                                           
353100         IF DIST35-NDCIN-NDC-REFILL                                       
353200           MOVE 'INR'                  TO CURR-KDVALISO-ROW               
353300         END-IF                                                           
353400         IF DIST35-NDCMY-NDC-REFILL                                       
353500           MOVE 'MYR'                  TO CURR-KDVALISO-ROW               
353600         END-IF                                                           
353700         IF DIST35-NDCTH-NDC-REFILL                                       
353800           MOVE 'THB'                  TO CURR-KDVALISO-ROW               
353900         END-IF                                                           
354000         IF DIST35-NDCTW-NDC-REFILL                                       
354100           MOVE 'TWD'                  TO CURR-KDVALISO-ROW               
354200         END-IF                                                           
354300                                                                          
354400         MOVE W-DATE-AAMM               TO CURR-TIAAMM                    
354500         MOVE WS-KDVALISO-HUV           TO CURR-KDVALISO-HUV              
354600         MOVE 'M'                       TO CURR-KDVALTYP                  
354700                                                                          
354800         CALL W510CURR USING CURR-W510CURR WDG2-PCB                       
354900         IF CURR-KDSVAR = ' '                                             
355000            CONTINUE                                                      
355100         ELSE                                                             
355200            MOVE 1                      TO CURR-PRKURS-NEW                
355300         END-IF                                                           
355400                                                                          
355500       END-IF                                                             
355600     END-IF                                                               
355700     IF NOLL-BGMT-PRFRAKT NOT = ZERO                                      
355800       IF DCS-NDC-NA        AND NOT                                       
355900          DIST35-NONVCC-REFILL AND                                        
356000         (SHIP-KDFAKSTA-EXP = SPACE OR ZERO)                              
356100*                                                                         
356200*         *FRAKTBELOPP FÖR USA I DET VANLIGA FLÖDET ÄR I DOLLAR           
356300*          FÖR USA ÄR I DOLLAR SKALL EJ TILL BILLIT                       
356400*         *GÄLLER EJ:                                                     
356500*          -REFILLDISTR. MED 'STUDS' FLÖDE (DUBBELFAKT.)                  
356600*          -FLÖDET USA --> CDC                                            
356700*          -FLÖDET USA --> IMPORTÖR                                       
356800         CONTINUE                                                         
356900       ELSE                                                               
357000         IF (SHIP-KDFAKSTA-EXP = 2) AND                                   
357100            (SHIP-IDDC-EXP NOT = WC-CDC-SE)                               
357200*            TILLÄGGSKOSTNADER FÖR VOR DC11->CN/IN/KR                     
357300*            SKALL INTE SKICKAS I 2:A FAKT. FRÅN DCXX->DEALER             
357400           CONTINUE                                                       
357500         ELSE                                                             
357600           MOVE NEJ                  TO TRACK-SW                          
357700           PERFORM GA-RADINFO                                             
357800           MOVE NOLL-BGMT-PRFRAKT    TO WF-PRARTNTO                       
357900                                        WF-PRARTBTO                       
358000* TO REPLACE WITH THIS 88-LEVEL WHEN ALL THE DC'S ARE                     
358100* IN THE SCOPE.                                                           
358200*          IF (DIST35-NONVCC-NONVCC-REFILL AND                            
358300*                                                                         
358400           IF ((DIST35-NDCCN-NONVCC-REFILL OR                             
358500                DIST35-NDCUS-NONVCC-REFILL OR                             
358600                DIST35-NDCKR-NDC-REFILL    OR                             
358700                DIST35-NDCIN-NDC-REFILL    OR                             
358800                DIST35-NDCMY-NDC-REFILL    OR                             
358900                DIST35-NDCTH-NDC-REFILL    OR                             
359000                DIST35-NDCTW-NDC-REFILL)                                  
359100                AND                                                       
359200                SHIP-KDFAKSTA-EXP = 2)                                    
359300                                                                          
359400                     OR                                                   
359500              (SHIP-KDFAKSTA-EXP = 2       AND                            
359600               SHIP-IDDC-EXP = WC-CDC-SE)                                 
359700                                                                          
359800             COMPUTE WF-PRARTNTO ROUNDED = NOLL-BGMT-PRFRAKT *            
359900                                           CURR-PRKURS-NEW                
360000             MOVE WF-PRARTNTO        TO WF-PRARTBTO                       
360100           END-IF                                                         
360200           MOVE 'FREIGHT'            TO WF-BEART                          
360300           MOVE ZERO                 TO WF-IDSTATNR                       
360400                                        WF-VKARTNTO                       
360500           MOVE +1                   TO WF-KVBEART                        
360600                                        WF-KVLEVART                       
360700           MOVE SPACES               TO WF-KDARTURS                       
360800                                        WF-IDARTNR-FINANCE                
360900                                        WF-IDEXCUST(2)                    
361000                                        WF-IDOPTION(2)                    
361100                                        WF-IDOPTION(3)                    
361200                                        WF-IDOPTION(4)                    
361300                                        WF-IDOPTION(5)                    
361400                                        WF-IDSEQ(1)                       
361500                                        WF-IDSEQ(2)                       
361600                                        WF-IDLEVNR-ART                    
361700           MOVE NEJ                  TO WF-FLSPECPR                       
361800           MOVE NEJ                  TO WF-FLPCOO                         
361900           MOVE '99999999'           TO WF-IDSEQ(3)                       
362000                                                                          
362100           PERFORM S02-PUT-RAD-WZ01                                       
362200           MOVE JA                   TO TRACK-SW                          
362300         END-IF                                                           
362400       END-IF                                                             
362500     END-IF                                                               
362600*                                                                         
362700     IF NOLL-BGMT-PRFOERS NOT = ZERO                                      
362800       IF (SHIP-KDFAKSTA-EXP = 2) AND                                     
362900          (SHIP-IDDC-EXP NOT = WC-CDC-SE)                                 
363000*          TILLÄGGSKOSTNADER FÖR VOR DC11->CN/IN/KR                       
363100*          SKALL INTE SKICKAS I 2:A FAKT. FRÅN DCXX->DEALER               
363200         CONTINUE                                                         
363300       ELSE                                                               
363400         MOVE NEJ                    TO TRACK-SW                          
363500         PERFORM GA-RADINFO                                               
363600         MOVE NOLL-BGMT-PRFOERS      TO WF-PRARTNTO                       
363700                                        WF-PRARTBTO                       
363800* TO REPLACE WITH THIS 88-LEVEL WHEN ALL THE DC'S ARE                     
363900* IN THE SCOPE.                                                           
364000*          IF (DIST35-NONVCC-NONVCC-REFILL AND                            
364100*                                                                         
364200         IF ((DIST35-NDCCN-NONVCC-REFILL OR                               
364300              DIST35-NDCUS-NONVCC-REFILL OR                               
364400              DIST35-NDCKR-NDC-REFILL    OR                               
364500              DIST35-NDCIN-NDC-REFILL    OR                               
364600              DIST35-NDCMY-NDC-REFILL    OR                               
364700              DIST35-NDCTH-NDC-REFILL    OR                               
364800              DIST35-NDCTW-NDC-REFILL)                                    
364900              AND                                                         
365000              SHIP-KDFAKSTA-EXP = 2)                                      
365100                                                                          
365200                   OR                                                     
365300            (SHIP-KDFAKSTA-EXP = 2       AND                              
365400             SHIP-IDDC-EXP = WC-CDC-SE)                                   
365500*                                                                         
365600           COMPUTE WF-PRARTNTO ROUNDED = NOLL-BGMT-PRFOERS *              
365700                                         CURR-PRKURS-NEW                  
365800           MOVE WF-PRARTNTO          TO WF-PRARTBTO                       
365900         END-IF                                                           
366000         MOVE 'INSURANCE'            TO WF-BEART                          
366100         MOVE ZERO                   TO WF-IDSTATNR                       
366200                                        WF-VKARTNTO                       
366300         MOVE +1                     TO WF-KVBEART                        
366400                                        WF-KVLEVART                       
366500         MOVE SPACES                 TO WF-KDARTURS                       
366600                                        WF-IDARTNR-FINANCE                
366700                                        WF-IDEXCUST(2)                    
366800                                        WF-IDOPTION(2)                    
366900                                        WF-IDOPTION(3)                    
367000                                        WF-IDOPTION(4)                    
367100                                        WF-IDOPTION(5)                    
367200                                        WF-IDSEQ(1)                       
367300                                        WF-IDSEQ(2)                       
367400                                        WF-IDLEVNR-ART                    
367500         MOVE NEJ                    TO WF-FLSPECPR                       
367600         MOVE NEJ                    TO WF-FLPCOO                         
367700         MOVE '99999999'             TO WF-IDSEQ(3)                       
367800                                                                          
367900         PERFORM S02-PUT-RAD-WZ01                                         
368000         MOVE JA                     TO TRACK-SW                          
368100       END-IF                                                             
368200     END-IF                                                               
368300*                                                                         
368400     IF NOLL-BGMT-PRLEGKST NOT = ZERO                                     
368500       IF (SHIP-KDFAKSTA-EXP = 2) AND                                     
368600          (SHIP-IDDC-EXP NOT = WC-CDC-SE)                                 
368700*          TILLÄGGSKOSTNADER FÖR VOR DC11->CN/IN/KR                       
368800*          SKALL INTE SKICKAS I ANDRA FAKT. FRÅN DCXX->DEALER             
368900         CONTINUE                                                         
369000       ELSE                                                               
369100         MOVE NEJ                    TO TRACK-SW                          
369200         PERFORM GA-RADINFO                                               
369300         MOVE NOLL-BGMT-PRLEGKST     TO WF-PRARTNTO                       
369400                                        WF-PRARTBTO                       
369500* TO REPLACE WITH THIS 88-LEVEL WHEN ALL THE DC'S ARE                     
369600* IN THE SCOPE.                                                           
369700*          IF (DIST35-NONVCC-NONVCC-REFILL AND                            
369800*                                                                         
369900         IF ((DIST35-NDCCN-NONVCC-REFILL OR                               
370000              DIST35-NDCUS-NONVCC-REFILL OR                               
370100              DIST35-NDCKR-NDC-REFILL    OR                               
370200              DIST35-NDCIN-NDC-REFILL    OR                               
370300              DIST35-NDCMY-NDC-REFILL    OR                               
370400              DIST35-NDCTH-NDC-REFILL    OR                               
370500              DIST35-NDCTW-NDC-REFILL)                                    
370600              AND                                                         
370700              SHIP-KDFAKSTA-EXP = 2)                                      
370800                                                                          
370900                    OR                                                    
371000            (SHIP-KDFAKSTA-EXP = 2       AND                              
371100             SHIP-IDDC-EXP = WC-CDC-SE)                                   
371200*                                                                         
371300           COMPUTE WF-PRARTNTO ROUNDED = NOLL-BGMT-PRLEGKST *             
371400                                         CURR-PRKURS-NEW                  
371500           MOVE WF-PRARTNTO          TO WF-PRARTBTO                       
371600         END-IF                                                           
371700         MOVE 'LEGAL'                TO WF-BEART                          
371800         MOVE ZERO                   TO WF-IDSTATNR                       
371900                                        WF-VKARTNTO                       
372000         MOVE +1                     TO WF-KVBEART                        
372100                                         WF-KVLEVART                      
372200         MOVE SPACES                 TO WF-KDARTURS                       
372300                                        WF-IDARTNR-FINANCE                
372400                                        WF-IDEXCUST(2)                    
372500                                        WF-IDOPTION(2)                    
372600                                        WF-IDOPTION(3)                    
372700                                        WF-IDOPTION(4)                    
372800                                        WF-IDOPTION(5)                    
372900                                        WF-IDSEQ(1)                       
373000                                        WF-IDSEQ(2)                       
373100                                        WF-IDLEVNR-ART                    
373200         MOVE NEJ                    TO WF-FLSPECPR                       
373300         MOVE NEJ                    TO WF-FLPCOO                         
373400         MOVE '99999999'             TO WF-IDSEQ(3)                       
373500                                                                          
373600         PERFORM S02-PUT-RAD-WZ01                                         
373700         MOVE JA                     TO TRACK-SW                          
373800       END-IF                                                             
373900     END-IF                                                               
374000                                                                          
374100     IF NOLL-BGMT-PREMBHNT NOT = ZERO                                     
374200       IF (SHIP-KDFAKSTA-EXP = 2) AND                                     
374300          (SHIP-IDDC-EXP NOT = WC-CDC-SE)                                 
374400*          TILLÄGGSKOSTNADER FÖR VOR DC11->CN/IN/KR                       
374500*          SKALL INTE SKICKAS I ANDRA FAKT. FRÅN DCXX->DEALER             
374600         CONTINUE                                                         
374700       ELSE                                                               
374800         MOVE NEJ                    TO TRACK-SW                          
374900         PERFORM GA-RADINFO                                               
375000         MOVE NOLL-BGMT-PREMBHNT     TO WF-PRARTNTO                       
375100                                        WF-PRARTBTO                       
375200* TO REPLACE WITH THIS 88-LEVEL WHEN ALL THE DC'S ARE                     
375300* IN THE SCOPE.                                                           
375400*          IF (DIST35-NONVCC-NONVCC-REFILL AND                            
375500*                                                                         
375600         IF ((DIST35-NDCCN-NONVCC-REFILL OR                               
375700              DIST35-NDCUS-NONVCC-REFILL OR                               
375800              DIST35-NDCKR-NDC-REFILL    OR                               
375900              DIST35-NDCIN-NDC-REFILL    OR                               
376000              DIST35-NDCMY-NDC-REFILL    OR                               
376100              DIST35-NDCTH-NDC-REFILL    OR                               
376200              DIST35-NDCTW-NDC-REFILL)                                    
376300              AND                                                         
376400              SHIP-KDFAKSTA-EXP = 2)                                      
376500                    OR                                                    
376600             (SHIP-IDDC-EXP = WC-CDC-SE                                   
376700              AND                                                         
376800              SHIP-KDFAKSTA-EXP = 2)                                      
376900                                                                          
377000*                                                                         
377100           COMPUTE WF-PRARTNTO ROUNDED = NOLL-BGMT-PREMBHNT *             
377200                                         CURR-PRKURS-NEW                  
377300           MOVE WF-PRARTNTO          TO WF-PRARTBTO                       
377400**** CHECK IF WE SHOULD ADD FREIGHT ON THE SECOND INVOICE                 
377500*THIS FUNCTION IS VALID FOR CHINA NOW.OTHER FLOWS MAY BE INCLUDED         
377600*IN FUTURE                                                                
377700           IF DIST35-NDCCN-NONVCC-REFILL AND                              
377800             (ADDL-COST1-SW = JA)                                         
377900             PERFORM S09-FREIGHT-ON-SECOND-INVOICE                        
378000           END-IF                                                         
378100         END-IF                                                           
378200*                                                                         
378300**** LYNK EMB SHOULD HAVE SERVICE FEE AS TEXT                             
378400*        IF SHIP-IDSYSTEM = 'LYNK' --> FUNGERAR EJ I MIXADE SKEPP.        
378500*                                  --> MÅSTE TÄNKA UT ANNAT!              
378600*                                  --> MEN DIST79 FUNKAR JUST NU.         
378700*        IF DIST79-NON-SEK                                                
378800*          MOVE 'SERVICE FEE'        TO WF-BEART                          
378900*        ELSE                                                             
379000           MOVE 'PACKING & HANDLING' TO WF-BEART                          
379100*        END-IF                                                           
379200                                                                          
379300         MOVE ZERO                   TO WF-IDSTATNR                       
379400                                        WF-VKARTNTO                       
379500         MOVE +1                     TO WF-KVBEART                        
379600                                          WF-KVLEVART                     
379700         MOVE SPACES                 TO WF-KDARTURS                       
379800                                        WF-IDARTNR-FINANCE                
379900                                        WF-IDEXCUST(2)                    
380000                                        WF-IDOPTION(2)                    
380100                                        WF-IDOPTION(3)                    
380200                                        WF-IDOPTION(4)                    
380300                                        WF-IDOPTION(5)                    
380400                                        WF-IDSEQ(1)                       
380500                                        WF-IDSEQ(2)                       
380600                                        WF-IDLEVNR-ART                    
380700         MOVE NEJ                    TO WF-FLSPECPR                       
380800         MOVE NEJ                    TO WF-FLPCOO                         
380900         MOVE '99999999'             TO WF-IDSEQ(3)                       
381000                                                                          
381100         PERFORM S02-PUT-RAD-WZ01                                         
381200         MOVE JA                     TO TRACK-SW                          
381300**** IF WE SHOULD ADD FREIGHT OR INSURANCE ON THE SECOND INVOICE          
381400         IF ADDL-COST1-SW = JA                                            
381500            IF WS-FREIGHT > ZERO                                          
381600              COMPUTE W-RAD-RAKNARE    = W-RAD-RAKNARE  +  1              
381700              MOVE W-RAD-RAKNARE         TO WF-IDREFRAD                   
381800              MOVE WS-FREIGHT           TO WF-PRARTNTO                    
381900              MOVE WF-PRARTNTO          TO WF-PRARTBTO                    
382000              MOVE 'FREIGHT'            TO WF-BEART                       
382100              PERFORM S02-PUT-RAD-WZ01                                    
382200              MOVE WS-FREIGHT           TO WS-FREIGHT-SAVE                
382300              MOVE ZERO                 TO WS-FREIGHT                     
382400            END-IF                                                        
382500            IF WS-INSURANCE > ZERO                                        
382600              COMPUTE W-RAD-RAKNARE    = W-RAD-RAKNARE  +  1              
382700              MOVE W-RAD-RAKNARE         TO WF-IDREFRAD                   
382800              MOVE WS-INSURANCE         TO WF-PRARTNTO                    
382900              MOVE WF-PRARTNTO          TO WF-PRARTBTO                    
383000              MOVE 'INSURANCE'          TO WF-BEART                       
383100              PERFORM S02-PUT-RAD-WZ01                                    
383200              MOVE WS-INSURANCE         TO WS-INSURANCE-SAVE              
383300              MOVE ZERO                 TO WS-INSURANCE                   
383400            END-IF                                                        
383500         END-IF                                                           
383600*AFTER THE FRIEGHT & INSURANCE ARE SENT TO BILL-IT FOR THE 1ST            
383700*CUSTOMER, WE STOP SENDING FOR OTHER CUSTOMERS                            
383800         IF ADDL-COST1-SW = JA     AND                                    
383900           (WS-FREIGHT-SAVE   > ZERO   OR                                 
384000            WS-INSURANCE-SAVE > ZERO )                                    
384100             MOVE NEJ                TO ADDL-COST1-SW                     
384200         END-IF                                                           
384300       END-IF                                                             
384400     END-IF                                                               
384500*                                                                         
384600     IF NOLL-BGMT-PRAVDRAG NOT = ZERO                                     
384700       IF (SHIP-KDFAKSTA-EXP = 2) AND                                     
384800          (SHIP-IDDC-EXP NOT = WC-CDC-SE)                                 
384900*          TILLÄGGSKOSTNADER FÖR VOR DC11->CN/IN/KR                       
385000*          SKALL INTE SKICKAS I ANDRA FAKT. FRÅN DCXX->DEALER             
385100         CONTINUE                                                         
385200       ELSE                                                               
385300         MOVE NEJ                    TO TRACK-SW                          
385400         PERFORM GA-RADINFO                                               
385500         IF NOLL-BGMT-PRAVDRAG > ZERO                                     
385600            COMPUTE NOLL-BGMT-PRAVDRAG =                                  
385700                    0 - NOLL-BGMT-PRAVDRAG                                
385800         END-IF                                                           
385900         MOVE NOLL-BGMT-PRAVDRAG     TO WF-PRARTNTO                       
386000                                        WF-PRARTBTO                       
386100* TO REPLACE WITH THIS 88-LEVEL WHEN ALL THE DC'S ARE                     
386200* IN THE SCOPE.                                                           
386300*          IF (DIST35-NONVCC-NONVCC-REFILL AND                            
386400*                                                                         
386500         IF ((DIST35-NDCCN-NONVCC-REFILL OR                               
386600              DIST35-NDCUS-NONVCC-REFILL OR                               
386700              DIST35-NDCKR-NDC-REFILL    OR                               
386800              DIST35-NDCIN-NDC-REFILL    OR                               
386900              DIST35-NDCMY-NDC-REFILL    OR                               
387000              DIST35-NDCTH-NDC-REFILL    OR                               
387100              DIST35-NDCTW-NDC-REFILL)                                    
387200              AND                                                         
387300              SHIP-KDFAKSTA-EXP = 2)                                      
387400                                                                          
387500                    OR                                                    
387600            (SHIP-KDFAKSTA-EXP = 2       AND                              
387700             SHIP-IDDC-EXP = WC-CDC-SE)                                   
387800*                                                                         
387900           COMPUTE WF-PRARTNTO ROUNDED = NOLL-BGMT-PRAVDRAG *             
388000                                         CURR-PRKURS-NEW                  
388100           MOVE WF-PRARTNTO          TO WF-PRARTBTO                       
388200         END-IF                                                           
388300         MOVE 'REDUCTION'            TO WF-BEART                          
388400         MOVE ZERO                   TO WF-IDSTATNR                       
388500                                        WF-VKARTNTO                       
388600         MOVE +1                     TO WF-KVBEART                        
388700                                        WF-KVLEVART                       
388800         MOVE SPACES                 TO WF-KDARTURS                       
388900                                        WF-IDARTNR-FINANCE                
389000                                        WF-IDEXCUST(2)                    
389100                                        WF-IDOPTION(2)                    
389200                                        WF-IDOPTION(3)                    
389300                                        WF-IDOPTION(4)                    
389400                                        WF-IDOPTION(5)                    
389500                                        WF-IDSEQ(1)                       
389600                                        WF-IDSEQ(2)                       
389700                                        WF-IDLEVNR-ART                    
389800         MOVE NEJ                    TO WF-FLSPECPR                       
389900         MOVE NEJ                    TO WF-FLPCOO                         
390000         MOVE '99999999'             TO WF-IDSEQ(3)                       
390100                                                                          
390200         PERFORM S02-PUT-RAD-WZ01                                         
390300         MOVE JA                     TO TRACK-SW                          
390400       END-IF                                                             
390500     END-IF                                                               
390600     MOVE ZERO                       TO NOLL-BGMT-IDDISTR                 
390700     .                                                                    
390800     EJECT                                                                
390900 H-UPPDAT-STATUS-WDE221  SECTION.                                         
391000     MOVE 'H-UPPDAT-STATUS-WDE221'   TO WS-SEKTION                        
391100                                                                          
392000     IF WS-IDKOLLI = ZERO                                                 
393000       PERFORM IMS-GHNP-WDE221-KVAL                                       
           ELSE                                                                 
395000       PERFORM IMS-GHU-WDE221-HEL                                         
           END-IF                                                               
     .                                                                          
398000     MOVE 'K'                        TO BKOLLI-KDPRSTA                    
399000     PERFORM IMS-REPL-WDE221                                              
399100     .                                                                    
399200     EJECT                                                                
399300 S01-OPEN-WZ01 SECTION.                                                   
399400     MOVE 'S01-OPEN-WZ01'            TO WS-SEKTION                        
399500                                                                          
399600     MOVE 'OPEN'                     TO SEND-KDFUNC                       
399700     MOVE 'CARPARTS.BILLIT.RECEIVE'  TO SEND-ADDISPABS                    
399800**   MOVE 'CARPARTS.BILLIT.RECEIVE-VIA-VCOM'  TO SEND-ADDISPABS           
399900     MOVE 'CARPARTS.PULS.SAVEIT'     TO SEND-ADDISPABS-RETURN             
400000     IF DIST19-SATS                                                       
400100       MOVE 'CARPARTS.PULS.PASSIT'   TO SEND-ADDISPABS                    
400200       MOVE JA                       TO SATS-SW                           
400300     END-IF                                                               
400400                                                                          
400500     CALL WZ01SEND   USING      SEND-CONTROL-AREA                         
400600                                SEND-OPEN-AREA                            
400700     IF SEND-KDRC > 0                                                     
400800       MOVE SEND-KDRC           TO KDRC-DISP                              
400900       STRING 'WZ01SEND-OPEN RC-ERR = ' KDRC-DISP                         
401000            DELIMITED BY SIZE INTO FELTEXT                                
401100       CALL FELLOG                                                        
401200     ELSE                                                                 
401300       MOVE SEND-IDCOM               TO WS-IDCOM                          
401400     END-IF                                                               
401500     .                                                                    
401600     EJECT                                                                
401700 S02-PUT-RAD-WZ01 SECTION.                                                
401800     MOVE 'S02-PUT-RAD-WZ01'         TO WS-SEKTION                        
401900*--------------                                                           
402000     ADD 1 TO ANTAL-SEND                                                  
402100**   DISPLAY 'W4063700 ' ANTAL-SEND ' ' UT-AREA(1:30)                     
                                                                                
402300     MOVE 'PUT'                 TO SEND-KDFUNC                            
402400     MOVE LENGTH OF UT-AREA     TO SEND-KVDLEN                            
                                                                                
402600     CALL WZ01SEND   USING      SEND-CONTROL-AREA                         
402700                                SEND-KVDLEN                               
402800                                UT-AREA                                   
402900     IF SEND-KDRC > 1                                                     
403000       MOVE SEND-KDRC           TO KDRC-DISP                              
403100       STRING 'WZ01SEND-PUT RC-ERR = ' KDRC-DISP                          
403200            DELIMITED BY SIZE INTO FELTEXT                                
403300       CALL FELLOG                                                        
403400     END-IF                                                               
403500     .                                                                    
403600     EJECT                                                                
403700 S03-CLOSE-WZ01  SECTION.                                                 
403800     MOVE 'S03-CLOSE-WZ01'      TO WS-SEKTION                             
403900                                                                          
404000     MOVE 'CLOSE'               TO SEND-KDFUNC                            
                                                                                
404200     CALL WZ01SEND   USING      SEND-CONTROL-AREA                         
404300     IF SEND-KDRC > 0                                                     
404400       MOVE SEND-KDRC           TO KDRC-DISP                              
404500       STRING 'WZ01SEND-CLOSE RC-ERR = ' KDRC-DISP                        
404600            DELIMITED BY SIZE INTO FELTEXT                                
404700       CALL FELLOG                                                        
404800     END-IF                                                               
404900     .                                                                    
405000     EJECT                                                                
405100 S04-DC-LAND-IDPARTNR  SECTION.                                           
405200     MOVE 'S04-DC-LAND-IDPART'  TO WS-SEKTION                             
     .                                                                          
405400     IF SHIP-IDDC-EXP > SPACE                                             
405500*                                                                         
405600*---   FÖR 'STUDS' DISTR. GÄLLER PARMANR'ET FRÅN                          
405700*      'STUDS'DC'T I FÖRSTA FLÖDET FÖR:                                   
405800*      *GE-REFILLDISTRIKT                                                 
405900*      *VOR FRÅN DC11 TILL CN/IN/KR                                       
406000*      *NDC (EJ FTG=57) TILL IMPORTÖR                                     
406100*                                                                         
406200       IF SHIP-KDFAKSTA-EXP = 1                                           
406300         MOVE SHIP-IDDC-EXP     TO W-IDDC-B6                              
406400         PERFORM IMS-GU-WDB601                                            
406500         MOVE DCS-IDPARTNR      TO WS-IDPARTNR-BOUNCE                     
406600                                   WF-IDPARTNR                            
406700                                   WS-IDPARTNR                            
406800         MOVE SHIP-IDDC         TO W-IDDC-B6                              
406900                                   W-IDDC-WDK711                          
407000       ELSE                                                               
407100         MOVE SHIP-IDDC-EXP     TO W-IDDC-B6                              
407200*                                  W-IDDC-WDK711                          
407300         MOVE BGMT-IDDISTR      TO W-IDDISTR-WDB2                         
407400                                   W-IDDISTR-WDB2-MIN                     
407500                                   W-IDDISTR-WDB2-MAX                     
407600         MOVE BGMT-IDKUNDNR     TO W-IDKUNDNR-WDB2                        
407700         PERFORM IMS-GU-GMTA-WDB201                                       
407800         IF SEGMENT-FINNS                                                 
407900           CONTINUE                                                       
408000         ELSE                                                             
408100           PERFORM IMS-GU-WDB201                                          
408200         END-IF                                                           
408300         MOVE GMT-IDPARTNR      TO WF-IDPARTNR                            
408400                                   WS-IDPARTNR                            
408500       END-IF                                                             
408600     ELSE                                                                 
408700       IF BILL-IDDC NOT = W-IDDC-B6                                       
408800         MOVE BILL-IDDC         TO W-IDDC-B6                              
408900                                   W-IDDC-WDK711                          
409000*                                                                         
409100*        MOVE BGMT-IDPARTNR     TO WF-IDPARTNR                            
409200*                                  WS-IDPARTNR                            
409300       END-IF                                                             
409400     END-IF                                                               
409500     PERFORM IMS-GU-WDB601                                                
409600     MOVE DCS-IDLANDX2          TO WF-IDLANDX3-SEND                       
                                                                                
409800     IF WF-IDLANDX3-SEND = SPACE                                          
409900       STRING 'SÄNDANDE LAND SAKNAR TEXT' BILL-IDDC                       
410000            DELIMITED BY SIZE INTO FELTEXT                                
410100       CALL FELLOG                                                        
410200     END-IF                                                               
410300     .                                                                    
410400     EJECT                                                                
410500 S05-NUM-TEXT  SECTION.                                                   
410600     INSPECT WS-REDUIN REPLACING LEADING ZERO BY SPACE                    
410700     CALL W009REDU USING WS-REDUIN WS-REDUUT                              
410800     .                                                                    
410900     EJECT                                                                
411000 S06-HAEMTA-LEVVIL-TEXT SECTION.                                          
411100     MOVE 'S06-HAEMTA-LEVVIL'    TO WS-SEKTION                            
411200*      LEV.VILLKORSTEXT ÄR 4735  I WDR1                                   
*                                                                               
411400     IF BGMT-KDLEVVIL > ZERO                                              
411500       MOVE '4735'                TO IDHTYP-4735                          
411600       MOVE LOW-VALUE             TO FILLER-4735                          
411700       MOVE BGMT-KDLEVVIL         TO KDLEVVIL-4735                        
411800*                                                                         
411900       PERFORM IMS-4735-GET-ROOT                                          
412000       IF SEGMENT-FINNS                                                   
412100         PERFORM IMS-4735-GET-SEGMENT                                     
412200         IF SEGMENT-FINNS                                                 
412300           IF WS-KDSPRAK = ZERO                                           
412400             PERFORM S07-KDSPRAK                                          
412500           END-IF                                                         
412600           MOVE LEVVIL-BELEVVIL(WS-KDSPRAK) TO WF-BELEVVIL                
412700         ELSE                                                             
412800           MOVE SPACE                TO WF-BELEVVIL                       
412900         END-IF                                                           
             ELSE                                                               
413100         MOVE SPACE                  TO WF-BELEVVIL                       
             END-IF                                                             
           ELSE                                                                 
413400       PERFORM S08-SPEC-LEVVIL                                            
413500     END-IF                                                               
413600     .                                                                    
413700     EJECT                                                                
413800 S07-KDSPRAK  SECTION.                                                    
413900     MOVE 'S07-KDSPRAK'       TO WS-SEKTION                               
414000                                                                          
414100     IF BGMT-IDDISTR = GMT-IDDISTR AND                                    
414200        BGMT-IDKUNDNR = GMT-IDKUNDNR                                      
414300       CONTINUE                                                           
414400     ELSE                                                                 
414500       MOVE BGMT-IDDISTR      TO W-IDDISTR-WDB2                           
414600                                 W-IDDISTR-WDB2-MIN                       
414700                                 W-IDDISTR-WDB2-MAX                       
414800       MOVE BGMT-IDKUNDNR     TO W-IDKUNDNR-WDB2                          
414900       PERFORM IMS-GU-GMTA-WDB201                                         
415000       IF SEGMENT-FINNS                                                   
415100         CONTINUE                                                         
415200       ELSE                                                               
415300         PERFORM IMS-GU-WDB201                                            
             END-IF                                                             
415500     END-IF                                                               
415600     COMPUTE  WS-KDSPRAK  =  GMT-KDSPRAK  +  1                            
415700     .                                                                    
415800     EJECT                                                                
415900 S08-SPEC-LEVVIL  SECTION.                                                
416000     MOVE 'S08-SPEC-LEVVIL'        TO WS-SEKTION                          
416100                                                                          
416200*--- OBS!!! -VIKTIGT- OBS!!!                                              
416300*    VID UPPDATERING AV LEV.VILLKOR, UPPDATERA MED SAMMA KOD ÄVEN:        
416400*    W476SPED                                                             
416500*    W476VERS                                                             
416600                                                                          
416700     MOVE BGMT-IDDISTR TO TEST-IDDISTR                                    
416800     IF DIST41-DDU-FRAKT                                                  
416900       MOVE       'DDU CONSIGNEE (INCOTERMS 2010) '       TO              
417000                 WF-BELEVVIL                                              
417100     ELSE                                                                 
417200       IF DIST41-CIP-FRAKT                                                
417300         MOVE     'CIP                            '       TO              
417400                 WF-BELEVVIL                                              
417410         IF DIST25-ISRAEL-FRAKT AND                                       
417420            KORD-KDORDKL = 1    AND                                       
417430            KORD-KDFRAKT = 17                                             
417440            MOVE  'FCA                            '       TO              
417450                 WF-BELEVVIL                                              
417460         END-IF                                                           
417500       ELSE                                                               
417600         MOVE BGMT-IDDISTR TO TEST-IDDISTR                                
417700         IF BGMT-PRFRAKT = ZERO AND                                       
417800            NOLL-BGMT-PRFRAKT = ZERO                                      
417900           IF BILL-IDDC NOT = W-IDDC-B6                                   
418000              MOVE BILL-IDDC      TO W-IDDC-B6                            
418100                                     W-IDDC-WDK711                        
418200              PERFORM IMS-GU-WDB601                                       
418300           END-IF                                                         
418400           IF DCS-CDC OR (DCS-DDC AND DCS-SWEDEN)                         
418500             EVALUATE TRUE                                                
418600             WHEN DIST76-ROMANIA                                          
418700               MOVE 'CIP CONSIGNEE                  '     TO              
418800                     WF-BELEVVIL                                          
418900             WHEN DIST76-RYSSLAND                                         
419000               MOVE 'CIP MOSCOW  (INCOTERMS 2010)   '     TO              
419100                     WF-BELEVVIL                                          
419200             WHEN DIST76-RYSSLAND-2606                                    
419300               MOVE 'FCA GOTHENBURG,SWE (INCOTERMS 2010)' TO              
419400                     WF-BELEVVIL                                          
419500             WHEN DIST76-VITRYSSLAND                                      
419600               MOVE 'CIP DNEPROPETROVSK             '     TO              
419700                     WF-BELEVVIL                                          
419800             WHEN DIST76-PERU                                             
419900               MOVE 'FCA GOTHENBURG                 '     TO              
420000                     WF-BELEVVIL                                          
420100             WHEN DIST76-BELARUS                                          
420200               MOVE 'CIP                            '     TO              
420300                     WF-BELEVVIL                                          
420400             WHEN DIST76-MOLDAVIA                                         
420500               MOVE 'DAP CHISINAU                   '     TO              
420600                     WF-BELEVVIL                                          
420700             WHEN DIST76-OMAN                                             
420800               MOVE 'DDP                            '     TO              
420900                     WF-BELEVVIL                                          
421000             WHEN DIST76-ARGENTINA AND WF-FLSOFT = JA                     
421100               MOVE 'ECM DOWNLOAD SOFTWARE '              TO              
421200                     WF-BELEVVIL                                          
421300             WHEN DIST76-COLUMBIA                                         
421400               MOVE 'FCA GOTHENBURG                 '     TO              
421500                     WF-BELEVVIL                                          
421600             WHEN DIST76-CANADA-REF                                       
421700               MOVE 'CIP                            '     TO              
421800                     WF-BELEVVIL                                          
421900             WHEN DIST76-SLOVENIA                                         
422000               MOVE 'CIP DEALER                     '     TO              
422100                     WF-BELEVVIL                                          
422200             WHEN DIST76-BOSNIA                                           
422300               MOVE 'CIP LJUBLJANA                  '     TO              
422400                     WF-BELEVVIL                                          
422500             WHEN DIST76-MACEDONIA                                        
422600               MOVE 'CIP LJUBLJANA                  '     TO              
422700                     WF-BELEVVIL                                          
422800             WHEN DIST76-SERBIA                                           
422900               MOVE 'CIP LJUBLJANA                  '     TO              
423000                     WF-BELEVVIL                                          
423100             WHEN DIST76-GEORGIA                                          
423200               MOVE 'CIP TBILISI                    '     TO              
423300                     WF-BELEVVIL                                          
423400             WHEN DIST76-TUNISIA                                          
423500               MOVE 'CPT 2010 TUNIS-CARTHAGE AIRPORT'     TO              
423600                     WF-BELEVVIL                                          
423700             WHEN OTHER                                                   
423800               MOVE 'FCA GÖTEBORG   (INCOTERMS 2010)'     TO              
423900                     WF-BELEVVIL                                          
424000             END-EVALUATE                                                 
424100                                                                          
473100           ELSE                                                           
424300             EVALUATE TRUE                                                
424400             WHEN DIST76-ROMANIA                                          
424500               MOVE 'CIP CONSIGNEE                  '     TO              
424600                     WF-BELEVVIL                                          
424700             WHEN DIST76-RYSSLAND                                         
424800               MOVE 'CIP MOSCOW  (INCOTERMS 2010)   '     TO              
424900                     WF-BELEVVIL                                          
425000             WHEN DIST76-RYSSLAND-2606                                    
425100               MOVE 'FCA GOTHENBURG,SWE (INCOTERMS 2010)' TO              
425200                     WF-BELEVVIL                                          
425300             WHEN DIST76-VITRYSSLAND                                      
425400               MOVE 'CIP DNEPROPETROVSK             '     TO              
425500                     WF-BELEVVIL                                          
425600             WHEN DIST76-BELARUS                                          
425700               MOVE 'CIP                            '     TO              
425800                     WF-BELEVVIL                                          
425900             WHEN DIST76-MOLDAVIA                                         
426000               MOVE 'DAP CHISINAU                   '     TO              
426100                     WF-BELEVVIL                                          
426200             WHEN DIST76-OMAN                                             
426300               MOVE 'DDP                            '     TO              
426400                     WF-BELEVVIL                                          
426500             WHEN DIST76-ARGENTINA AND WF-FLSOFT = JA                     
426600               MOVE 'ECM DOWNLOAD SOFTWARE '              TO              
426700                     WF-BELEVVIL                                          
426800             WHEN DIST76-SLOVENIA                                         
426900               MOVE 'CIP DEALER                     '     TO              
427000                     WF-BELEVVIL                                          
427100             WHEN DIST76-BOSNIA                                           
427200               MOVE 'CIP LJUBLJANA                  '     TO              
427300                     WF-BELEVVIL                                          
427400             WHEN DIST76-MACEDONIA                                        
427500               MOVE 'CIP LJUBLJANA                  '     TO              
427600                     WF-BELEVVIL                                          
427700             WHEN DIST76-SERBIA                                           
427800               MOVE 'CIP LJUBLJANA                  '     TO              
427900                     WF-BELEVVIL                                          
428000             WHEN DIST76-GEORGIA                                          
428100               MOVE 'CIP TBILISI                    '     TO              
428200                     WF-BELEVVIL                                          
428300             WHEN DIST76-TUNISIA                                          
428400               MOVE 'CPT 2010 TUNIS-CARTHAGE AIRPORT'     TO              
428500                     WF-BELEVVIL                                          
428600             WHEN OTHER                                                   
428700               MOVE 'FCA     ' TO WF-BELEVVIL                             
428800             END-EVALUATE                                                 
477200           END-IF                                                         
429000                                                                          
               ELSE                                                             
429200           IF BGMT-PRFOERS = ZERO AND                                     
429300              NOLL-BGMT-PRFOERS = ZERO                                    
429400             EVALUATE TRUE                                                
429500             WHEN DIST76-ROMANIA                                          
429600               MOVE 'CIP CONSIGNEE                  '     TO              
429700                     WF-BELEVVIL                                          
429800             WHEN DIST76-RYSSLAND                                         
429900               MOVE 'CIP MOSCOW  (INCOTERMS 2010)   '     TO              
430000                     WF-BELEVVIL                                          
430100             WHEN DIST76-RYSSLAND-2606                                    
430200               MOVE 'FCA GOTHENBURG,SWE (INCOTERMS 2010)' TO              
430300                     WF-BELEVVIL                                          
430400             WHEN DIST76-VITRYSSLAND                                      
430500               MOVE 'CIP DNEPROPETROVSK             '     TO              
430600                     WF-BELEVVIL                                          
430700             WHEN DIST76-PERU                                             
430800               MOVE 'CPT                            '     TO              
430900                     WF-BELEVVIL                                          
431000             WHEN DIST76-BELARUS                                          
431100               MOVE 'CIP                            '     TO              
431200                     WF-BELEVVIL                                          
431300             WHEN DIST76-MOLDAVIA                                         
431400               MOVE 'DAP CHISINAU                   '     TO              
431500                     WF-BELEVVIL                                          
431600             WHEN DIST76-OMAN                                             
431700               MOVE 'DDP                            '     TO              
431800                     WF-BELEVVIL                                          
431900             WHEN DIST76-ARGENTINA AND WF-FLSOFT = JA                     
432000               MOVE 'ECM DOWNLOAD SOFTWARE '              TO              
432100                     WF-BELEVVIL                                          
432200             WHEN DIST76-COLUMBIA                                         
432300               MOVE 'CPT BOGOTA - COLOMBIA          '     TO              
432400                     WF-BELEVVIL                                          
432500             WHEN DIST76-CANADA-REF                                       
432600               MOVE 'CIP                            '     TO              
432700                     WF-BELEVVIL                                          
432800             WHEN DIST76-SLOVENIA                                         
432900               MOVE 'CIP DEALER                     '     TO              
433000                     WF-BELEVVIL                                          
433100             WHEN DIST76-BOSNIA                                           
433200               MOVE 'CIP LJUBLJANA                  '     TO              
433300                     WF-BELEVVIL                                          
433400             WHEN DIST76-MACEDONIA                                        
433500               MOVE 'CIP LJUBLJANA                  '     TO              
433600                     WF-BELEVVIL                                          
433700             WHEN DIST76-SERBIA                                           
433800               MOVE 'CIP LJUBLJANA                  '     TO              
433900                     WF-BELEVVIL                                          
434000             WHEN DIST76-GEORGIA                                          
434100               MOVE 'CIP TBILISI                    '     TO              
434200                     WF-BELEVVIL                                          
434300             WHEN DIST76-TUNISIA                                          
434400               MOVE 'CPT 2010 TUNIS-CARTHAGE AIRPORT'     TO              
434500                     WF-BELEVVIL                                          
434600             WHEN OTHER                                                   
434700               MOVE 'CPT            (INCOTERMS 2010)'     TO              
434800                     WF-BELEVVIL                                          
434900             END-EVALUATE                                                 
435000           ELSE                                                           
435100             EVALUATE TRUE                                                
435200             WHEN DIST76-ROMANIA                                          
435300               MOVE 'CIP CONSIGNEE                  '     TO              
435400                     WF-BELEVVIL                                          
435500             WHEN DIST76-RYSSLAND                                         
435600               MOVE 'CIP MOSCOW  (INCOTERMS 2010)   '     TO              
435700                     WF-BELEVVIL                                          
435800             WHEN DIST76-RYSSLAND-2606                                    
435900               MOVE 'FCA GOTHENBURG,SWE (INCOTERMS 2010)' TO              
436000                     WF-BELEVVIL                                          
436100             WHEN DIST76-VITRYSSLAND                                      
436200               MOVE 'CIP DNEPROPETROVSK             '     TO              
436300                     WF-BELEVVIL                                          
436400             WHEN DIST76-PERU                                             
436500               MOVE 'CIP                            '     TO              
436600                     WF-BELEVVIL                                          
436700             WHEN DIST76-BELARUS                                          
436800               MOVE 'CIP                            '     TO              
436900                     WF-BELEVVIL                                          
437000             WHEN DIST76-MOLDAVIA                                         
437100               MOVE 'DAP CHISINAU                   '     TO              
437200                     WF-BELEVVIL                                          
437300             WHEN DIST76-OMAN                                             
437400               MOVE 'DDP                            '     TO              
437500                     WF-BELEVVIL                                          
437600             WHEN DIST76-ARGENTINA AND WF-FLSOFT = JA                     
437700               MOVE 'ECM DOWNLOAD SOFTWARE '              TO              
437800                     WF-BELEVVIL                                          
437900             WHEN DIST76-COLUMBIA                                         
438000               MOVE 'CPT BOGOTA - COLOMBIA          '     TO              
438100                     WF-BELEVVIL                                          
438200             WHEN DIST76-SLOVENIA                                         
438300               MOVE 'CIP DEALER                     '     TO              
438400                     WF-BELEVVIL                                          
438500             WHEN DIST76-BOSNIA                                           
438600               MOVE 'CIP LJUBLJANA                  '     TO              
438700                     WF-BELEVVIL                                          
438800             WHEN DIST76-MACEDONIA                                        
438900               MOVE 'CIP LJUBLJANA                  '     TO              
439000                     WF-BELEVVIL                                          
439100             WHEN DIST76-SERBIA                                           
439200               MOVE 'CIP LJUBLJANA                  '     TO              
439300                     WF-BELEVVIL                                          
439400             WHEN DIST76-GEORGIA                                          
439500               MOVE 'CIP TBILISI                    '     TO              
439600                     WF-BELEVVIL                                          
439700             WHEN DIST76-TUNISIA                                          
439800               MOVE 'CPT 2010 TUNIS-CARTHAGE AIRPORT'     TO              
439900                     WF-BELEVVIL                                          
440000             WHEN OTHER                                                   
440100               MOVE 'CIP            (INCOTERMS 2010)'     TO              
440200                     WF-BELEVVIL                                          
440300             END-EVALUATE                                                 
440400                                                                          
440500          END-IF                                                          
477300         END-IF                                                           
477600       END-IF                                                             
477900     END-IF                                                               
478200                                                                          
441000     IF DCS-CDC OR (DCS-DDC AND DCS-SWEDEN)                               
441100                                                                          
441200* OBS! OBS! OBS! OBS!                                                     
441300*      HÄR KOLLAR MAN PÅ TRANSPORT FÖR ATT BESTÄMMA LEV.VILLKOR           
441400* OBS! OBS! OBS! OBS!                                                     
441500*      OM MAN LÄGGER TILL NYA DISTR. HÄR SOM MAN MÅSTE KOLLA PÅ           
441600*      KOLLI-IDTRPTNR MÅSTE MAN LÄGGA TILL DISTR. DÄR MAN LÄSER           
441700*      WDE611 I HUVUDSLINGAN... GÖR 'FIND ALL DIST76-'.                   
441800                                                                          
441900* OBS! OBS! OBS! OBS!                                                     
442000*      HERE WE CHECK ON THE TRANSPORT TO DECIDE DELIVERY TERMS!           
442200* OBS! OBS! OBS! OBS!                                                     
442300*      IF THERE ARE NEW DISTRICTS THAN THOSE BELOW, THAT CHECK            
442400*      KOLLI-IDTRPTNR --> THE NEW DISTR. MUST BE FETCH FROM WDE611        
442500*      IN THE MAIN LOOP --> 'FIND ALL DIST76-' TO FIND WHERE...           
442600                                                                          
442700                                                                          
442800* SPECIELL TEXT PÅ FAKTURN FÖR 7050-BRASILIEN, GÄLLER BARA FÖR BÅT        
442900* TRANSPORTER, 29/5 '12 ENLIGT BJÖRN JENSEN.                              
443000* ANNAN TEXT GÄLLER ÄVEN FLYGTRANSPORTER, 01/11 '25 ENLIGT LINN A.        
443100* OBS!                                                                    
443200* SPECIAL TEXT ON THE INVOICE 7050-BRASIL, APPLIES ONLY FOR BOAT          
443300* TRANSP. 29/5 '12 ACC. TO BJÖRN JENSEN.                                  
443400* DIFF. TEXT IS VALID EVEN FOR THE FLIGHT TRANSPORTS, ACCORDING           
443500* TO LINN A. 01/11 '25.                                                   
443600* OBS!                                                                    
443700       IF DIST76-BRASIL                                                   
443800         IF KOLLI-IDTRPTNR < +100                                         
443900           MOVE 'CIP                           '           TO             
444000                       WF-BELEVVIL                                        
444100         END-IF                                                           
444200                                                                          
444300         IF KOLLI-IDTRPTNR > +99                                          
444400           MOVE 'FOB GÖTEBORG  (INCOTERMS 2010)'           TO             
444500                       WF-BELEVVIL                                        
444600         END-IF                                                           
444700       END-IF                                                             
444800                                                                          
444900* SPECIELL TEXT PÅ FAKTURN FÖR COLOMBIA, DIST 7481 7482,                  
445000* GÄLLER BARA FÖR BÅTTRANSPORTER                                          
445100* 20/8 '15 ENLIGT BJÖRN JENSEN.                                           
445200                                                                          
445300* SPECIAL TEXT ON THE INVOICE FOR COLOMBIA, DIST 7481 7482,               
445400* VALID ONLY FOR BOAT TRANSP.                                             
445500* 20/8 '15 ACC.TO BJÖRN JENSEN.                                           
445600                                                                          
445700       IF DIST76-COLUMBIA                                                 
445800         IF KOLLI-IDTRPTNR > +99                                          
445900           MOVE 'CPT BUENAVENTURA - COLUMBIA '             TO             
446000                       WF-BELEVVIL                                        
446100         END-IF                                                           
446200       END-IF                                                             
446300                                                                          
446400* SPECIELL TEXT PÅ FAKTURN FÖR 4850-SAUDIARABIEN GÄLLER BARA FÖR          
446500* FLYG TRANSPORTER, 26/11'12 ENLIGT BJÖRN JENSEN.                         
446600*                                                                         
446700* SPECIA TEXT ON THE INVOICE 4850-SAUDI ARABIA, VALID ONLY FOR            
446800* FLIGHT TRANSP., 26/11'12 ACC. TO BJÖRN JENSEN.                          
446900       IF DIST76-SAUDI                                                    
447000         IF KOLLI-IDTRPTNR < +100                                         
447100          MOVE 'FCA GLA GOTHENBURG (INCOTERMS 2010)'       TO             
447200                       WF-BELEVVIL                                        
447300         END-IF                                                           
447400       END-IF                                                             
447500                                                                          
447600* SPECIELL TEXT PÅ FAKTURAN FÖR DISTR. 6010, 6124 OCH 6271 MEN            
447700* BARA FRÅN DC 11, DEC.'19 ENLIGT BJÖRN JENSEN.                           
447800*                                                                         
447900* SPECIAL TEXT ON THE INVOICE DISTR. 6010, 6124 & 6271 BUT                
448000* ONLY FROM DC 11, DEC.'19 ACC.TO BJÖRN JENSEN.                           
448100       IF DIST41-CIP-FRAKT-DC                                             
448200         MOVE  'CIP                                '        TO            
448300                       WF-BELEVVIL                                        
448400       END-IF                                                             
448500                                                                          
448600* SPECIELL TEXT PÅ FAKTURN FÖR OMAN, DIST 6233                            
448700* GÄLLER BARA FÖR BÅTTRANSPORTER                                          
448800* 14/1 '20 ENLIGT BJÖRN JENSEN.                                           
448900*                                                                         
449000* SPECIAL TEXT ON THE INVOICE, OMAN, DIST 6233                            
449100* VALID ONLY FOR THE BOAT TRANS.                                          
449200* 14/1 '20 ACC.OT BJÖRN JENSEN.                                           
449300                                                                          
449400       IF DIST76-OMAN                                                     
449500         IF KOLLI-IDTRPTNR > +99                                          
449600           MOVE 'CPT SOHAR               '                 TO             
449700                       WF-BELEVVIL                                        
449800         END-IF                                                           
449900       END-IF                                                             
450000                                                                          
450100* SPECIELL TEXT PÅ FAKTURN FÖR UKRAINE DIST 2615                          
450200* GÄLLER BARA FÖR BIL O BÅTTRANSPORTER                                    
450300* 17/3 '20 ENLIGT BJÖRN JENSEN.                                           
450400                                                                          
450500* SPECIAL TEXT ON THE INVOICE, UKRAINE DIST 2615                          
450600* VALID ONLY FOR TRUCK & BOAT TRANSP.                                     
450700* 17/3 '20 ACC.TO BJÖRN JENSEN.                                           
450800                                                                          
450900       IF DIST76-UKRAINE                                                  
451000         IF KOLLI-IDTRPTNR > +99                                          
451100           MOVE 'CIP COLOGNE             '                 TO             
451200                       WF-BELEVVIL                                        
451300         END-IF                                                           
451400       END-IF                                                             
451500                                                                          
451600* SPECIELL TEXT PÅ FAKTURN FÖR EGYPTEN DIST 3250                          
451700* GÄLLER ALLA FRAKTKODER OCH ORDERKLASS                                   
451800* 22/9 '22 ENLIGT BJÖRN JENSEN.                                           
451900*                                                                         
452000* SPECIAL TEXT ON THE INVOICE, EGYPTEN DIST 3250                          
452100* VALID FOR ATT FREIGHT CODES AND ORDER CLASSES                           
452200* 22/9 '22 ACC.TO BJÖRN JENSEN.                                           
452300                                                                          
452400       IF DIST76-EGYPT                                                    
452500         MOVE 'SEE LASTPAGE OF INVOICE '                   TO             
452600                       WF-BELEVVIL                                        
452700       END-IF                                                             
452800                                                                          
454400     END-IF                                                               
454500     .                                                                    
454600     EJECT                                                                
478500                                                                          
454800 S09-FREIGHT-ON-SECOND-INVOICE SECTION.                                   
454900**** THE DC SHOULD BE THE DC THAT IS SENDING THE GOOD FROM THE            
455000**** FIRST INVOICE                                                        
455100     MOVE 'S09-FREIGHT-ON'            TO WS-SEKTION                       
478900                                                                          
455300     MOVE SHIP-IDDC                  TO IDDC-4591                         
455400     MOVE BGMT-IDDISTR               TO W-IDDISTR-4592                    
480100                                                                          
455600     PERFORM IMS-GU-WDGX4592                                              
480400                                                                          
455800     IF SEGMENT-FINNS                                                     
455900****                                                                      
456000**** AIR FREIGHT                                                          
456100****                                                                      
456200**** THESE HARDCODED FREIGHT NEEDS TO BE RECEIVED FROM ANOTHER            
456300**** SOURCE IN THE FUTURE                                                 
456400****                                                                      
456500       IF KORD-KDFRAKT = 17                                               
456600       OR KORD-KDFRAKT = 18                                               
456700       OR KORD-KDFRAKT = 19                                               
456800**** CALCULATE FREIGHT FOR COST OR WEIGHT                                 
456900         IF 4592-RETRPFAC-AIR > ZERO                                      
457000           COMPUTE WS-FREIGHT = SHIP-SUORDV-FAKT     *                    
457100                                4592-RETRPFAC-AIR                         
457200         ELSE                                                             
457300           IF 4592-PRWEIGHT-AIR > ZERO                                    
457400             COMPUTE WS-FREIGHT = SHIP-VKORDBTO-FAKT *                    
457500                                  4592-PRWEIGHT-AIR                       
457600           ELSE                                                           
457700             MOVE ZERO TO WS-FREIGHT                                      
457800           END-IF                                                         
457900         END-IF                                                           
458000**** WE ADD A FIXED PRICE IF THE INVOICE CONTAINS HAZMAT GOODS            
458100         IF 4592-PRHAZMAT-AIR > ZERO                                      
458200           IF SHIP-FLFARLIG   = JA                                        
458300             COMPUTE WS-FREIGHT = WS-FREIGHT +                            
458400                                  4592-PRHAZMAT-AIR                       
458500           END-IF                                                         
458600         ELSE                                                             
458700           COMPUTE WS-FREIGHT = WS-FREIGHT + 0                            
458800         END-IF                                                           
458900**** CALCULATE INSURANCE FOR COST                                         
459000         IF 4592-REINSFAC-AIR > ZERO                                      
459100           COMPUTE WS-INSURANCE = SHIP-SUORDV-FAKT    *                   
459200                                  4592-REINSFAC-AIR                       
459300         ELSE                                                             
459400           MOVE ZERO TO WS-INSURANCE                                      
459500         END-IF                                                           
459600       ELSE                                                               
459700****                                                                      
459800**** BOAT FREIGHT                                                         
459900****                                                                      
460000         IF KORD-KDFRAKT = 41                                             
460010         OR KORD-KDFRAKT = 43                                             
460100**** CALCULATE FREIGHT FOR COST OR WEIGHT                                 
460200           IF 4592-RETRPFAC-BOAT > ZERO                                   
460300             COMPUTE WS-FREIGHT = SHIP-SUORDV-FAKT    *                   
460400                                  4592-RETRPFAC-BOAT                      
460500           ELSE                                                           
460600             IF 4592-PRWEIGHT-BOAT > ZERO                                 
460700               COMPUTE WS-FREIGHT = SHIP-VKORDBTO-FAKT *                  
460800                                    4592-PRWEIGHT-BOAT                    
460900             ELSE                                                         
461000               MOVE ZERO TO WS-FREIGHT                                    
461100             END-IF                                                       
461200           END-IF                                                         
461300**** WE ADD A FIXED PRICE IF THE INVOICE CONTAINS HAZMAT GOODS            
461400           IF 4592-PRHAZMAT-BOAT > ZERO                                   
461500             IF SHIP-FLFARLIG = JA                                        
461600               COMPUTE WS-FREIGHT = WS-FREIGHT +                          
461700                                    4592-PRHAZMAT-BOAT                    
461800             END-IF                                                       
461900           ELSE                                                           
462000             COMPUTE WS-FREIGHT = WS-FREIGHT + 0                          
462100           END-IF                                                         
462200**** CALCULATE INSURANCE FOR COST                                         
462300           IF 4592-REINSFAC-BOAT > ZERO                                   
462400             COMPUTE WS-INSURANCE = SHIP-SUORDV-FAKT    *                 
462500                                    4592-REINSFAC-BOAT                    
462600           ELSE                                                           
462700             MOVE ZERO TO WS-INSURANCE                                    
462800           END-IF                                                         
462900         ELSE                                                             
463000****                                                                      
463100**** ROAD FREIGHT                                                         
463200****                                                                      
463300**** CALCULATE FREIGHT FOR COST OR WEIGHT                                 
463400           IF 4592-RETRPFAC-ROAD > ZERO                                   
463500             COMPUTE WS-FREIGHT = SHIP-SUORDV-FAKT    *                   
463600                                  4592-RETRPFAC-ROAD                      
463700           ELSE                                                           
463800             IF 4592-PRWEIGHT-ROAD > ZERO                                 
463900               COMPUTE WS-FREIGHT = SHIP-VKORDBTO-FAKT *                  
464000                                    4592-PRWEIGHT-ROAD                    
464100             ELSE                                                         
464200               MOVE ZERO TO WS-FREIGHT                                    
464300             END-IF                                                       
464400           END-IF                                                         
464500**** WE ADD A FIXED PRICE IF THE INVOICE CONTAINS HAZMAT GOODS            
464600           IF 4592-PRHAZMAT-ROAD > ZERO                                   
464700             IF SHIP-FLFARLIG = JA                                        
464800               COMPUTE WS-FREIGHT = WS-FREIGHT +                          
464900                                    4592-PRHAZMAT-ROAD                    
465000             END-IF                                                       
465100           ELSE                                                           
465200             COMPUTE WS-FREIGHT = WS-FREIGHT + 0                          
465300           END-IF                                                         
465400**** CALCULATE INSURANCE FOR COST                                         
465500           IF 4592-REINSFAC-ROAD > ZERO                                   
465600             COMPUTE WS-INSURANCE = SHIP-SUORDV-FAKT    *                 
465700                                    4592-REINSFAC-ROAD                    
465800           ELSE                                                           
465900             MOVE ZERO TO WS-INSURANCE                                    
466000           END-IF                                                         
466100         END-IF                                                           
466200       END-IF                                                             
466300     END-IF                                                               
466400     .                                                                    
466500     EJECT                                                                
480700                                                                          
466700 S11-RECV-OPEN SECTION.                                                   
466800     MOVE 'S11-RECV-OPEN'          TO WS-SEKTION                          
481800                                                                          
467000     MOVE 'OPEN'                   TO RECV-KDFUNC                         
467100     MOVE 'CARPARTS.PULS.BUILDIT'  TO RECV-ADDISPABS                      
482100                                                                          
467300     CALL WZ01RECV USING           RECV-CONTROL-AREA                      
467400                                   RECV-OPEN-AREA                         
482300                                                                          
467600     IF RECV-KDRC > 0                                                     
467700      MOVE RECV-KDRC               TO KDRC-DISP                           
467800      STRING 'WZ01RECV OPEN ERROR RC= ' KDRC-DISP                         
467900        DELIMITED BY SIZE INTO FELTEXT                                    
468000      CALL FELLOG                                                         
468100     END-IF                                                               
468200     .                                                                    
468300     EJECT                                                                
468400 S12-RECV-MESSAGE SECTION.                                                
468500     MOVE 'S12-RECV-MESSAGE'      TO WS-SEKTION                           
468600                                                                          
468700     MOVE 'GET'                    TO RECV-KDFUNC                         
468800     MOVE LENGTH OF MID-W40637I1   TO RECV-KVDLEN                         
468900     CALL WZ01RECV USING RECV-CONTROL-AREA                                
469000                         RECV-KVDLEN                                      
469100                         MID-W40637I1                                     
469200*                                                                         
469300     IF RECV-KDRC > 1                                                     
469400       MOVE RECV-KDRC            TO KDRC-DISP                             
469500       STRING 'WZ01RECV GET  ERROR RC= ' KDRC-DISP                        
469600         DELIMITED BY SIZE INTO FELTEXT                                   
469700       CALL FELLOG                                                        
469800     END-IF                                                               
469900     .                                                                    
470000     EJECT                                                                
470100 S13-RECV-CLOSE SECTION.                                                  
470200     MOVE 'S13-RECV-CLOSE'      TO WS-SEKTION                             
470300                                                                          
470400     MOVE 'CLOSE'                TO RECV-KDFUNC                           
470500     CALL WZ01RECV     USING        RECV-CONTROL-AREA                     
470600*                                                                         
470700     IF RECV-KDRC > 0                                                     
470800       MOVE RECV-KDRC            TO KDRC-DISP                             
470900       STRING 'WZ01RECV CLOSE ERROR RC= ' KDRC-DISP                       
471000         DELIMITED BY SIZE INTO FELTEXT                                   
471100       CALL FELLOG                                                        
471200     END-IF                                                               
471300     .                                                                    
471400     EJECT                                                                
471500 S20-HAMTA-FLPCOO          SECTION.                                       
471600     MOVE BRAD-IDARTNR    TO W-IDARTNR                                    
471700     PERFORM IMS-GU-WDK601                                                
471800     IF SEGMENT-FINNS                                                     
471900       PERFORM IMS-GNP-WDK611                                             
472000       IF SEGMENT-FINNS                                                   
472100         MOVE CLAG-KDARTURS TO TEST-IDLANDX2                              
472200         IF CLAG-KDARTURS = SPACE                                         
472300           MOVE JA  TO WF-FLPCOO                                          
472400         ELSE                                                             
472500           IF CLAG-KDPCOO = 'A'                                           
472600             IF DAGENS-DATUM > CLAG-TIGILTIG-PCOO                         
472700               MOVE JA  TO WF-FLPCOO                                      
472800             ELSE                                                         
472900               MOVE NEJ TO WF-FLPCOO                                      
473000             END-IF                                                       
473100           ELSE                                                           
473200             IF CLAG-KDPCOO = ' '                                         
473300               MOVE JA  TO WF-FLPCOO                                      
473400             END-IF                                                       
473500             IF CLAG-KDPCOO = 'P'                                         
473600               IF LANDX2-EU-IDLANDX2                                      
473700                 IF DAGENS-DATUM > CLAG-TIGILTIG-PCOO                     
473800                   MOVE JA  TO WF-FLPCOO                                  
473900                 ELSE                                                     
474000                   MOVE BGMT-IDDISTR TO W-IDDISTR-WDB2                    
474100                                        W-IDDISTR-WDB2-MIN                
474200                                        W-IDDISTR-WDB2-MAX                
474300                   MOVE BGMT-IDKUNDNR TO W-IDKUNDNR-WDB2                  
474400                   PERFORM IMS-GU-GMTA-WDB201                             
474500                   IF SEGMENT-FINNS                                       
474600                     MOVE GMT-IDPARTNR TO W-WDB1-IDPARTNR                 
474700                     MOVE GMT-IDFTG    TO W-WDB1-IDFTG                    
474800                     PERFORM IMS-GU-WDB1-WDB101                           
474900                     IF SEGMENT-FINNS                                     
475000                       MOVE BET-IDLANDX2 TO PCOO-IDLANDX2                 
475100                       MOVE BRAD-IDARTNR TO PCOO-IDARTNR                  
475200                       MOVE ART-IDLEVNR  TO PCOO-IDLEVNR                  
475300                       MOVE '1'        TO PCOO-KDCALL                     
475400                       CALL W111PCOO USING PCOO-W111PCOO                  
475500                                       PCOO-WDM1-PCB PCOO-WDR2-PCB        
475600                       IF PCOO-KDSVAR = ZERO                              
475700                         MOVE PCOO-FLPCOO TO WF-FLPCOO                    
475800                       ELSE                                               
475900                         MOVE JA  TO WF-FLPCOO                            
476000                       END-IF                                             
476100                     ELSE                                                 
476200                       MOVE JA  TO WF-FLPCOO                              
476300                     END-IF                                               
476400                   ELSE                                                   
476500                     MOVE JA  TO WF-FLPCOO                                
476600                   END-IF                                                 
476700                 END-IF                                                   
476800               ELSE                                                       
476900                 MOVE JA  TO WF-FLPCOO                                    
477000               END-IF                                                     
477100             END-IF                                                       
477200           END-IF                                                         
477300         END-IF                                                           
477400       ELSE                                                               
477500         MOVE JA  TO WF-FLPCOO                                            
477600       END-IF                                                             
477700     ELSE                                                                 
477800       MOVE JA  TO WF-FLPCOO                                              
477900     END-IF                                                               
478000     .                                                                    
478100     EJECT                                                                
483200                                                                          
478300 S31-OPEN-SHIP2TMS SECTION.                                               
478400     MOVE 'S01-OPEN-SHIP2TMS'    TO WS-SEKTION                            
483800                                                                          
478600     MOVE 'OPEN'                 TO SEND-KDFUNC                           
478700     MOVE 'CARPARTS.PULS.SHIP2TMS'                                        
478800                                 TO SEND-ADDISPABS                        
484400                                                                          
479000     CALL WZ01SEND            USING SEND-CONTROL-AREA                     
479100                                    SEND-OPEN-AREA                        
479200     IF SEND-KDRC > 0                                                     
479300       MOVE SEND-KDRC            TO KDRC-DISP                             
479400       STRING 'WZ01SEND-OPEN SHIP2TMS RC-ERR = ' KDRC-DISP                
479500             DELIMITED BY SIZE INTO FELTEXT                               
479600       CALL FELLOG                                                        
479700     ELSE                                                                 
479800       MOVE SEND-IDCOM           TO WS-IDCOM                              
479900     END-IF                                                               
480000     .                                                                    
485000                                                                          
480200 S32-PUT-SHIP2TMS SECTION.                                                
480300     MOVE 'S02-PUT-SHIP2TMS'     TO WS-SEKTION                            
485700                                                                          
480500     MOVE 'PUT'                  TO SEND-KDFUNC                           
480600     MOVE LENGTH OF 4679-AREA    TO SEND-KVDLEN                           
487300                                                                          
480800     CALL WZ01SEND            USING SEND-CONTROL-AREA                     
480900                                    SEND-KVDLEN                           
481000                                    4679-AREA                             
481100     IF SEND-KDRC > 1                                                     
481200       MOVE SEND-KDRC            TO KDRC-DISP                             
481300       STRING 'WZ01SEND-PUT RC-ERR = ' KDRC-DISP                          
481400             DELIMITED BY SIZE INTO FELTEXT                               
481500       CALL FELLOG                                                        
481600     END-IF                                                               
481700     .                                                                    
488100                                                                          
481900 S33-CLOSE-SHIP2TMS SECTION.                                              
482000     MOVE 'S03-CLOSE-SHIP2TMS'   TO WS-SEKTION                            
489800                                                                          
482200     MOVE 'CLOSE'                TO SEND-KDFUNC                           
490700                                                                          
482400     CALL WZ01SEND            USING SEND-CONTROL-AREA                     
482500     IF SEND-KDRC > 0                                                     
482600       MOVE SEND-KDRC            TO KDRC-DISP                             
482700       STRING 'WZ01SEND-CLOSE SHIP2TMS RC-ERR = ' KDRC-DISP               
482800             DELIMITED BY SIZE INTO FELTEXT                               
482900       CALL FELLOG                                                        
483000     END-IF                                                               
483100     .                                                                    
483200                                                                          
483300* --- IMS SEKTIONER ---                                                   
483400     SKIP3                                                                
483500     EJECT                                                                
483600 IMS-GHU-WDE101 SECTION.                                                  
483700     MOVE 'IMS-GHU-WDE101'     TO WS-SEKTION                              
483800                                                                          
483900     STRING 'WDE101  (IDSHIPM  =' W-IDSHIPM-X ')'                         
484000          DELIMITED BY SIZE INTO SSA1                                     
484100     MOVE '  GE' TO GODK-STATUSKODER                                      
484200     CALL CBLTDLI USING GHU WDE1-PCB DLI-IO-WDE101 SSA1                   
484300     MOVE WDE1-STATUS-CODE TO STATUS-WS-E1                                
484400                                                                          
484500*    DENNA KOLLAR INTE PÅ STATUS-WS-E1!                                   
484600     PERFORM IMS-STATUSKONTROLL                                           
484700     .                                                                    
484800 IMS-REPL-WDE101 SECTION.                                                 
484900     MOVE 'IMS-REPL-WDE101'    TO WS-SEKTION                              
485000                                                                          
485100     MOVE '    ' TO GODK-STATUSKODER                                      
485200     CALL CBLTDLI USING REPL WDE1-PCB DLI-IO-WDE101                       
485300     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
485400     PERFORM IMS-STATUSKONTROLL                                           
485500     .                                                                    
485600 IMS-GU-WDE131   SECTION.                                                 
485700                                                                          
485800     STRING 'WDE101  (IDSHIPM  =' W-IDSHIPM-X ')'                         
485900          DELIMITED BY SIZE INTO SSA1                                     
486000     STRING 'WDE111  (WDE111KY =' W-WDE111KY-X ')'                        
486100          DELIMITED BY SIZE INTO SSA2                                     
486200     STRING 'WDE121  (WDE121KY =' W-WDE121KY-X ')'                        
486300          DELIMITED BY SIZE INTO SSA3                                     
486400     STRING 'WDE131  (IDPURAD  =' W-IDPURAD-X ')'                         
486500          DELIMITED BY SIZE INTO SSA4                                     
486600     MOVE '  ' TO GODK-STATUSKODER                                        
486700     CALL CBLTDLI USING GU WDE1-PCB DLI-IO-WDE131 SSA1 SSA2               
486800                                                  SSA3 SSA4               
486900     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
487000     PERFORM IMS-STATUSKONTROLL                                           
487100     .                                                                    
487200 IMS-GNP-WDE141   SECTION.                                                
487300                                                                          
487400     MOVE 'WDE141 ' TO SSA1                                               
487500     MOVE '  GE' TO GODK-STATUSKODER                                      
487600     CALL CBLTDLI USING GNP WDE1-PCB DLI-IO-WDE141 SSA1                   
487700     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
487800     PERFORM IMS-STATUSKONTROLL                                           
487900     .                                                                    
488000 IMS-ISRT-WDE141 SECTION.                                                 
488100                                                                          
488200     STRING 'WDE101  (IDSHIPM  =' W-IDSHIPM-X ')'                         
488300          DELIMITED BY SIZE INTO SSA1                                     
488400     STRING 'WDE111  (WDE111KY =' W-WDE111KY-X ')'                        
488500          DELIMITED BY SIZE INTO SSA2                                     
488600     STRING 'WDE121  (WDE121KY =' W-WDE121KY-X ')'                        
488700          DELIMITED BY SIZE INTO SSA3                                     
488800     STRING 'WDE131  (IDPURAD  =' W-IDPURAD-X ')'                         
488900          DELIMITED BY SIZE INTO SSA4                                     
489000     MOVE   'WDE141 ' TO SSA5                                             
489100     MOVE '  ' TO GODK-STATUSKODER                                        
489200     CALL CBLTDLI USING ISRT WDE1-PCB DLI-IO-WDE141 SSA1 SSA2 SSA3        
489300                                                    SSA4 SSA5             
489400     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
489500     PERFORM IMS-STATUSKONTROLL                                           
489600     .                                                                    
489700 IMS-ISRT-WDL301 SECTION.                                                 
489800                                                                          
489900     MOVE   'WDL301 ' TO SSA1                                             
490000     MOVE '  II' TO GODK-STATUSKODER                                      
490100     CALL CBLTDLI USING ISRT WDL3-PCB DLI-IO-WDL301 SSA1                  
490200     MOVE WDL3-STATUS-CODE TO STATUS-WS                                   
490300     PERFORM IMS-STATUSKONTROLL                                           
490400     .                                                                    
490500 IMS-GU-WDE201 SECTION.                                                   
490600     MOVE 'IMS-GU-WDE201'      TO WS-SEKTION                              
490700                                                                          
490800     STRING 'WDE201  (IDSHIPM  =' W-IDSHIPM-X ')'                         
490900          DELIMITED BY SIZE INTO SSA1                                     
491000     MOVE '    ' TO GODK-STATUSKODER                                      
491100     CALL CBLTDLI USING GU WDE2-PCB DLI-IO-WDE201 SSA1                    
491200     MOVE WDE2-STATUS-CODE TO STATUS-WS                                   
491300     PERFORM IMS-STATUSKONTROLL                                           
491400     .                                                                    
491500     EJECT                                                                
491600 IMS-GU-WDE211 SECTION.                                                   
491700     MOVE 'IMS-GU-WDE211'      TO WS-SEKTION                              
491800                                                                          
491900     STRING 'WDE211  (WDE211KY =' W-WDE211KY-X ')'                        
492000          DELIMITED BY SIZE INTO SSA1                                     
492100     MOVE '  GE' TO GODK-STATUSKODER                                      
492200     CALL CBLTDLI USING GU WDE2-PCB DLI-IO-WDE211 SSA1                    
492300     MOVE WDE2-STATUS-CODE TO STATUS-WS                                   
492400     PERFORM IMS-STATUSKONTROLL                                           
492500     .                                                                    
492600     SKIP2                                                                
492700 IMS-GNP-WDE211 SECTION.                                                  
492800     MOVE 'IMS-GNP-WDE211'      TO WS-SEKTION                             
492900                                                                          
493000     MOVE 'WDE211  '            TO SSA1                                   
493100     MOVE '  GE' TO GODK-STATUSKODER                                      
493200     CALL CBLTDLI USING GNP WDE2-PCB DLI-IO-WDE211 SSA1                   
493300     MOVE WDE2-STATUS-CODE TO STATUS-WS                                   
493400     PERFORM IMS-STATUSKONTROLL                                           
493500     .                                                                    
493600     SKIP2                                                                
493700 IMS-GHNP-WDE221 SECTION.                                                 
493800     MOVE 'IMS-GHNP-WDE221'      TO WS-SEKTION                            
493900                                                                          
494000     STRING 'WDE211  (WDE211KY =' W-WDE211KY-X ')'                        
494100          DELIMITED BY SIZE INTO SSA1                                     
494200     MOVE 'WDE221  '            TO SSA2                                   
494300*    MOVE 'WDE221  '            TO SSA1                                   
494400     MOVE '  GE' TO GODK-STATUSKODER                                      
494500     CALL CBLTDLI USING GHNP WDE2-PCB DLI-IO-WDE221 SSA1 SSA2             
494600     MOVE WDE2-STATUS-CODE TO STATUS-WS                                   
494700     PERFORM IMS-STATUSKONTROLL                                           
494800     .                                                                    
494900     SKIP2                                                                
495000 IMS-GHU-WDE221 SECTION.                                                  
495100     MOVE 'IMS-GHU-WDE221'      TO WS-SEKTION                             
495200                                                                          
495300     STRING 'WDE211  (WDE211KY =' W-WDE211KY-X ')'                        
495400          DELIMITED BY SIZE INTO SSA1                                     
495500     STRING 'WDE221  (WDE221KY =' W-WDE221KY-X ')'                        
495600          DELIMITED BY SIZE INTO SSA2                                     
495700     MOVE '  GE' TO GODK-STATUSKODER                                      
495800     CALL CBLTDLI USING GHU WDE2-PCB DLI-IO-WDE221 SSA1 SSA2              
495900     MOVE WDE2-STATUS-CODE TO STATUS-WS                                   
496000     PERFORM IMS-STATUSKONTROLL                                           
496100     .                                                                    
496200     SKIP2                                                                
496300 IMS-GHU-WDE221-HEL SECTION.                                              
496400     MOVE 'IMS-GHU-WDE221-HEL'      TO WS-SEKTION                         
496500                                                                          
496600     STRING 'WDE201  (IDSHIPM  =' W-IDSHIPM-X ')'                         
496700          DELIMITED BY SIZE INTO SSA1                                     
496800     STRING 'WDE211  (WDE211KY =' W-WDE211KY-X ')'                        
496900          DELIMITED BY SIZE INTO SSA2                                     
497000     STRING 'WDE221  (WDE221KY =' W-WDE221KY-X ')'                        
497100          DELIMITED BY SIZE INTO SSA3                                     
497200     MOVE '  GE' TO GODK-STATUSKODER                                      
497300     CALL CBLTDLI USING GHU WDE2-PCB DLI-IO-WDE221 SSA1 SSA2 SSA3         
497400     MOVE WDE2-STATUS-CODE TO STATUS-WS                                   
497500     PERFORM IMS-STATUSKONTROLL                                           
497600     .                                                                    
497700     SKIP2                                                                
497800 IMS-GHNP-WDE221-KVAL SECTION.                                            
497900     MOVE 'IMS-GHNP-WDE221-KVAL'  TO WS-SEKTION                           
498000                                                                          
498100     STRING 'WDE211  (WDE211KY =' W-WDE211KY-X ')'                        
498200          DELIMITED BY SIZE INTO SSA1                                     
498300     STRING 'WDE221  *F(WDE221KY =' W-WDE221KY-X ')'                      
498400          DELIMITED BY SIZE INTO SSA2                                     
498500     MOVE '  ' TO GODK-STATUSKODER                                        
498600     CALL CBLTDLI USING GHNP WDE2-PCB DLI-IO-WDE221 SSA1 SSA2             
498700     MOVE WDE2-STATUS-CODE TO STATUS-WS                                   
498800     PERFORM IMS-STATUSKONTROLL                                           
498900     .                                                                    
499000     EJECT                                                                
499100 IMS-REPL-WDE221  SECTION.                                                
499200     MOVE 'IMS-REPL-WDE221'     TO WS-SEKTION                             
499300                                                                          
499400     MOVE '    '   TO GODK-STATUSKODER                                    
499500     CALL CBLTDLI USING REPL WDE2-PCB DLI-IO-WDE221                       
499600     MOVE WDE2-STATUS-CODE TO STATUS-WS                                   
499700     PERFORM IMS-STATUSKONTROLL                                           
499800     .                                                                    
499900     SKIP2                                                                
500000 IMS-GNP-WDE231 SECTION.                                                  
500100     MOVE 'IMS-GNP-WDE231'      TO WS-SEKTION                             
500200                                                                          
500300     STRING 'WDE211  (WDE211KY =' W-WDE211KY-X ')'                        
500400          DELIMITED BY SIZE INTO SSA1                                     
500500     STRING 'WDE221  (WDE221KY =' W-WDE221KY-X ')'                        
500600          DELIMITED BY SIZE INTO SSA2                                     
500700     MOVE 'WDE231  '            TO SSA3                                   
500800     MOVE '  GE'   TO GODK-STATUSKODER                                    
500900     CALL CBLTDLI USING GNP WDE2-PCB DLI-IO-WDE231 SSA1 SSA2 SSA3         
501000     MOVE WDE2-STATUS-CODE TO STATUS-WS                                   
501100     PERFORM IMS-STATUSKONTROLL                                           
501200     .                                                                    
501300     EJECT                                                                
501400 IMS-GU-WDE401-ESEQ  SECTION.                                             
501500                                                                          
501600     STRING 'WDE401  (WDE4ESEQ =' W-WDE4ESEQ-X ')'                        
501700          DELIMITED BY SIZE INTO SSA1                                     
501800     MOVE '  GE' TO GODK-STATUSKODER                                      
501900     CALL CBLTDLI USING GU WDE4-PCB DLI-IO-WDE401 SSA1                    
502000     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
502100     PERFORM IMS-STATUSKONTROLL                                           
502200     .                                                                    
502300     SKIP3                                                                
502400 IMS-GNP-WDE411  SECTION.                                                 
502500                                                                          
502600     STRING 'WDE411  *F(IDPURAD  =' W-IDPURAD-X ')'                       
502700          DELIMITED BY SIZE INTO SSA1                                     
502800     MOVE '  GE' TO GODK-STATUSKODER                                      
502900     CALL CBLTDLI USING GNP WDE4-PCB DLI-IO-WDE411 SSA1                   
503000     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
503100     PERFORM IMS-STATUSKONTROLL                                           
503200     .                                                                    
503300     SKIP3                                                                
503400 IMS-GHU-WDE411-BSEQ  SECTION.                                            
503500                                                                          
503600     STRING 'WDE411  (WDE4BSEQ =' W-WDE4BSEQ-X ')'                        
503700          DELIMITED BY SIZE INTO SSA1                                     
503800     MOVE '  GE' TO GODK-STATUSKODER                                      
503900     CALL CBLTDLI USING GHU WDE4B-PCB DLI-IO-WDE411 SSA1                  
504000     MOVE WDE4B-STATUS-CODE TO STATUS-WS                                  
504100     PERFORM IMS-STATUSKONTROLL                                           
504200     .                                                                    
504300     SKIP3                                                                
504400 IMS-REPL-WDE411  SECTION.                                                
504500     MOVE 'IMS-REPL-WDE411'     TO WS-SEKTION                             
504600                                                                          
504700     MOVE '    '   TO GODK-STATUSKODER                                    
504800     CALL CBLTDLI USING REPL WDE4B-PCB DLI-IO-WDE411                      
504900     MOVE WDE4B-STATUS-CODE TO STATUS-WS                                  
505000     PERFORM IMS-STATUSKONTROLL                                           
505100     .                                                                    
505200     SKIP2                                                                
505300 IMS-GU-WDE611 SECTION.                                                   
505400     MOVE 'IMS-GU-WDE611'      TO WS-SEKTION                              
505500                                                                          
505600     STRING 'WDE601  (IDPRODNR =' W-WDE601-X ')'                          
505700          DELIMITED BY SIZE INTO SSA1                                     
505800     STRING 'WDE611  (IDKOLLI  =' W-WDE611-X ')'                          
505900          DELIMITED BY SIZE INTO SSA2                                     
506000     MOVE '  GE'   TO GODK-STATUSKODER                                    
506100     CALL CBLTDLI USING GU WDE6-PCB DLI-IO-WDE611 SSA1 SSA2               
506200     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
506300     PERFORM IMS-STATUSKONTROLL                                           
506400     .                                                                    
506500     EJECT                                                                
506600 IMS-GU-WDD311     SECTION.                                               
506700     MOVE 'IMS-GU-WDD311'        TO WS-SEKTION                            
506800                                                                          
506900     STRING 'WDD301  (WDD3BSEQ =' W-WDD3BSEQ-X ')'                        
507000            DELIMITED BY SIZE INTO SSA1                                   
507100     STRING 'WDD311  (IDSKYLT  =' W-IDSKYLT-X ')'                         
507200            DELIMITED BY SIZE INTO SSA2                                   
507300     MOVE '  GE' TO GODK-STATUSKODER                                      
507400     CALL CBLTDLI USING GU  WDD3-PCB DLI-IO-WDD311 SSA1 SSA2              
507500     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
507600     PERFORM IMS-STATUSKONTROLL                                           
507700     .                                                                    
507800     EJECT                                                                
507900 IMS-GU-GMTA-WDB201     SECTION.                                          
508000     MOVE 'IMS-GU-GMTA-WDB201'   TO WS-SEKTION                            
508100                                                                          
508200     STRING 'WDB201  (IDGMT    =' W-IDGMT-X ')'                           
508300          DELIMITED BY SIZE INTO SSA1                                     
508400     MOVE '    '              TO GODK-STATUSKODER                         
508500                                                                          
508600     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-WDB201 SSA1                    
508700     MOVE WDB2-STATUS-CODE    TO STATUS-WS                                
508800     PERFORM IMS-STATUSKONTROLL                                           
508900     .                                                                    
509000     EJECT                                                                
509100 IMS-GU-WDB201 SECTION.                                                   
509200     MOVE 'IMS-GU-WDB201'       TO WS-SEKTION                             
509300                                                                          
509400     STRING 'WDB201  (IDGMT   >=' W-IDGMT-MIN-X                           
509500                    '&IDGMT   <=' W-IDGMT-MAX-X ')'                       
509600          DELIMITED BY SIZE INTO SSA1                                     
509700     MOVE '    '              TO GODK-STATUSKODER                         
509800                                                                          
509900     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-WDB201 SSA1                    
510000     MOVE WDB2-STATUS-CODE    TO STATUS-WS                                
510100     PERFORM IMS-STATUSKONTROLL                                           
510200     .                                                                    
510300     EJECT                                                                
510400 IMS-GU-WDB1-WDB101    SECTION.                                           
510500     MOVE 'IMS-GU-WDB101'        TO WS-SEKTION                            
510600                                                                          
510700     STRING 'WDB101  (WDB101KY =' W-WDB101KY-X ')'                        
510800          DELIMITED BY SIZE INTO SSA1                                     
510900     MOVE '    '              TO GODK-STATUSKODER                         
511000                                                                          
511100     CALL CBLTDLI USING GU WDB1-PCB DLI-IO-WDB101 SSA1                    
511200     MOVE WDB1-STATUS-CODE    TO STATUS-WS                                
511300     PERFORM IMS-STATUSKONTROLL                                           
511400     .                                                                    
511500     EJECT                                                                
511600 IMS-4735-GET-ROOT SECTION.                                               
511700                                                                          
511800     STRING 'WDR101  (WDGXKEY  =' W-WDGXKEY-4735-X ')'                    
511900            DELIMITED BY SIZE INTO SSA1                                   
512000     MOVE '  GE' TO GODK-STATUSKODER                                      
512100     CALL  CBLTDLI  USING GU   4735-PCB DLI-IO-WDGX4735 SSA1              
512200     MOVE 4735-STATUS-CODE TO STATUS-WS                                   
512300     PERFORM IMS-STATUSKONTROLL                                           
512400     .                                                                    
512500     SKIP2                                                                
512600 IMS-4735-GET-SEGMENT SECTION.                                            
512700                                                                          
512800     STRING 'WDR101  (WDGXKEY  =' W-WDGXKEY-4735-X ')'                    
512900            DELIMITED BY SIZE INTO SSA1                                   
513000     MOVE 'WDGX4735' TO SSA2                                              
513100     MOVE '  GE' TO GODK-STATUSKODER                                      
513200     CALL CBLTDLI  USING GNP  4735-PCB DLI-IO-WDGX4735 SSA1 SSA2          
513300     MOVE 4735-STATUS-CODE TO STATUS-WS                                   
513400     PERFORM IMS-STATUSKONTROLL                                           
513500     .                                                                    
513600     SKIP2                                                                
513700 IMS-GU-WDB601    SECTION.                                                
513800     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
513900          DELIMITED BY SIZE INTO SSA1                                     
514000     MOVE '    ' TO GODK-STATUSKODER                                      
514100     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
514200     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
514300     PERFORM IMS-STATUSKONTROLL                                           
514400     IF SEGMENT-SAKNAS                                                    
514500         MOVE SPACE TO DCS-KDDC                                           
514600     END-IF                                                               
514700     .                                                                    
514800 IMS-GU-WDQ201 SECTION.                                                   
514900                                                                          
515000     STRING 'WDQ201  (IDORDER  =' W-IDORDER-X ')'                         
515100          DELIMITED BY SIZE INTO SSA1                                     
515200     MOVE '  GE'                 TO GODK-STATUSKODER                      
515300     CALL CBLTDLI USING GU WDQ2-PCB DLI-IO-WDQ201 SSA1                    
515400     MOVE WDQ2-STATUS-CODE    TO STATUS-WS                                
515500     PERFORM IMS-STATUSKONTROLL                                           
515600     .                                                                    
515700     SKIP3                                                                
515800 IMS-GU-WDK701 SECTION.                                                   
515900     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-WDK701-X ')'                  
516000     DELIMITED BY SIZE INTO SSA1                                          
516100     MOVE '    ' TO GODK-STATUSKODER                                      
516200     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK701 SSA1                    
516300     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
516400     PERFORM IMS-STATUSKONTROLL                                           
516500     .                                                                    
516600     SKIP3                                                                
516700                                                                          
516800 IMS-GNP-WDK711 SECTION.                                                  
516900     STRING 'WDK711  (IDDC     =' W-IDDC-WDK711-X ')'                     
517000     DELIMITED BY SIZE INTO SSA1                                          
517100     MOVE '  GE' TO GODK-STATUSKODER                                      
517200     CALL CBLTDLI USING GNP WDK7-PCB DLI-IO-WDK711 SSA1                   
517300     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
517400     PERFORM IMS-STATUSKONTROLL                                           
517500     .                                                                    
517600     EJECT                                                                
517700                                                                          
517800 IMS-GHNP-WDK711 SECTION.                                                 
517900     STRING 'WDK711  (IDDC     =' W-IDDC-WDK711-X ')'                     
518000     DELIMITED BY SIZE INTO SSA1                                          
518100     MOVE '  GE' TO GODK-STATUSKODER                                      
518200     CALL CBLTDLI USING GHNP WDK7-PCB DLI-IO-WDK711 SSA1                  
518300     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
518400     PERFORM IMS-STATUSKONTROLL                                           
518500     .                                                                    
518600     EJECT                                                                
518700                                                                          
518800 IMS-REPL-WDK711 SECTION.                                                 
518900     MOVE '  ' TO GODK-STATUSKODER                                        
519000     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-WDK711                       
519100     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
519200     PERFORM IMS-STATUSKONTROLL                                           
519300     .                                                                    
519400     EJECT                                                                
491800                                                                          
519600 IMS-GU-WDK711 SECTION.                                                   
492900                                                                          
519800     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-WDK701-X ')'                  
519900          DELIMITED BY SIZE INTO SSA1                                     
520000     STRING 'WDK711  (IDDC     =' W-IDDC-WDK711-X ')'                     
520100          DELIMITED BY SIZE INTO SSA2                                     
520200     MOVE '  GE' TO GODK-STATUSKODER                                      
520300     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2               
520400     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
520500     PERFORM IMS-STATUSKONTROLL                                           
520600     .                                                                    
520700     EJECT                                                                
520800 IMS-GHNP-WDK728 SECTION.                                                 
520900                                                                          
521000     STRING 'WDK711  (IDDC     =' W-IDDC-WDK711-X ')'                     
521100     DELIMITED BY SIZE INTO SSA1                                          
521200     MOVE 'WDK728 ' TO SSA2                                               
521300     MOVE '  GE' TO GODK-STATUSKODER                                      
521400     CALL CBLTDLI USING GHNP WDK7-PCB DLI-IO-WDK728 SSA1 SSA2             
521500     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
521600     PERFORM IMS-STATUSKONTROLL                                           
521700     .                                                                    
521800     EJECT                                                                
493900                                                                          
522000 IMS-REPL-WDK728 SECTION.                                                 
495200                                                                          
522200     MOVE '  ' TO GODK-STATUSKODER                                        
522300     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-WDK728                       
522400     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
522500     PERFORM IMS-STATUSKONTROLL                                           
522600     .                                                                    
522700     EJECT                                                                
496500                                                                          
522900 IMS-GU-WDK601 SECTION.                                                   
523000     MOVE 'IMS-GU-WDK601'      TO WS-SEKTION                              
523100*                                                                         
523200     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
523300          DELIMITED BY SIZE INTO SSA1                                     
523400     MOVE '  GE'              TO GODK-STATUSKODER                         
523500     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
523600     MOVE WDK6-STATUS-CODE    TO STATUS-WS                                
523700     PERFORM IMS-STATUSKONTROLL                                           
523800     .                                                                    
523900     SKIP3                                                                
524000 IMS-GNP-WDK611 SECTION.                                                  
524100     MOVE 'IMS-GNP-WDK611'    TO WS-SEKTION                               
524200*                                                                         
524300     MOVE 'WDK611 '           TO SSA1                                     
524400     MOVE '  GE'              TO GODK-STATUSKODER                         
524500     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK611 SSA1                   
524600     MOVE WDK6-STATUS-CODE    TO STATUS-WS                                
524700     PERFORM IMS-STATUSKONTROLL                                           
524800     .                                                                    
524900 IMS-GU-WDF502 SECTION.                                                   
525000     STRING 'WDF501  (IDARTNR  =' W-IDARTNR-WDF502-X ')'                  
525100            DELIMITED BY SIZE INTO SSA1                                   
525200     MOVE 'WDF502'         TO SSA2                                        
525300     MOVE '  GE'                TO GODK-STATUSKODER                       
525400     CALL CBLTDLI USING GU WDF5-PCB DLI-IO-WDF502 SSA1 SSA2               
525500     MOVE WDF5-STATUS-CODE      TO STATUS-WS                              
525600     PERFORM IMS-STATUSKONTROLL                                           
525700     .                                                                    
525800 IMS-GU-WDGX4592      SECTION.                                            
525900     MOVE 'IMS-GU-WDGX45 '    TO WS-SEKTION                               
498000                                                                          
526100     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-4591-X ')'                    
526200            DELIMITED BY SIZE INTO SSA1                                   
526300     STRING 'WDGX4592(IDDISTR  =' W-IDDISTR-4592-X ')'                    
526400          DELIMITED BY SIZE INTO SSA2                                     
526500     MOVE '  GE' TO GODK-STATUSKODER                                      
526600     CALL CBLTDLI  USING GU  WDR2-PCB DLI-IO-WDGX4592 SSA1 SSA2           
526700     MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
526800     PERFORM IMS-STATUSKONTROLL                                           
526900     .                                                                    
527000     SKIP2                                                                
527100 IMS-STATUSKONTROLL SECTION.                                              
499300                                                                          
527300     SET STATUS-IX TO 1                                                   
527400     SEARCH GODK-STATUS                                                   
527500       AT END                                                             
527600         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
527700         DELIMITED BY SIZE INTO FELTEXT                                   
527800         CALL FELLOG                                                      
527900       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
528000         CONTINUE                                                         
528100     END-SEARCH                                                           
529000     .                                                                    
