000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4063400.                                                
000300 AUTHOR.         MOGREN STINA.                                            
000400 DATE-WRITTEN.   02/03/01.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000800*    FUNKTION:                                                            
000900*        PROGRAMMET ÄR EN BAKGRUNDS-MPP I MODULEN  SAVE-IT                
001000*        STARTAS AV ÖVERFÖRING FRÅN BILL-IT GENOM WZ01-RUTINEN            
001100*        INFORMATIONEN KOMMER FAKTURAVIS -                                
001200*        FAKTURANR, FAKTURASUMMOR, FAKTURAINFO OCH RADSUMMOR              
001300*        KOMMER RADVIS  (NYCKELINFO I RADEN)                              
001400*        INFORMATIONEN KAN OCKSÅ KOMMA FRÅN W40635 FÖR SATSER             
001500*                                                                         
001600*        WDE1 UPPDATERAS SAMT ANDRA CENTRALA PULSDATABASER                
001700*        TRANS FÖR INFO LÄGGS PÅ WDR4 O WDR7                              
001800*                                                                         
001900*        BILL-IT  DELAR UPP SÄNDNINGEN OM DET BLIR FÖR                    
002000*                 MÅNGA RADER                                             
002100*                                                                         
002200*        PROFORMA-POSTER UPPDATERAR INGA BASER,                           
002300*                                                                         
002400*        PROGRAMMET STARTAR AVSLUTNINGSVIS UPP SOP-RUTIN W476S5           
002500*                                                                         
002600*        PROGRAMMET UPPDATERAR WDE1 (SKEPPN.REG)                          
002700*        PROGRAMMET LÄSER      WDE2 (TRPT.RELEASEREG)                     
002800*        PROGRAMMET UPPDATERAR WDB2 (GODSMOTTAGARREG)                     
002900*        PROGRAMMET UPPDATERAR WDE6 (KOLLIREG)                            
003000*        PROGRAMMET UPPDATERAR WDQ2 (ORDERHUVUD)                          
003100*        PROGRAMMET UPPDATERAR WDR4 (TRANSAR BATCH)                       
003200*        PROGRAMMET UPPDATERAR WDR7 (TRANSAR BATCH)                       
003300*                   LÄSER      WDB1 (KUNDREG)                             
003400*                                                                         
003500*    INDATA.                                                              
003600*        TRANSAKTION: W40634X                                             
003700*                                                                         
003800*                                                                         
003900*    UTDATA.                                                              
004000                                                                          
004100     SKIP3                                                                
004200 ENVIRONMENT DIVISION.                                                    
004300                                                                          
004400 DATA DIVISION.                                                           
004500     EJECT                                                                
004600 WORKING-STORAGE SECTION.                                                 
004700 77  IDPGM                       PIC X(08)   VALUE 'W4063400'.            
004800                                                                          
004900*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
005000 77  FELTEXT                     PIC X(80)   VALUE SPACE.                 
005100                                                                          
005200 77  JA                          PIC X       VALUE 'J'.                   
005300 77  NEJ                         PIC X       VALUE 'N'.                   
005400 77  WS-SOFTWARE                 PIC X       VALUE 'N'.                   
005500                                                                          
005600 77  FL-W476S5                   PIC X(1)    VALUE 'N'.                   
005700                                                                          
005800*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
005900                                                                          
006000 77  INDATA-SW                   PIC X       VALUE 'J'.                   
006100     88  INDATA-OK                           VALUE 'J'.                   
006200     88  INDATA-FEL                          VALUE 'N'.                   
006300                                                                          
006400 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
006500     88  NYCKLAR-OK                          VALUE 'J'.                   
006600     88  NYCKLAR-FEL                         VALUE 'N'.                   
006700                                                                          
006800 77  FIRST-TIME                  PIC X       VALUE 'J'.                   
006900     88  FIRST-JA                            VALUE 'J'.                   
007000     88  FIRST-NEJ                           VALUE 'N'.                   
007100                                                                          
007200 77  TILLAGG-SW                  PIC X       VALUE 'N'.                   
007300     88  TILLAGG-JA                          VALUE 'J'.                   
007400     88  TILLAGG-NEJ                         VALUE 'N'.                   
007500                                                                          
007600 77  SKRIV-POST-SW               PIC X       VALUE 'N'.                   
007700     88  SKRIV-POST                          VALUE 'J'.                   
007800     88  SKRIV-POST-NEJ                      VALUE 'N'.                   
007900                                                                          
008000 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
008100     88  EGEN-MID                            VALUE '4634'.                
008200     88  GODK-MID                            VALUE '4634'.                
008300     88  HELP-MID                            VALUE '0551'.                
008400     EJECT                                                                
008500 77  KDRC-DISP                   PIC 9(4)    VALUE ZERO.                  
008600                                                                          
008700 77  IX                          PIC S9(3)   VALUE ZERO COMP-3.           
008800 77  IX-MAX                      PIC S9(3)   VALUE +7   COMP-3.           
008900 77  IXA                         PIC S9(3)   VALUE ZERO COMP-3.           
009000                                                                          
009100 77  MAX-CHKP                    PIC S9(5)   VALUE +100 COMP-3.           
009200 77  W-CHKP                      PIC S9(5)   VALUE ZERO COMP-3.           
009300 77  CHKP-ID                     PIC X(8)    VALUE 'W40634  '.            
009400 77  DUMMY-AREA                  PIC X(1)    VALUE SPACE.                 
009500                                                                          
009600*    --- ARBETSFÄLT FÖR BERÄKNING AV DAT./TID                             
009700 77  WS-AAAAMMDD                 PIC 9(8)    VALUE ZERO.                  
009800 77  WS-TTMMSSTH                 PIC 9(8)    VALUE ZERO.                  
009900 77  WS-TTMMSS                   PIC 9(6)    VALUE ZERO.                  
010000                                                                          
010100*      --- VALID IDDC CODES                                               
010200*                                                                         
010300*01    -COPY WWDC99                                                       
010400       EJECT                                                              
010500 01  WS-TIKLOCK                  PIC S9(9)   VALUE ZERO COMP-3.           
010600 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
010700                                                                          
010800 01  WS-DCUSER.                                                           
010900     03  FILLER                   PIC X(5)   VALUE 'WIDDC'.               
011000     03  WS-DCUSER-IDDC           PIC X(2)   VALUE SPACE.                 
011100     03  FILLER                   PIC X(1)   VALUE SPACE.                 
011200                                                                          
011300 01  FILLER                      PIC X(16)   VALUE 'WS-SEKTION'.          
011400 01  WS-SEKTION                  PIC X(32)   VALUE SPACE.                 
011500                                                                          
011600*                                                                         
011700 01   WS-BC-PARAMETRAR.                                                   
011800     03  WS-BC.                                                           
011900         05  BC-URV-IDFAKT       PIC S9(7)   VALUE ZERO COMP-3.           
012000                                                                          
012100 01  W-ANT                       PIC S9(3)   VALUE ZERO COMP-3.           
012200 01  W-RAKNARE                   PIC S9(5)   VALUE ZERO COMP-3.           
012300 01  W-IDSEKVNR                  PIC S9(5)   VALUE ZERO COMP-3.           
012400                                                                          
012500 01  FILLER                      PIC X(16)   VALUE 'WS-IDFAKT'.           
012600 01  WS-IDFAKT                   PIC S9(9)   VALUE ZERO COMP-3.           
012700 01  WS-IDPARTNR                 PIC X(9)    VALUE SPACE.                 
012800 01  WS-IDSHIPM                  PIC 9(7)    VALUE ZERO.                  
012900 01  WS-IDSHIPM-FIRST            PIC 9(7)    VALUE ZERO.                  
013000 01  WS-IDDISTR                  PIC 9(5)    VALUE ZERO.                  
013100 01  WS-IDKUNDNR                 PIC 9(7)    VALUE ZERO.                  
013200 01  WS-IDPRODNR                 PIC 9(7)    VALUE ZERO.                  
013300 01  WS-IDKOLLI                  PIC 9(5)    VALUE ZERO.                  
013400 01  WS-IDPURAD                  PIC 9(5)    VALUE ZERO.                  
013500 01  WS-IDARTNR                  PIC 9(9)    VALUE ZERO.                  
013600 01  WS-IDDC-SEND                PIC X(2)    VALUE SPACE.                 
013700 01  WS-IDCOM                    PIC S9(9)   VALUE ZERO COMP-3.           
013800 01  SPAR-DAFAKT                 PIC 9(8)    VALUE ZERO.                  
013900 01  FILLER                      REDEFINES SPAR-DAFAKT.                   
014000   03  SPAR-SEKEL                PIC 9(2).                                
014100   03  SPAR-AAMMDD               PIC 9(6).                                
014200 01  SPAR-TIFAKTID               PIC S9(7)   VALUE ZERO COMP-3.           
014300 01  SPAR-IDKOLLI                PIC S9(5)   VALUE ZERO COMP-3.           
014400 01  SPAR-IDARTNR                PIC S9(9)   VALUE ZERO COMP-3.           
014500 01  SPAR-IDFAKT                 PIC S9(7)   VALUE ZERO COMP-3.           
014600*                                                                         
014700*      CONSTANTER                                                         
014800*01  -COPY WWDCKONS                                                       
014900*                                                                         
015000 01   TEST-IDDISTR                 PIC 9(5)  COMP-3.                      
015100*01  FILLER    -COPY WWDIST10   -RED TEST-IDDISTR.                        
015200     EJECT                                                                
015300*01  FILLER    -COPY WWDIST18   -RED TEST-IDDISTR.                        
015400     EJECT                                                                
015410*01  FILLER    -COPY WWDIST20   -RED TEST-IDDISTR.                        
015420     EJECT                                                                
015500*01  FILLER    -COPY WWDIST35   -RED TEST-IDDISTR.                        
015600     EJECT                                                                
015700*01  FILLER    -COPY WWDIST74   -RED TEST-IDDISTR.                        
015800     EJECT                                                                
015900*01  FILLER    -COPY WWDIS134   -RED TEST-IDDISTR.                        
016000                                                                          
016100 01  TEST-IDARTNR              PIC 9(9)  COMP-3.                          
016200*01  FILLER -COPY WWBYT19    -RED TEST-IDARTNR                            
016300     EJECT                                                                
016400 01  FILLER                      PIC X(16) VALUE 'ISO-KODER'.             
016500*    -COPY  W460LISO                                                      
016600 01  FILLER                      PIC X(20)    VALUE                       
016700                                              'REKOMST-NORGE'.            
016800     SKIP2                                                                
016900                                                                          
017000*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
017100 01  GENERELLA-SUBPROGRAM.                                                
017200     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
017300     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
017400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
017500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
017600     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
017700     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
017800     03  WZ01RECV                PIC X(8)    VALUE 'WZ01RECV'.            
017900     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
018000     EJECT                                                                
018100*                                                                         
018200*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
018300*01 -COPY WMEDAREA                                                        
018400     EJECT                                                                
018500*01  -COPY WDATAREA                                                       
018600     EJECT                                                                
018700*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
018800*                                                                         
018900 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
019000     SKIP3                                                                
019100*01 -COPY WMSGINIT                                                        
019200     EJECT                                                                
019300*    --- PARAMETRAR TILL GEMENSAMMA SUBPROGRAM                            
019400*    --- AREA MED DATA SOM SKA SPARAS MELLAN DIALOGSTEGEN                 
019500*                                                                         
019600 01  SPAR-AREA.                                                           
019700     03  SPAR-IDTRANS            PIC X(4)    VALUE '4634'.                
019800     EJECT                                                                
019900 01  RKOD-ABEND-MED-DUMP         PIC S9(4)   VALUE +33  COMP-3.           
020000                                                                          
020100 01  FILLER                      PIC X(16)   VALUE 'WMSGSOP '.            
020200 01   PROG-TO-PROG-SW.                                                    
020300     03  -COPY WMSGSOP                                                    
020400                                                                          
020500*    --- AREOR FÖR ANROP TILL  WZ01                                       
020600 01  FILLER                      PIC X(16)   VALUE 'WZ01-SEND '.          
020700*01  -COPY WZ01SEND                                                       
020800                                                                          
020900*    --- AREOR FÖR ANROP FRÅN  WZ01                                       
021000 01  FILLER                      PIC X(16)   VALUE 'WZ01-RECV '.          
021100*01  -COPY WZ01RECV                                                       
021200                                                                          
021300 01  FILLER                      PIC X(16)   VALUE 'WF2104I1  '.          
021400*                WF20XXI1  FEEDBACK TO PULS                               
021500 01  INPOST.                                                              
021600*03  -COPY WZ01REQU   -PRE IN-                                            
021700*03  -COPY WF2104I1   -PRE WF-                                            
021800*                                                                         
021900 01  RECV-DATA             REDEFINES INPOST.                              
022000     03  FILLER            PIC X(750).                                    
022100                                                                          
022200 01  FILLER                      PIC X(16)  VALUE 'MID-4631-SEND'.        
022300*01  MOD -COPY W4I63101 -PRE 4631-                                        
022400                                                                          
022500 01  FILLER                      PIC X(16)  VALUE 'MID-4637-SEND'.        
022600*01  MOD -COPY W40637I1 -PRE MOD-                                         
022700*                                                                         
022800 01  FILLER                      PIC X(16)  VALUE 'MID-4679-SEND'.        
022900 01  4679-AREA.                                                           
023000     03  4679-IDSHIPM            PIC X(7).                                
023100     03  4679-KDCALL             PIC X      VALUE '2'.                    
023200*                                                                         
023300*    --- ARBETS-AREOR TILL SEGMENT PÅ  WDR7                               
023400*                                                                         
023500 01  FILLER                      PIC X(16) VALUE 'W4760001'.              
023600*01  -COPY W4760001                                                       
023700                                                                          
023800                                                                          
023900*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
024000*                                                                         
024100 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
024200     SKIP3                                                                
024300 01  NYCKLAR-TILL-DLI.                                                    
024400     03  W-IDSHIPM-X.                                                     
024500         05  W-IDSHIPM           PIC  9(7)   VALUE ZERO.                  
024600     03  W-WDE111KY-X.                                                    
024700         05  W-IDDISTR           PIC S9(5)   VALUE ZERO COMP-3.           
024800         05  W-IDKUNDNR          PIC S9(7)   VALUE ZERO COMP-3.           
024900     03  W-WDE121KY-X.                                                    
025000         05  W-IDPRODNR          PIC S9(7)   VALUE ZERO COMP-3.           
025100         05  W-IDKOLLI           PIC S9(5)   VALUE ZERO COMP-3.           
025200     03  W-IDPURAD-X.                                                     
025300         05  W-IDPURAD           PIC S9(5)   VALUE ZERO COMP-3.           
025400     03  W-WDE4ESEQ-X.                                                    
025500         05  W-IDPRODNR-ESEQ     PIC S9(7)   VALUE ZERO COMP-3.           
025600                                                                          
025700     03  W-IDPRODNR-E601-X.                                               
025800         05  W-IDPRODNR-E6       PIC S9(7)   VALUE ZERO COMP-3.           
025900     03  W-IDKOLLI-E611-X.                                                
026000         05  W-IDKOLLI-E6        PIC S9(5)   VALUE ZERO COMP-3.           
026100     03  W-IDORDER-Q2-X.                                                  
026200         05  W-IDORDER           PIC S9(7)   VALUE ZERO COMP-3.           
026300     03  W-WDQ211KY-X.                                                    
026400         05  W-IDDC-Q211         PIC X(2)    VALUE SPACE.                 
026500         05  W-IDLEVNR-Q211      PIC X(5)    VALUE SPACE.                 
026600     03  W-Q2-IDDC-X.                                                     
026700         05  W-Q2-IDDC           PIC  X(2)   VALUE SPACES.                
026800     03  W-IDPARTNR-X.                                                    
026900         05  W-IDPARTNR          PIC X(9)    VALUE SPACE.                 
027000     03  W-IDGMT-X.                                                       
027100         05  W-IDDISTR-B2        PIC S9(5)   VALUE ZERO COMP-3.           
027200         05  W-IDKUNDNR-B2       PIC S9(7)   VALUE ZERO COMP-3.           
027300     03  W-IDARTNR-X.                                                     
027400         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
027500     03  W-KDSEGKEY-X.                                                    
027600         05  W-KDSEGKEY          PIC X(1)    VALUE SPACE.                 
027700     03  W-IDDC-X.                                                        
027800         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
027900     03  W-WDGXKEY-4507-X.                                                
028000         05  W-IDHTYP-4507       PIC X(4)    VALUE '4507'.                
028100         05  FILLER              PIC X(26)   VALUE LOW-VALUE.             
028200     03  W-IDFAKT-4508-X.                                                 
028300         05  W-IDFAKT-4508       PIC S9(7)   VALUE ZERO COMP-3.           
028400     03  W-KY4510-4510-X.                                                 
028500         05  W-IDPRODNR-4510     PIC S9(7)   VALUE ZERO COMP-3.           
028600         05  W-IDKOLLI-4510      PIC S9(5)   VALUE ZERO COMP-3.           
028700         05  W-IDPURAD-4510      PIC S9(5)   VALUE ZERO COMP-3.           
028800                                                                          
028900     03  W-IDDC-B6-X.                                                     
029000         05 W-IDDC-B6                  PIC X(2).                          
029100     SKIP2                                                                
029200*    --- STATUS-KOD FRÅN IMS                                              
029300 01  STATUS-WS                   PIC XX.                                  
029400     88  SEGMENT-FINNS                       VALUE '  '.                  
029500     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
029600     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
029700     88  SEGMENT-SLUT                        VALUE 'GB'.                  
029800     88  IMS-EJ-OK                           VALUE 'XD'.                  
029900     SKIP2                                                                
030000 01  GODK-STATUSKODER.                                                    
030100     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
030200     SKIP3                                                                
030300 01  SSA1                        PIC X(64).                               
030400 01  SSA2                        PIC X(64).                               
030500     EJECT                                                                
030600*    --- IMS FUNKTIONSKODER                                               
030700*01  -COPY W0003                                                          
030800     EJECT                                                                
030900*    ---  DLI INPUT-OUTPUT AREA                                           
031000                                                                          
031100 01  FILLER             PIC X(16) VALUE 'DLI-IO-WDE101'.                  
031200 01  DLI-IO-WDE101.                                                       
031300*    03  -COPY WDE101                                                     
031400     SKIP3                                                                
031500 01  FILLER             PIC X(16) VALUE 'DLI-IO-WDE111'.                  
031600 01  DLI-IO-WDE111.                                                       
031700*    03  -COPY WDE111                                                     
031800     SKIP3                                                                
031900 01  FILLER             PIC X(16) VALUE 'DLI-IO-WDE121'.                  
032000 01  DLI-IO-WDE121.                                                       
032100*    03  -COPY WDE121                                                     
032200     SKIP3                                                                
032300 01  FILLER             PIC X(16) VALUE 'DLI-IO-WDE201'.                  
032400 01  DLI-IO-WDE201.                                                       
032500*    03  -COPY WDE201 -PRE  WDE2-                                         
032600 01  FILLER             PIC X(16) VALUE 'DLI-IO-WDE211'.                  
032700 01  DLI-IO-WDE211.                                                       
032800*    03  -COPY WDE211                                                     
032900     SKIP3                                                                
033000 01  FILLER             PIC X(16) VALUE 'DLI-IO-WDE221'.                  
033100 01  DLI-IO-WDE221.                                                       
033200*    03  -COPY WDE221                                                     
033300     SKIP3                                                                
033400 01  FILLER             PIC X(16) VALUE 'DLI-IO-WDE231'.                  
033500 01  DLI-IO-WDE231.                                                       
033600*    03  -COPY WDE231                                                     
033700     EJECT                                                                
033800 01  FILLER             PIC X(16) VALUE 'DLI-IO-WDB101'.                  
033900 01  DLI-IO-WDB101.                                                       
034000*    03  -COPY WDB101                                                     
034100 01  FILLER             PIC X(16) VALUE 'DLI-IO-WDB201'.                  
034200 01  DLI-IO-WDB201.                                                       
034300*    03  -COPY WDB201                                                     
034400 01  FILLER             PIC X(16) VALUE 'DLI-IO-WDE401'.                  
034500 01  DLI-IO-WDE401.                                                       
034600*    03  -COPY WDE401                                                     
034700     SKIP3                                                                
034800 01  FILLER             PIC X(16) VALUE 'DLI-IO-WDE601'.                  
034900 01  DLI-IO-WDE601.                                                       
035000*    03  -COPY WDE601                                                     
035100     SKIP3                                                                
035200 01  FILLER             PIC X(16) VALUE 'DLI-IO-WDE611'.                  
035300 01  DLI-IO-WDE611.                                                       
035400*    03  -COPY WDE611                                                     
035500 01  FILLER             PIC X(16) VALUE 'DLI-IO-WDQ201'.                  
035600 01  DLI-IO-WDQ201.                                                       
035700*    03  -COPY WDQ201                                                     
035800 01  FILLER             PIC X(16) VALUE 'DLI-IO-WDQ211'.                  
035900 01  DLI-IO-WDQ211.                                                       
036000*    03  -COPY WDQ211                                                     
036100 01  FILLER             PIC X(16) VALUE 'DLI-IO-WDQ211'.                  
036200 01  DLI-IO-WDQ212.                                                       
036300*    03  -COPY WDQ212                                                     
036400 01  FILLER             PIC X(16) VALUE 'DLI-IO-WDK601'.                  
036500 01  DLI-IO-WDK601.                                                       
036600*    03  -COPY WDK601                                                     
036700     EJECT                                                                
036800 01  FILLER             PIC X(16)  VALUE 'DLI-IO-WDR701'.                 
036900 01  DLI-IO-WDR701.                                                       
037000*    03   -COPY WDR701                                                    
037100 01  FILLER             PIC X(16)  VALUE 'DLI-IO-WDGX4508'.               
037200 01  DLI-IO-WDGX4508.                                                     
037300*    03   -COPY WDGX4508                                                  
037400 01  FILLER             PIC X(16)  VALUE 'DLI-IO-WDGX4510'.               
037500 01  DLI-IO-WDGX4510.                                                     
037600*    03   -COPY WDGX4510                                                  
037700                                                                          
037800 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
037900 01   DLI-IO-AREA-B601.                                                   
038000*     03  -COPY WDB601                                                    
038100     EJECT                                                                
038200 LINKAGE SECTION.                                                         
038300 01  IO-PCB                      PIC X.                                   
038400                                                                          
038500*01  -COPY W0009  -PRE MSG-                                               
038600     EJECT                                                                
038700                                                                          
038800*01  -COPY W0009  -PRE  ALT-                                              
038900                                                                          
039000*01  -COPY W0009  -PRE SH31-                                              
039100                                                                          
039200*01  -COPY W0009  -PRE BU37-                                     0        
039300     EJECT                                                                
039400*01  -COPY W0009  -PRE  SHIP2TMS-                                         
039500     EJECT                                                                
039600*01  -COPY W0008  -PRE WDP7-                                              
039700     05  FILLER                  PIC X.                                   
039800                                                                          
039900*01  -COPY W0008  -PRE WDE1-                                              
040000     05  FILLER                  PIC X.                                   
040100                                                                          
040200*01  -COPY W0008  -PRE WDE2-                                              
040300     05  FILLER                  PIC X.                                   
040400                                                                          
040500*01  -COPY W0008  -PRE WDB1-                                              
040600     05  FILLER                  PIC X.                                   
040700                                                                          
040800*01  -COPY W0008  -PRE WDB2-                                              
040900     05  FILLER                  PIC X.                                   
041000                                                                          
041100*01  -COPY W0008  -PRE WDE4-                                              
041200     05  FILLER                  PIC X.                                   
041300                                                                          
041400*01  -COPY W0008  -PRE WDE6-                                              
041500     05  FILLER                  PIC X.                                   
041600                                                                          
041700*01  -COPY W0008  -PRE WDQ2-                                              
041800     05  FILLER                  PIC X.                                   
041900                                                                          
042000*01  -COPY W0008  -PRE WDK6-                                              
042100     05  FILLER                  PIC X.                                   
042200                                                                          
042300*01  -COPY W0008  -PRE WDR7-                                              
042400     05  FILLER                  PIC X.                                   
042500                                                                          
042600     EJECT                                                                
042700*01  -COPY W0008  -PRE 4507-                                              
042800     05  FILLER                  PIC X.                                   
042900     EJECT                                                                
043000*01  -COPY W0008  -PRE WDB6-                                              
043100     05  FILLER                  PIC X.                                   
043200     EJECT                                                                
043300 PROCEDURE DIVISION  USING  IO-PCB                                        
043400                           ALT-PCB                                        
043500                           SH31-PCB                                       
043600                           BU37-PCB                                       
043700                           SHIP2TMS-PCB                                   
043800                           WDP7-PCB                                       
043900                           WDE1-PCB WDE2-PCB                              
044000                           WDB1-PCB WDB2-PCB                              
044100                           WDE4-PCB                                       
044200                           WDE6-PCB WDQ2-PCB                              
044300                           WDK6-PCB                                       
044400                           WDR7-PCB                                       
044500                           4507-PCB WDB6-PCB.                             
044600 MAIN SECTION.                                                            
044700     ENTRY 'DLITCBL' USING  IO-PCB                                        
044800                           ALT-PCB                                        
044900                           SH31-PCB                                       
045000                           BU37-PCB                                       
045100                           SHIP2TMS-PCB                                   
045200                           WDP7-PCB                                       
045300                           WDE1-PCB WDE2-PCB                              
045400                           WDB1-PCB WDB2-PCB                              
045500                           WDE4-PCB                                       
045600                           WDE6-PCB WDQ2-PCB                              
045700                           WDK6-PCB                                       
045800                           WDR7-PCB                                       
045900                           4507-PCB WDB6-PCB.                             
046000                                                                          
046100     PERFORM A-INIT                                                       
046200     PERFORM S01-RECV-OPEN                                                
046300     PERFORM S02-RECV-MESSAGE                                             
046400     IF WF-IDFINDOC = HIGH-VALUE AND  RECV-KDRC = 0                       
046500       PERFORM S02-RECV-MESSAGE                                           
046600     END-IF                                                               
046700     PERFORM UNTIL RECV-KDRC > 0                                          
046800                                                                          
046900       PERFORM D-NYCKLAR-OSV                                              
047000       PERFORM IMS-GU-WDE201                                              
047100       IF SEGMENT-FINNS                                                   
047200                                                                          
047300         IF WDE2-BILL-IDDC NOT = DCS-IDDC                                 
047400            MOVE WDE2-BILL-IDDC TO W-IDDC-B6                              
047500            PERFORM IMS-GU-WDB601                                         
047600         END-IF                                                           
047700         PERFORM IMS-GNP-WDE211                                           
047800         IF SEGMENT-FINNS                                                 
047900*                                                                         
048000*--        WDE1-LÄSNING FÖR KONTROLL AV FAKT.STATUS...                    
048100*--        READ WDE1 TO CHECK THE INVOICE STATUS...                       
048200           PERFORM IMS-GHU-WDE101                                         
048300*                                                                         
048400           IF SEGMENT-FINNS                                               
048500             IF SHIP-KDFAKSTA-EXP = 0 OR 1 OR SPACE                       
048600*              'VANLIGT' FLÖDE ELLER 1-A FAKT I EXP.'STUDS'               
048700*              'THE ORDINARY' FLOW OR THE 1-ST INV.IN THE BOUNCE          
048800               MOVE WDE2-BILL-IDDC      TO WS-IDDC-SEND                   
048900                                           WS-DCUSER-IDDC                 
049000             ELSE                                                         
049100*              2-A FAKT. I EXPORT 'STUDS'FLÖDET                           
049200*              THE SECOND INV. IN THE BOUNCE FLOW                         
049300               MOVE SHIP-IDDC-EXP       TO WS-IDDC-SEND                   
049400                                           WS-DCUSER-IDDC                 
049500             END-IF                                                       
049600*                                                                         
049700*--          UPPDAT WDE101 MED BILLIT VÄRDEN HÄR                          
049800*--          UPDATE WDE101 WITH SOME BILLIT VALUES HERE                   
049900             IF WS-IDSHIPM-FIRST NOT = WS-IDSHIPM                         
050000                                                                          
050100               IF SHIP-KDFAKSTA-EXP = 0 OR 2 OR SPACE                     
050200                                                                          
050300                 MOVE WF-SUNTO-PART     TO SHIP-SUNTO-TOT                 
050400                 MOVE WF-PRKURS-BET     TO SHIP-PRKURS-BET                
050500                 MOVE WF-KDVALISO-BET   TO SHIP-KDVALISO-BET              
050600                                                                          
050700                 PERFORM IMS-REPL-WDE101                                  
050800               END-IF                                                     
050900                                                                          
051000*IF ITS NOT A BOUNCE SHIPMENT, TRIGGER 4679 WHICH WILL CHECK              
051100*THE SHIPMENT AND SEND TO TMS SYSTEM.                                     
051200               IF SHIP-KDFAKSTA-EXP < 2                                   
051300                 MOVE WS-IDSHIPM        TO 4679-IDSHIPM                   
051400                 PERFORM S27-OPEN-SHIP2TMS                                
051500                 PERFORM S28-PUT-SHIP2TMS                                 
051600                 PERFORM S29-CLOSE-SHIP2TMS                               
051700               END-IF                                                     
051800                                                                          
051900               MOVE WS-IDSHIPM          TO WS-IDSHIPM-FIRST               
052000                                                                          
052100             END-IF                                                       
052200           ELSE                                                           
052300             MOVE WDE2-BILL-IDDC        TO WS-IDDC-SEND                   
052400                                           WS-DCUSER-IDDC                 
052500             MOVE JA                    TO WS-SOFTWARE                    
052600*            WDE1 SAKNAS FÖR SOFTWARE                                     
052700           END-IF                                                         
052800*                                                                         
052900           IF TILLAGG-NEJ                                                 
053000            PERFORM IMS-GNP-WDE231                                        
053100           ELSE                                                           
053200            MOVE 'GE'                 TO STATUS-WS                        
053300           END-IF                                                         
053400           IF SEGMENT-FINNS                                               
053500                                                                          
053600             IF WDE2-BILL-KDFINDOC = 'INV' OR 'INT'                       
053700               PERFORM E-UPPDAT-FAKTURA-NIVA                              
053800               PERFORM G-UPPDAT-RADINFO                                   
053900                                                                          
054000             END-IF                                                       
054100           ELSE                                                           
054200             IF WDE2-BILL-KDFINDOC = 'INV' OR 'INT'                       
054300               IF TILLAGG-JA                                              
054400                 PERFORM H-FRAKTER                                        
054500               ELSE                                                       
054600                 MOVE 'FEL WDE231'         TO FELTEXT                     
054700                 CALL FELLOG                                              
054800               END-IF                                                     
054900             END-IF                                                       
055000           END-IF                                                         
055100         END-IF                                                           
055200       ELSE                                                               
055300         MOVE 'IDSHIP-RENSAT!?'           TO FELTEXT                      
055400*        DISPLAY 'IDSHIP/PROD-SAKNAS PÅ E2 = ' W-IDSHIPM                  
055500* -DETTA DISPLAY BORDE GÖRAS OM TILL D&P   ' ' W-IDPRODNR                 
055600         CALL FELLOG                                                      
055700       END-IF                                                             
055800       PERFORM S02-RECV-MESSAGE                                           
055900       IF WF-IDFINDOC = HIGH-VALUE AND  RECV-KDRC = 0                     
056000         IF WDE2-BILL-KDFINDOC NOT = 'PROF'                               
056100           PERFORM IMS-GHU-WDGX4508                                       
056200           MOVE JA                   TO 4508-FLKLAR                       
056300           PERFORM IMS-REPL-WDGX4508                                      
056400*                                                                         
056500           PERFORM S20-SHIPPING-NA-PROFORMA                               
056600*                                                                         
056700*--SKAPA SKEPPNINGSDOKUMENT OM 'STUDS' FLÖDE (EFTER ANDRA FAKT)           
056800*  * CN->US                                                               
056900*  * US->CN                                                               
057000*  * VOR FRÅN DC11->CN/IN/KR DEALERS                                      
057100*  * NDC (NON FTG 57) -> IMPORTERS                                        
057200*                                                                         
057300           IF WDE2-BILL-IDDC-EXP > SPACE                                  
057400*                                                                         
057500             PERFORM IMS-GHU-WDE101                                       
057600             IF SEGMENT-FINNS                                             
057700               IF SHIP-KDFAKSTA-EXP = 2                                   
057800*                STARTA W4631 -- SHIPDOK                                  
057900*                                                                         
058000                 PERFORM S21-OPEN-WZ01                                    
058100                 PERFORM S22-SEND-WZ01                                    
058200                 PERFORM S23-CLOSE-WZ01                                   
058300               END-IF                                                     
058400             END-IF                                                       
058500           END-IF                                                         
058600*                                                                         
058700*--SKAPA FAKTURA NR.2 OM 'STUDS' FLÖDE (EFTER FÖRSTA FAKT)                
058800*--CREATE INV. NO. 2 IF BOUNCE FLOW (AFTER THE FIRST INV.)                
058900*  * CN->US                                                               
059000*  * US->CN                                                               
059100*  * VOR FRÅN DC11->CN/IN/KR DEALERS                                      
059200*  * NDC (NON FTG 57) -> IMPORTERS                                        
059300*                                                                         
059400           IF WDE2-BILL-IDDC-EXP > SPACE                                  
059500             PERFORM IMS-GHU-WDE101                                       
059600             IF SEGMENT-FINNS                                             
059700               IF SHIP-KDFAKSTA-EXP = 1                                   
059800                 MOVE 2 TO SHIP-KDFAKSTA-EXP                              
059900                 PERFORM IMS-REPL-WDE101                                  
060000*                                                                         
060100*                STARTA W4637 -- BUILDIT                                  
060200                 MOVE ZERO        TO MOD-MID-W40637I1                     
060300                 MOVE SHIP-IDSHIPM TO MOD-MID-IDSHIPM                     
060400                                                                          
060500                 PERFORM S25-OPEN-WZ01                                    
060600                 PERFORM S26-SEND-WZ01                                    
060700                 PERFORM S23-CLOSE-WZ01                                   
060800               END-IF                                                     
060900             END-IF                                                       
061000           END-IF                                                         
061100*                                                                         
061200*--SKAPA EN 'PAPPERS'FAKTURA FOR DISTR. NONVCC TILL VCC EJ DC.11          
061300*  DETTA BEHÖVS FÖR GLOBAL EXP.-FAS 2                                     
061400*--CREATE A 'PAPPER'INVOICE OR DISTR. NONVCC TO VCC NOT DC.11             
061500*  THIS IS FOR GLOBAL EXP.-FASE 2                                         
061600*                                                                         
061700           IF WDE2-BILL-IDDC-EXP = 0 OR SPACE                             
061800             MOVE WS-IDDISTR TO TEST-IDDISTR                              
061900             IF DIST35-NONVCC-VCC-REFILL   OR                             
062000                DIST35-NONVCC-VCC-TRANSFER                                
062100               PERFORM IMS-GHU-WDE101                                     
062200               IF SEGMENT-FINNS                                           
062300                 MOVE 3 TO SHIP-KDFAKSTA-EXP                              
062400                 PERFORM IMS-REPL-WDE101                                  
062500*                                                                         
062600*                STARTA W4637 -- BUILDIT                                  
062700                 MOVE ZERO        TO MOD-MID-W40637I1                     
062800                 MOVE SHIP-IDSHIPM TO MOD-MID-IDSHIPM                     
062900                                                                          
063000                 PERFORM S25-OPEN-WZ01                                    
063100                 PERFORM S26-SEND-WZ01                                    
063200                 PERFORM S23-CLOSE-WZ01                                   
063300               END-IF                                                     
063400             END-IF                                                       
063500           END-IF                                                         
063600*                                                                         
063700*--SKAPA SKEPPNINGSDOKUMENT OM REFILL TILL DUBAI PGA ATT DE               
063800*  BEHÖVER ETT SPECIELT STAT.NR.DOKUMENT EFTER FAKTURERINGEN              
063900*  * REFILL FRÅN DC.11 -> DUBAI                                           
064000*--CREATE A SHIPPING DOC. IF REFILL TO DUBAI DUE TO A SPECIAL             
064100*  STAT.NO.DOCUMENT AFTER INVOICING                                       
064200*  * REFILL FROM DC.11 -> DUBAI                                           
064300*                                                                         
064400           MOVE WS-IDDISTR   TO TEST-IDDISTR                              
064500           PERFORM IMS-GHU-WDE101                                         
064600           IF SEGMENT-FINNS                                               
064700             IF ((SHIP-IDDC = WC-NDC-AE)      AND                         
064710                    (DIST18-SCRAP-NDC-SC       OR                         
064720                     DIST18-SCRAP-NDC-QUAL     OR                         
064730                     DIST18-SCRAP-NDC-SC-LOCAL OR                         
064740                     DIST20-EMBALLAGE-SDC ))                              
064750                                               OR                         
064760                     DIST35-CDC-AE-REFILL      OR                         
064770                     DIST35-AE-CDC-RETURNS                                
064800*              STARTA W4631 -- SHIPDOK                                    
064900*                                                                         
065000               PERFORM S21-OPEN-WZ01                                      
065100               PERFORM S22-SEND-WZ01                                      
065200               PERFORM S23-CLOSE-WZ01                                     
065300             END-IF                                                       
065400           END-IF                                                         
065500*                                                                         
065600         END-IF                                                           
065700         PERFORM S02-RECV-MESSAGE                                         
065800       END-IF                                                             
065900     END-PERFORM                                                          
066000     IF RECV-KDRC > 1                                                     
066100       MOVE 'WZ01-RECV AVSLUTAS FEL' TO FELTEXT                           
066200       CALL FELLOG                                                        
066300     END-IF                                                               
066400     PERFORM S03-RECV-CLOSE                                               
066500                                                                          
066600***                                                                       
066700     PERFORM Z-FINIT                                                      
066800     MOVE ZERO TO RETURN-CODE                                             
066900     GOBACK                                                               
067000     .                                                                    
067100     EJECT                                                                
067200 A-INIT SECTION.                                                          
067300     MOVE 'A-INIT'               TO WS-SEKTION                            
067400                                                                          
067500                                                                          
067600     ACCEPT DAGENS-DATUM      FROM DATE                                   
067700     ACCEPT WS-TIKLOCK        FROM TIME                                   
067800     ACCEPT FIL-TIKLOCK       FROM TIME                                   
067900                                                                          
068000     MOVE ZERO             TO SHIP-IDSHIPM                                
068100                              SKOLLI-IDDISTR                              
068200                              SKOLLI-IDKUNDNR                             
068300                              SKOLLI-IDPRODNR                             
068400                              SKOLLI-IDKOLLI                              
068500                              GMT-IDDISTR                                 
068600                              GMT-IDKUNDNR                                
068700                              VORD-IDPRODNR                               
068800                              KOLLI-IDKOLLI                               
068900                              OHUV-IDORDER                                
069000                              KORD-IDORDER                                
069100                              ART-IDARTNR                                 
069200                              FIL-IDSEKVNR                                
069300     MOVE SPACE            TO STATUS-WS                                   
069400*    PERFORM IMS-GHU-WDR4-4507                                            
069500     .                                                                    
069600     EJECT                                                                
069700 D-NYCKLAR-OSV  SECTION.                                                  
069800     MOVE 'D-NYCKLAR-OSV '      TO WS-SEKTION                             
069900                                                                          
070000     MOVE WF-IDFINDOC              TO WS-IDFAKT                           
070100                                                                          
070200     MOVE WF-IDPARTNR              TO WS-IDPARTNR                         
070300                                                                          
070400     MOVE +0         TO W-ANT                                             
070500     INSPECT WF-IDBUNDLE TALLYING W-ANT FOR CHARACTERS                    
070600             BEFORE INITIAL ' '                                           
070700     IF W-ANT > ZERO                                                      
070800       MOVE WF-IDBUNDLE(1:W-ANT)     TO WS-IDSHIPM                        
070900     END-IF                                                               
071000                                                                          
071100     MOVE +0         TO W-ANT                                             
071200     INSPECT WF-IDEXCUST-1 TALLYING W-ANT FOR CHARACTERS                  
071300             BEFORE INITIAL ' '                                           
071400     IF W-ANT > ZERO                                                      
071500       MOVE WF-IDEXCUST-1(1:W-ANT)   TO WS-IDDISTR                        
071600     END-IF                                                               
071700                                                                          
071800     MOVE +0         TO W-ANT                                             
071900     INSPECT WF-IDEXCUST-2 TALLYING W-ANT FOR CHARACTERS                  
072000             BEFORE INITIAL ' '                                           
072100     IF W-ANT > ZERO                                                      
072200       MOVE WF-IDEXCUST-2(1:W-ANT)   TO WS-IDKUNDNR                       
072300     END-IF                                                               
072400                                                                          
072500     MOVE +0         TO W-ANT                                             
072600     INSPECT WF-IDOPTION-1 TALLYING W-ANT FOR CHARACTERS                  
072700             BEFORE INITIAL ' '                                           
072800     IF W-ANT > ZERO                                                      
072900       MOVE WF-IDOPTION-1(1:W-ANT)   TO WS-IDPRODNR                       
073000     END-IF                                                               
073100                                                                          
073200     MOVE +0         TO W-ANT                                             
073300     INSPECT WF-IDOPTION-2 TALLYING W-ANT FOR CHARACTERS                  
073400             BEFORE INITIAL ' '                                           
073500     IF W-ANT > ZERO                                                      
073600       MOVE WF-IDOPTION-2(1:W-ANT)   TO WS-IDKOLLI                        
073700     END-IF                                                               
073800                                                                          
073900     MOVE +0         TO W-ANT                                             
074000     INSPECT WF-IDOPTION-3 TALLYING W-ANT FOR CHARACTERS                  
074100             BEFORE INITIAL ' '                                           
074200     IF W-ANT > ZERO                                                      
074300       MOVE WF-IDOPTION-3(1:W-ANT)   TO WS-IDPURAD                        
074400     END-IF                                                               
074500                                                                          
074600     MOVE +0         TO W-ANT                                             
074700     INSPECT WF-IDARTNR-FINANCE TALLYING W-ANT FOR CHARACTERS             
074800             BEFORE INITIAL ' '                                           
074900     IF W-ANT > ZERO                                                      
075000       MOVE WF-IDARTNR-FINANCE(1:W-ANT) TO WS-IDARTNR                     
075100     END-IF                                                               
075200                                                                          
075300     MOVE WS-IDSHIPM             TO W-IDSHIPM                             
075400     MOVE WS-IDDISTR             TO W-IDDISTR                             
075500                                    W-IDDISTR-B2                          
075600                                    TEST-IDDISTR                          
075700     MOVE WS-IDKUNDNR            TO W-IDKUNDNR                            
075800                                    W-IDKUNDNR-B2                         
075900     MOVE WS-IDPRODNR            TO W-IDPRODNR                            
076000     MOVE WS-IDKOLLI             TO W-IDKOLLI                             
076100     MOVE WS-IDPURAD             TO W-IDPURAD                             
076200     MOVE WS-IDPARTNR            TO W-IDPARTNR                            
076300     MOVE WS-IDARTNR             TO W-IDARTNR                             
076400                                                                          
076500     MOVE WF-DAFINDOC            TO SPAR-DAFAKT                           
076600                                                                          
076700     MOVE ZERO                   TO KORD-IDPRODNR                         
076800                                    KORD-IDDISTR                          
076900                                    KORD-IDKUNDNR                         
077000     MOVE NEJ                    TO TILLAGG-SW                            
077100                                                                          
077200     IF WF-BEART = 'FREIGHT' OR 'INSURANCE' OR                            
077300                   'LEGAL' OR                                             
077400                   'PACKING & HANDLING' OR                                
077500                   'REDUCTION' OR                                         
077600                   'SERVICE FEE'                                          
077700       MOVE JA                   TO TILLAGG-SW                            
077800     END-IF                                                               
077900                                                                          
078000     .                                                                    
078100     EJECT                                                                
078200 E-UPPDAT-FAKTURA-NIVA SECTION.                                           
078300     MOVE 'E-UPPDAT-FAKTURA-NIVA'    TO WS-SEKTION                        
078400                                                                          
078500     PERFORM EB-UPPDAT-WDB2                                               
078600     PERFORM EA-UPPDAT-WDE6                                               
078700     PERFORM EC-UPPDAT-WDQ2                                               
078800     PERFORM ED-UPPDAT-WDE1                                               
078900     .                                                                    
079000     EJECT                                                                
079100 EA-UPPDAT-WDE6  SECTION.                                                 
079200*       KOLLIREG.                                                         
079300     MOVE 'EA-UPPDAT-WDE6'       TO WS-SEKTION                            
079400                                                                          
079500     IF WS-IDPRODNR = VORD-IDPRODNR  AND                                  
079600        WS-IDKOLLI  = KOLLI-IDKOLLI                                       
079700        CONTINUE                                                          
079800     ELSE                                                                 
079900       IF WDE2-BILL-IDDC NOT = DCS-IDDC                                   
080000         MOVE WDE2-BILL-IDDC TO W-IDDC-B6                                 
080100         PERFORM IMS-GU-WDB601                                            
080200       END-IF                                                             
080300       MOVE WS-IDPRODNR            TO W-IDPRODNR-E6                       
080400       PERFORM IMS-GHU-WDE601                                             
080500       IF SEGMENT-FINNS                                                   
080600                                                                          
080700         MOVE SPAR-AAMMDD          TO VORD-TIFAKT-SK                      
080800         IF DCS-NDC-NA                                                    
080900           MOVE ALL '+'           TO MSGI-WMSGINIT                        
081000           MOVE '011'             TO MSGI-KDCALL                          
081100           MOVE WS-DCUSER         TO MSGI-IDUSER                          
081200           MOVE SPAR-AAMMDD       TO MSGI-TILOKDAT                        
081300           MOVE WF-TIEXTID (1:4)  TO MSGI-TILOKTID                        
081400           MOVE WF-TIEXTID (1:4)  TO MSGI-TILOKTID                        
081500           IF WF-TIEXTID > 240000                                         
081600             MOVE 2356            TO MSGI-TILOKTID                        
081700             IF WF-TIEXTID = 777777                                       
081800               MOVE 2357          TO MSGI-TILOKTID                        
081900             END-IF                                                       
082000             IF WF-TIEXTID = 888888                                       
082100               MOVE 2358          TO MSGI-TILOKTID                        
082200             END-IF                                                       
082300             IF WF-TIEXTID = 999999                                       
082400               MOVE 2359          TO MSGI-TILOKTID                        
082500             END-IF                                                       
082600           END-IF                                                         
082700           CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                     
082800           MOVE MSGI-TILOKDAT     TO VORD-TIFAKT-SK                       
082900         END-IF                                                           
083000         IF WDE2-BILL-IDDC-EXP > SPACE                                    
083100           IF SHIP-KDFAKSTA-EXP = 0 OR 1                                  
083200             ADD +1                TO VORD-KVKOLLI-FAKT                   
083300             ADD +1                TO VORD-KVKOLLI-LAST                   
083400           ELSE                                                           
083500             CONTINUE                                                     
083600           END-IF                                                         
083700         ELSE                                                             
083800           ADD +1                  TO VORD-KVKOLLI-FAKT                   
083900           ADD +1                  TO VORD-KVKOLLI-LAST                   
084000         END-IF                                                           
084100         IF VORD-KDORDSTA > 2                                             
084200           IF VORD-KVORDRAD = VORD-KVORDRAD-PACK                          
084300             IF VORD-KVKOLLI = VORD-KVKOLLI-FAKT AND                      
084400               VORD-KVKOLLI = VORD-KVKOLLI-LAST                           
084500               MOVE 5               TO VORD-KDORDSTA                      
084600             END-IF                                                       
084700           END-IF                                                         
084800         END-IF                                                           
084900         PERFORM IMS-REPL-WDE601                                          
085000                                                                          
085100         MOVE WS-IDKOLLI           TO W-IDKOLLI-E6                        
085200         PERFORM IMS-GHNP-WDE611                                          
085300         IF SEGMENT-FINNS                                                 
085400                                                                          
085500           IF WDE2-BILL-IDDC-EXP > SPACE                                  
085600             IF WDE2-BILL-IDDC-EXP = 11                                   
085700               IF SHIP-KDFAKSTA-EXP = 0 OR 2 OR SPACE                     
085800                 MOVE WS-IDFAKT     TO KOLLI-IDFAKT                       
085900                 MOVE SPAR-AAMMDD   TO KOLLI-TIFAKT                       
086000                 MOVE WF-TIEXTID    TO KOLLI-TIFAKTID                     
086100               ELSE                                                       
086200                 IF SHIP-KDFAKSTA-EXP = 1                                 
086300                   MOVE WS-IDFAKT   TO KOLLI-IDFAKT-EXP                   
086400                   MOVE SPAR-AAMMDD TO KOLLI-TIFAKT-EXP                   
086500                   MOVE WF-TIEXTID TO KOLLI-TIFAKTID-EXP                  
086600                 END-IF                                                   
086700               END-IF                                                     
086800             ELSE                                                         
086900*---   VOR FRÅN DC11--> CN/IN                                             
087000*                                                                         
087100               IF SHIP-KDFAKSTA-EXP = 0 OR 2 OR SPACE                     
087200                 MOVE WS-IDFAKT     TO KOLLI-IDFAKT-EXP                   
087300                 MOVE SPAR-AAMMDD   TO KOLLI-TIFAKT-EXP                   
087400                 MOVE WF-TIEXTID    TO KOLLI-TIFAKTID-EXP                 
087500               ELSE                                                       
087600                 IF SHIP-KDFAKSTA-EXP = 1                                 
087700                   MOVE WS-IDFAKT   TO KOLLI-IDFAKT                       
087800                   MOVE SPAR-AAMMDD TO KOLLI-TIFAKT                       
087900                   MOVE WF-TIEXTID TO KOLLI-TIFAKTID                      
088000                 END-IF                                                   
088100               END-IF                                                     
088200             END-IF                                                       
088300           ELSE                                                           
088400             MOVE WS-IDFAKT        TO KOLLI-IDFAKT                        
088500             MOVE SPAR-AAMMDD      TO KOLLI-TIFAKT                        
088600             MOVE WF-TIEXTID       TO KOLLI-TIFAKTID                      
088700           END-IF                                                         
088800*                                                                         
088900*--        UPPDATERA TIDEN                                                
089000           IF DCS-NDC-NA                                                  
089100*            MOVE 240000             TO KOLLI-TIFAKTID                    
089200             MOVE ALL '+'           TO MSGI-WMSGINIT                      
089300             MOVE '011'             TO MSGI-KDCALL                        
089400             MOVE WS-DCUSER         TO MSGI-IDUSER                        
089500             MOVE SPAR-AAMMDD       TO MSGI-TILOKDAT                      
089600             MOVE WF-TIEXTID (1:4)  TO MSGI-TILOKTID                      
089700             IF WF-TIEXTID > 240000                                       
089800               MOVE 2356            TO MSGI-TILOKTID                      
089900               IF WF-TIEXTID = 777777                                     
090000                 MOVE 2357          TO MSGI-TILOKTID                      
090100               END-IF                                                     
090200               IF WF-TIEXTID = 888888                                     
090300                 MOVE 2358          TO MSGI-TILOKTID                      
090400               END-IF                                                     
090500               IF WF-TIEXTID = 999999                                     
090600                 MOVE 2359          TO MSGI-TILOKTID                      
090700               END-IF                                                     
090800             END-IF                                                       
090900             CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                   
091000             IF WDE2-BILL-IDDC-EXP > SPACE                                
091100               IF SHIP-KDFAKSTA-EXP = 0 OR 2 OR SPACE                     
091200                 MOVE MSGI-TILOKDAT TO KOLLI-TIFAKT                       
091300                 MOVE MSGI-TILOKTID TO WS-TTMMSS (1:4)                    
091400                 MOVE FUNCTION CURRENT-DATE (13:2)                        
091500                                      TO WS-TTMMSS(5:2)                   
091600                 MOVE WS-TTMMSS     TO KOLLI-TIFAKTID                     
091700               ELSE                                                       
091800                 IF SHIP-KDFAKSTA-EXP = 1                                 
091900                   MOVE MSGI-TILOKDAT TO KOLLI-TIFAKT-EXP                 
092000                   MOVE MSGI-TILOKTID TO WS-TTMMSS (1:4)                  
092100                   MOVE FUNCTION CURRENT-DATE (13:2)                      
092200                                        TO WS-TTMMSS(5:2)                 
092300                   MOVE WS-TTMMSS   TO KOLLI-TIFAKTID-EXP                 
092400                 END-IF                                                   
092500               END-IF                                                     
092600             ELSE                                                         
092700               MOVE MSGI-TILOKDAT TO KOLLI-TIFAKT                         
092800               MOVE MSGI-TILOKTID TO WS-TTMMSS (1:4)                      
092900               MOVE FUNCTION CURRENT-DATE (13:2)                          
093000                                    TO WS-TTMMSS(5:2)                     
093100               MOVE WS-TTMMSS       TO KOLLI-TIFAKTID                     
093200             END-IF                                                       
093300           ELSE                                                           
093400             IF WDE2-BILL-IDDC-EXP > SPACE                                
093500*---           'STUDS' REFILL-EXPORT                                      
093600               IF WDE2-BILL-IDDC-EXP = WC-CDC-SE                          
093700                 IF SHIP-KDFAKSTA-EXP = 0 OR 2 OR SPACE                   
093800                   IF WF-TIEXTID > 240000                                 
093900                     MOVE 235656    TO KOLLI-TIFAKTID                     
094000                     IF WF-TIEXTID = 777777                               
094100                       MOVE 235757  TO KOLLI-TIFAKTID                     
094200                     END-IF                                               
094300                     IF WF-TIEXTID = 888888                               
094400                       MOVE 235858  TO KOLLI-TIFAKTID                     
094500                     END-IF                                               
094600                     IF WF-TIEXTID = 999999                               
094700                       MOVE 235959  TO KOLLI-TIFAKTID                     
094800                     END-IF                                               
094900                   END-IF                                                 
095000                 ELSE                                                     
095100                   IF SHIP-KDFAKSTA-EXP = 1                               
095200                     IF WF-TIEXTID > 240000                               
095300                       MOVE 235656  TO KOLLI-TIFAKTID-EXP                 
095400                       IF WF-TIEXTID = 777777                             
095500                         MOVE 235757 TO KOLLI-TIFAKTID-EXP                
095600                       END-IF                                             
095700                       IF WF-TIEXTID = 888888                             
095800                         MOVE 235858 TO KOLLI-TIFAKTID-EXP                
095900                       END-IF                                             
096000                       IF WF-TIEXTID = 999999                             
096100                         MOVE 235959 TO KOLLI-TIFAKTID-EXP                
096200                       END-IF                                             
096300                     END-IF                                               
096400                   END-IF                                                 
096500                 END-IF                                                   
096600               ELSE                                                       
096700*                                                                         
096800*---           VOR DC11 --> CN/IN                                         
096900                 IF SHIP-KDFAKSTA-EXP = 0 OR 2 OR SPACE                   
097000                   IF WF-TIEXTID > 240000                                 
097100                     MOVE 235656    TO KOLLI-TIFAKTID-EXP                 
097200                     IF WF-TIEXTID = 777777                               
097300                       MOVE 235757  TO KOLLI-TIFAKTID-EXP                 
097400                     END-IF                                               
097500                     IF WF-TIEXTID = 888888                               
097600                       MOVE 235858  TO KOLLI-TIFAKTID-EXP                 
097700                     END-IF                                               
097800                     IF WF-TIEXTID = 999999                               
097900                       MOVE 235959  TO KOLLI-TIFAKTID-EXP                 
098000                     END-IF                                               
098100                   END-IF                                                 
098200                 ELSE                                                     
098300                   IF SHIP-KDFAKSTA-EXP = 1                               
098400                     IF WF-TIEXTID > 240000                               
098500                       MOVE 235656  TO KOLLI-TIFAKTID                     
098600                       IF WF-TIEXTID = 777777                             
098700                         MOVE 235757 TO KOLLI-TIFAKTID                    
098800                       END-IF                                             
098900                       IF WF-TIEXTID = 888888                             
099000                         MOVE 235858 TO KOLLI-TIFAKTID                    
099100                       END-IF                                             
099200                       IF WF-TIEXTID = 999999                             
099300                         MOVE 235959 TO KOLLI-TIFAKTID                    
099400                       END-IF                                             
099500                     END-IF                                               
099600                   END-IF                                                 
099700                 END-IF                                                   
099800               END-IF                                                     
099900             ELSE                                                         
100000               IF WF-TIEXTID > 240000                                     
100100                 MOVE 235656        TO KOLLI-TIFAKTID                     
100200                 IF WF-TIEXTID = 777777                                   
100300                   MOVE 235757      TO KOLLI-TIFAKTID                     
100400                 END-IF                                                   
100500                 IF WF-TIEXTID = 888888                                   
100600                   MOVE 235858      TO KOLLI-TIFAKTID                     
100700                 END-IF                                                   
100800                 IF WF-TIEXTID = 999999                                   
100900                   MOVE 235959      TO KOLLI-TIFAKTID                     
101000                 END-IF                                                   
101100               END-IF                                                     
101200             END-IF                                                       
101300           END-IF                                                         
101400                                                                          
101500* UPPD. STATUS 9 BARA OM: 'VANLIGT' ELLER 1:A FAKT I 'STUDS' FLÖDE        
101600           IF WDE2-BILL-IDDC-EXP = SPACE                                  
101700             MOVE +9               TO KOLLI-KDKOLSTA                      
101800                                                                          
101900             PERFORM IMS-REPL-WDE611                                      
102000           ELSE                                                           
102100             IF WDE2-BILL-IDDC-EXP > SPACE                                
102200               IF SHIP-KDFAKSTA-EXP = 0 OR 1 OR SPACE                     
102300                 MOVE +9           TO KOLLI-KDKOLSTA                      
102400               END-IF                                                     
102500                                                                          
102600               PERFORM IMS-REPL-WDE611                                    
102700             END-IF                                                       
102800           END-IF                                                         
102900         END-IF                                                           
103000       ELSE                                                               
103100         MOVE 'FEL WDE601'         TO FELTEXT                             
103200         CALL FELLOG                                                      
103300                                                                          
103400       END-IF                                                             
103500     END-IF                                                               
103600     .                                                                    
103700     EJECT                                                                
103800 EB-UPPDAT-WDB2 SECTION.                                                  
103900*       GODSMOTTAGARREG.                                                  
104000     MOVE 'EB-UPPDAT-WDB2'       TO WS-SEKTION                            
104100                                                                          
104200     IF WS-IDDISTR  = GMT-IDDISTR  AND                                    
104300        WS-IDKUNDNR = GMT-IDKUNDNR                                        
104400        CONTINUE                                                          
104500     ELSE                                                                 
104600       MOVE WS-IDDISTR             TO W-IDDISTR-B2                        
104700       MOVE WS-IDKUNDNR            TO W-IDKUNDNR-B2                       
104800                                                                          
104900       PERFORM IMS-GHU-WDB201                                             
105000       IF SEGMENT-FINNS                                                   
105100          MOVE SPAR-AAMMDD         TO GMT-TIFAKT                          
105200          PERFORM IMS-REPL-WDB201                                         
105300          MOVE GMT-IDPARTNR        TO W-IDPARTNR                          
105400       END-IF                                                             
105500     END-IF                                                               
105600     .                                                                    
105700     EJECT                                                                
105800 EC-UPPDAT-WDQ2 SECTION.                                                  
105900*       ORDERHUVUD                                                        
106000     MOVE 'EC-UPPDAT-WDQ2'       TO WS-SEKTION                            
106100                                                                          
106200     MOVE WS-IDPRODNR            TO W-IDPRODNR-ESEQ                       
106300     IF WDE2-BILL-IDDC NOT = DCS-IDDC                                     
106400        MOVE WDE2-BILL-IDDC TO W-IDDC-B6                                  
106500        PERFORM IMS-GU-WDB601                                             
106600     END-IF                                                               
106700                                                                          
106800     PERFORM IMS-GU-WDE401-ESEQ                                           
106900     IF SEGMENT-FINNS                                                     
107000       IF KORD-IDORDER NOT =  OHUV-IDORDER                                
107100         MOVE KORD-IDORDER         TO W-IDORDER                           
107200         PERFORM IMS-GU-WDQ201                                            
107300         IF DCS-DDC                                                       
107400                                                                          
107500           MOVE WDE2-BILL-IDDC       TO W-IDDC-Q211                       
107600           MOVE WDE2-BILL-IDLEVNR    TO W-IDLEVNR-Q211                    
107700           PERFORM IMS-GHNP-WDQ211                                        
107800           IF SEGMENT-FINNS                                               
107900             IF VORD-KVKOLLI-FL =  VORD-KVKOLLI                           
108000               AND VORD-KDORDSTA > 2                                      
108100               IF VORD-KVKOLLI-FAKT   NOT = VORD-KVKOLLI  AND             
108200                 VORD-KVKOLLI-LAST   NOT = VORD-KVKOLLI                   
108300                 MOVE 'S*'           TO DIRL-KDORDSTA                     
108400               ELSE                                                       
108500                 MOVE 'SF'           TO DIRL-KDORDSTA                     
108600               END-IF                                                     
108700               PERFORM IMS-REPL-WDQ211                                    
108800             END-IF                                                       
108900           END-IF                                                         
109000         ELSE                                                             
109100           IF VORD-KVKOLLI-FL = VORD-KVKOLLI                              
109200              AND VORD-KDORDSTA > 2                                       
109300             MOVE VORD-IDDC          TO W-Q2-IDDC                         
109400             PERFORM IMS-GHNP-WDQ212                                      
109500             IF VORD-KVKOLLI-FAKT  NOT = VORD-KVKOLLI  AND                
109600               VORD-KVKOLLI-LAST   NOT = VORD-KVKOLLI                     
109700               MOVE 'S*'           TO ARB-KDORDSTA                        
109800             ELSE                                                         
109900               MOVE 'SF'           TO ARB-KDORDSTA                        
110000             END-IF                                                       
110100             PERFORM IMS-REPL-WDQ212                                      
110200           END-IF                                                         
110300         END-IF                                                           
110400       END-IF                                                             
110500     END-IF                                                               
110600     .                                                                    
110700     EJECT                                                                
110800 ED-UPPDAT-WDE1  SECTION.                                                 
110900     IF WS-IDSHIPM  = SHIP-IDSHIPM    AND                                 
111000        WS-IDDISTR  = SKOLLI-IDDISTR  AND                                 
111100        WS-IDKUNDNR = SKOLLI-IDKUNDNR AND                                 
111200        WS-IDPRODNR = SKOLLI-IDPRODNR AND                                 
111300        WS-IDKOLLI  = SKOLLI-IDKOLLI                                      
111400       CONTINUE                                                           
111500     ELSE                                                                 
111600       PERFORM IMS-GHU-WDE101                                             
111700       IF SEGMENT-FINNS                                                   
111800         PERFORM IMS-GHNP-WDE121                                          
111900         IF SEGMENT-FINNS                                                 
112000           IF SHIP-KDFAKSTA-EXP = 0 OR SPACE                              
112100             IF SKOLLI-IDFAKT = ZERO                                      
112200                MOVE WS-IDFAKT        TO SKOLLI-IDFAKT                    
112300                PERFORM IMS-REPL-WDE121                                   
112400             ELSE                                                         
112500*              IF WS-IDSHIPM NOT = 4633231                                
112600                 DISPLAY ' SHPM + FAKT = ' WS-IDSHIPM ' / '               
112700                                       SKOLLI-IDFAKT                      
112800                 MOVE 'FAKTURANUMMER FINNS, REDAN FAKTURERKT ?'           
112900                                      TO FELTEXT                          
113000*                CALL FELLOG                                              
113100*              END-IF                                                     
113200             END-IF                                                       
113300           ELSE                                                           
113400             IF SHIP-KDFAKSTA-EXP = 2                                     
113500*                 DIST35-NONVCC-NONVCC-REFILL                             
113600*                                                                         
113700               IF SHIP-IDDC-EXP = WC-CDC-SE                               
113800                 IF SKOLLI-IDFAKT = ZERO                                  
113900                    MOVE WS-IDFAKT    TO SKOLLI-IDFAKT                    
114000                    PERFORM IMS-REPL-WDE121                               
114100                 ELSE                                                     
114200*                  IF WS-IDSHIPM NOT = 4633231                            
114300                     DISPLAY ' SHPM + FAKT = ' WS-IDSHIPM ' / '           
114400                                           SKOLLI-IDFAKT                  
114500                     MOVE                                                 
114600                     'FAKTURANUMMER FINNS, REDAN FAKTURERKT ?'            
114700                                      TO FELTEXT                          
114800*                    CALL FELLOG                                          
114900*                  END-IF                                                 
115000                 END-IF                                                   
115100               ELSE                                                       
115200*---             VOR CDC -> CN/IN/KR                                      
115300                 IF SKOLLI-IDFAKT-EXP = ZERO                              
115400                    MOVE WS-IDFAKT    TO SKOLLI-IDFAKT-EXP                
115500                    PERFORM IMS-REPL-WDE121                               
115600                 ELSE                                                     
115700*                  IF WS-IDSHIPM NOT = 4633231                            
115800                     DISPLAY ' SHPM + FAKT = ' WS-IDSHIPM ' / '           
115900                                           SKOLLI-IDFAKT-EXP              
116000                     MOVE                                                 
116100                     'FAKTURANUMMER FINNS, REDAN FAKTURERKT ?'            
116200                                      TO FELTEXT                          
116300*                    CALL FELLOG                                          
116400*                  END-IF                                                 
116500                 END-IF                                                   
116600               END-IF                                                     
116700             ELSE                                                         
116800               IF SHIP-KDFAKSTA-EXP = 1                                   
116900*                   DIST35-NONVCC-NONVCC-REFILL                           
117000*                                                                         
117100                 IF SHIP-IDDC-EXP = WC-CDC-SE                             
117200                   IF SKOLLI-IDFAKT-EXP = ZERO                            
117300                      MOVE WS-IDFAKT  TO SKOLLI-IDFAKT-EXP                
117400                      PERFORM IMS-REPL-WDE121                             
117500                   ELSE                                                   
117600*                    IF WS-IDSHIPM NOT = 4633231                          
117700                     DISPLAY ' SHPM + FAKT = ' WS-IDSHIPM ' / '           
117800                                             SKOLLI-IDFAKT-EXP            
117900                     MOVE                                                 
118000                     'FAKTURANUMMER FINNS, REDAN FAKTURERKT ?'            
118100                                      TO FELTEXT                          
118200*                     CALL FELLOG                                         
118300*                    END-IF                                               
118400                   END-IF                                                 
118500                 ELSE                                                     
118600*---               VOR CDC -> CN/IN/KR                                    
118700                   IF SKOLLI-IDFAKT = ZERO                                
118800                      MOVE WS-IDFAKT  TO SKOLLI-IDFAKT                    
118900                      PERFORM IMS-REPL-WDE121                             
119000                   ELSE                                                   
119100*                    IF WS-IDSHIPM NOT = 4633231                          
119200                     DISPLAY ' SHPM + FAKT = ' WS-IDSHIPM ' / '           
119300                                             SKOLLI-IDFAKT                
119400                     MOVE                                                 
119500                     'FAKTURANUMMER FINNS, REDAN FAKTURERKT ?'            
119600                                      TO FELTEXT                          
119700*                     CALL FELLOG                                         
119800*                    END-IF                                               
119900                   END-IF                                                 
120000                 END-IF                                                   
120100               END-IF                                                     
120200             END-IF                                                       
120300           END-IF                                                         
120400         END-IF                                                           
120500       END-IF                                                             
120600     END-IF                                                               
120700     .                                                                    
120800     EJECT                                                                
120900 G-UPPDAT-RADINFO SECTION.                                                
121000     MOVE 'G-UPPDAT-RADINFO'    TO WS-SEKTION                             
121100                                                                          
121200     MOVE BGMT-FLCOD            TO BILL-FLCOD                             
121300                                                                          
121400     PERFORM GA-HUVUD-R4                                                  
121500     PERFORM GF-FAKTURARAD-R7                                             
121600     PERFORM GG-FAKTURARAD-R4                                             
121700     .                                                                    
121800     SKIP3                                                                
121900 GA-HUVUD-R4  SECTION.                                                    
122000     MOVE 'GA-HUVUD-SECTION'    TO WS-SEKTION                             
122100                                                                          
122200     IF WS-IDFAKT NOT = SPAR-IDFAKT                                       
122300        MOVE WS-IDFAKT             TO W-IDFAKT-4508                       
122400                                      SPAR-IDFAKT                         
122500                                      4508-IDFAKT                         
122600        MOVE NEJ                   TO 4508-FLKLAR                         
122700        PERFORM IMS-ISRT-WDGX4508                                         
122800     END-IF                                                               
122900     .                                                                    
123000     EJECT                                                                
123100 GF-FAKTURARAD-R7  SECTION.                                               
123200     MOVE 'GF-FAKTURARAD-R7'         TO WS-SEKTION                        
123300                                                                          
123400     MOVE WF-IDFINDOC            TO BILL-IDFAKT                           
123500     MOVE WF-DAFINDOC            TO BILL-DAFINDOC                         
123600     MOVE WF-TIEXTID             TO BILL-TIFINDOC                         
123700     MOVE WF-IDPARTNR            TO BILL-IDPARTNR                         
123800     MOVE WS-IDSHIPM             TO BILL-IDSHIPM                          
123900     MOVE WS-IDDISTR             TO BILL-IDDISTR                          
124000     MOVE WS-IDKUNDNR            TO BILL-IDKUNDNR                         
124100     MOVE WS-IDPRODNR            TO BILL-IDPRODNR                         
124200     MOVE WS-IDKOLLI             TO BILL-IDKOLLI                          
124300     MOVE WS-IDPURAD             TO BILL-IDPURAD                          
124400     MOVE KORD-IDORDNR5          TO BILL-IDORDNR7                         
124500     MOVE WS-IDDC-SEND           TO BILL-IDDC                             
124600     MOVE WDE2-BILL-TISKEPPN     TO BILL-TISKEPPN                         
124700     MOVE WF-KDVALISO            TO BILL-KDVALISO-FAKT                    
124800     MOVE WF-SUBTO-TOT           TO BILL-SUBTO-TOT                        
124900     MOVE WF-SUNTO-PART          TO BILL-SUNTO-TOT                        
125000     MOVE WF-SUVAT-BILLIT-TOT    TO BILL-SUVAT-FAKT                       
125100     MOVE WF-BEART               TO BILL-BEART                            
125200     MOVE WF-VKARTNTO            TO BILL-VKARTNTO                         
125300     MOVE WF-SUBTO               TO BILL-SUBTO-LINE                       
125400     MOVE WF-SUNTO               TO BILL-SUNTO-LINE                       
125500     MOVE WF-SUVAT-BILLIT        TO BILL-SUVAT-LINE                       
125600     MOVE WF-KDVALISO-BET        TO BILL-KDVALISO-BET                     
125700     MOVE WF-PRKURS-BET          TO BILL-PRKURS-BET                       
125800     MOVE WF-PRKURS              TO BILL-PRKURS-FAKT                      
125900     MOVE WF-PRKURS-FAKBET       TO BILL-PRKURS-FIKTIV                    
126000     MOVE WF-IDLEVNR-ART         TO BILL-IDLEVNR-ART                      
126100     IF TILLAGG-JA                                                        
126200         MOVE ZERO               TO ART-KDPRODSL                          
126300                                    ART-IDFKNGRP                          
126400     ELSE                                                                 
126500       IF ART-IDARTNR NOT = BRAD-IDARTNR                                  
126600         PERFORM IMS-GU-WDK601                                            
126700         IF SEGMENT-SAKNAS                                                
126800           MOVE ZERO             TO ART-KDPRODSL                          
126900                                    ART-IDFKNGRP                          
127000         END-IF                                                           
127100       END-IF                                                             
127200     END-IF                                                               
127300     MOVE ART-KDPRODSL           TO BILL-KDPRODSL                         
127400     MOVE ART-IDFKNGRP           TO BILL-IDFKNGRP                         
127500     MOVE ZERO                   TO BILL-PRAVCOST                         
127600                                    BILL-PRAVCOST-CORE                    
127700     IF TILLAGG-NEJ                                                       
127800       PERFORM GFA-HAMTA-KDRABATT                                         
127900     ELSE                                                                 
128000       MOVE ZERO                 TO BILL-KDARTRAB                         
128100     END-IF                                                               
128200     MOVE BGMT-KDLEVVIL          TO BILL-KDLEVVIL                         
128300                                                                          
128400     IF WS-SOFTWARE = NEJ                                                 
128500       MOVE SHIP-KDFAKSTA-EXP    TO BILL-KDFAKSTA-EXP                     
128600     ELSE                                                                 
128700       MOVE ZERO                 TO BILL-KDFAKSTA-EXP                     
128800     END-IF                                                               
128900                                                                          
129000     MOVE WF-IDVAT-LEG           TO BILL-IDVAT-LEG                        
129100     MOVE WF-IDVAT-RESP          TO BILL-IDVAT-RESP                       
129200     MOVE WF-IDVAT-BET           TO BILL-IDVAT-BET                        
129300     MOVE WF-IDVAT-AGENT         TO BILL-IDVAT-AGENT                      
129400     MOVE WF-IDVAT-DDGS-RESP     TO BILL-IDVAT-DDGS-RESP                  
129500                                                                          
129600     MOVE WF-SUNTO-PART-LOC      TO BILL-SUNTO-PART-LOC                   
129700     MOVE WF-SUVAT-BILLIT-TOT-PART-L                                      
129800                                 TO BILL-SUVAT-BILLIT-TOT-PART-L          
129900     MOVE WF-SUBTO-TOT-PART-LOC  TO BILL-SUBTO-TOT-PART-LOC               
130000     MOVE WF-SUNTO-TOT-LOC       TO BILL-SUNTO-TOT-LOC                    
130100     MOVE WF-SUVAT-BILLIT-TOT-LOC                                         
130200                                 TO BILL-SUVAT-BILLIT-TOT-LOC             
130300     MOVE WF-SUBTO-TOT-LOC       TO BILL-SUBTO-TOT-LOC                    
130400     MOVE WF-KDVALISO-LOC        TO BILL-KDVALISO-LOC                     
130500     MOVE WF-PRKURS-LOC          TO BILL-PRKURS-LOC                       
130600     MOVE WF-KDSIGN-LOCC         TO BILL-KDTECKEN-LOC                     
130700     MOVE WF-SUNTO-LOCC          TO BILL-SUNTO-LOCC                       
130800     MOVE WF-SUNTO-PART-RECALC   TO BILL-SUNTO-PART-RECALC                
130900     MOVE WF-SUVAT-BILLIT-TOT-PART-R                                      
131000                                 TO BILL-SUVAT-BILLIT-TOT-PART-R          
131100     MOVE WF-SUBTO-TOT-PART-RECALC                                        
131200                                 TO BILL-SUBTO-TOT-PART-RECALC            
131300     MOVE WF-SUNTO-TOT-RECALC    TO BILL-SUNTO-TOT-RECALC                 
131400     MOVE WF-SUVAT-BILLIT-TOT-RECALC                                      
131500                                 TO BILL-SUVAT-BILLIT-TOT-RECALC          
131600     MOVE WF-SUBTO-TOT-RECALC    TO BILL-SUBTO-TOT-RECALC                 
131700     MOVE WF-KDVALISO-RECALC     TO BILL-KDVALISO-RECALC                  
131800     MOVE WF-PRKURS-RECALC       TO BILL-PRKURS-RECALC                    
131900     MOVE WF-KDSIGN-RECALC       TO BILL-KDTECKEN-RECALC                  
132000     MOVE WF-SUNTO-RECALC        TO BILL-SUNTO-LOCC-RECALC                
132100                                                                          
132200     MOVE WF-PRAVCOST            TO BILL-PRAVCOST-BILLIT                  
132300     MOVE WF-KDVALISO-AVC        TO BILL-KDVALISO-AVC                     
132400     MOVE WF-PRARTNTO            TO BILL-PRARTNTO                         
132500     MOVE WF-PRARTNTO-LOC        TO BILL-PRARTNTO-LOC                     
132600     MOVE WF-KDVALISO-NTO        TO BILL-KDVALISO-NTO                     
132700     MOVE WF-IDDC                TO BILL-IDDC-BILLIT                      
132800     MOVE WF-KVLEVART            TO BILL-KVLEVART                         
132900                                                                          
133000     MOVE WF-KDARTURS            TO BILL-KDARTURS                         
133100     MOVE WF-FLPCOO              TO BILL-FLPCOO                           
133200                                                                          
133300     MOVE WS-IDDISTR             TO TEST-IDDISTR                          
133400     IF BILL-IDDC NOT = DCS-IDDC                                          
133500        MOVE BILL-IDDC TO W-IDDC-B6                                       
133600        PERFORM IMS-GU-WDB601                                             
133700     END-IF                                                               
133800                                                                          
133900*      TRANS FÖR W500-SYSTEMET,  EKONOMI                                  
134000     MOVE 'W4063400'             TO FIL-IDPGM                             
134100     MOVE DAGENS-DATUM           TO FIL-TIREGDAT                          
134200***  MOVE WS-TIKLOCK             TO FIL-TIKLOCK                           
134300     ADD +1                      TO FIL-IDSEKVNR                          
134400     MOVE 'W476'                 TO FIL-CT-IDSYSTEM                       
134500     MOVE '510'                  TO FIL-CT-IDPTYP                         
134600     MOVE SPACE                  TO FIL-CT-IDVTYP                         
134700                                                                          
134800     MOVE BILL-W4760001          TO FIL-WDR701-DATA                       
134900                                                                          
135000     PERFORM IMS-ISRT-WDR701                                              
135100*FIX                                                                      
135200     IF BILL-IDDC = SPACE OR ZERO                                         
135300       CALL FELLOG                                                        
135400     END-IF                                                               
135500*                                                                         
135600     PERFORM UNTIL SEGMENT-FINNS                                          
135700        IF FIL-IDSEKVNR = 999                                             
135800          MOVE ZERO              TO FIL-IDSEKVNR                          
135900          ADD +1                 TO FIL-TIKLOCK                           
136000        END-IF                                                            
136100        ADD +1 TO FIL-IDSEKVNR                                            
136200        PERFORM IMS-ISRT-WDR701                                           
136300     END-PERFORM                                                          
136400                                                                          
136500     IF DCS-SDC AND DCS-IDLANDX2 = 'NL' AND                               
136600        (WS-IDDISTR = 2078 OR 2070)                                       
136700*        FRÅN DC21  TILL  SCHWEIZ                                         
136800*        TRANS FÖR FAKTURAINFO TILL SCHWEIZ                               
136900                                                                          
137000       MOVE JA                   TO FL-W476S5                             
137100     END-IF                                                               
137200                                                                          
137300     .                                                                    
137400     SKIP3                                                                
137500 GFA-HAMTA-KDRABATT SECTION.                                              
137600     MOVE 'GFA-HAMTA-KDRABATT'     TO WS-SEKTION                          
137700                                                                          
137800     MOVE ZERO                     TO BILL-KDARTRAB                       
137900     IF BRAD-KDARTRAB NOT = ZERO                                          
138000       MOVE BRAD-KDARTRAB          TO BILL-KDARTRAB                       
138100     END-IF                                                               
138200     .                                                                    
138300     EJECT                                                                
138400 GG-FAKTURARAD-R4 SECTION.                                                
138500     MOVE 'GG-FAKTURARAD-R4'       TO WS-SEKTION                          
138600                                                                          
138700     MOVE WS-IDPRODNR              TO W-IDPRODNR-4510                     
138800     IF WDE2-BILL-IDDC NOT = DCS-IDDC                                     
138900        MOVE WDE2-BILL-IDDC TO W-IDDC-B6                                  
139000        PERFORM IMS-GU-WDB601                                             
139100     END-IF                                                               
139200                                                                          
139300                                                                          
139400     MOVE WS-IDKOLLI               TO  W-IDKOLLI-4510                     
139500     MOVE WS-IDPURAD               TO  W-IDPURAD-4510                     
139600     PERFORM S30-BILL-TILL-4510                                           
139700                                                                          
139800     PERFORM IMS-ISRT-WDGX4510                                            
139900*FIX                                                                      
140000     IF 4510-IDDC = SPACE OR ZERO                                         
140100       CALL FELLOG                                                        
140200     END-IF                                                               
140300*                                                                         
140400     IF WS-IDKOLLI = ZERO AND WS-IDPURAD = ZERO                           
140500       PERFORM S13-WDR4-FINNS                                             
140600     END-IF                                                               
140700                                                                          
140800     .                                                                    
140900     EJECT                                                                
141000 H-FRAKTER  SECTION.                                                      
141100     MOVE 'H-FRAKTER'            TO WS-SEKTION                            
141200                                                                          
141300     MOVE ZERO                   TO ART-KDPRODSL                          
141400                                    ART-IDFKNGRP                          
141500*    MOVE 'R'                    TO BKOLLI-KDFAKTYP                       
141600                                                                          
141700     PERFORM IMS-GU-WDE111                                                
141800     MOVE SGMT-FLCOD             TO BILL-FLCOD                            
141900                                                                          
142000     PERFORM GA-HUVUD-R4                                                  
142100     PERFORM GF-FAKTURARAD-R7                                             
142200     PERFORM GG-FAKTURARAD-R4                                             
142300     .                                                                    
142400     EJECT                                                                
142500                                                                          
142600 Z-FINIT  SECTION.                                                        
142700     MOVE 'Z-FINIT'              TO WS-SEKTION                            
142800                                                                          
142900                                                                          
143000     .                                                                    
143100     EJECT                                                                
143200 S01-RECV-OPEN SECTION.                                                   
143300     MOVE 'S01-RECV-OPEN'        TO WS-SEKTION                            
143400                                                                          
143500     MOVE 'OPEN'                   TO RECV-KDFUNC                         
143600     MOVE 'CARPARTS.PULS.SAVEIT'   TO RECV-ADDISPABS                      
143700                                                                          
143800     CALL WZ01RECV USING           RECV-CONTROL-AREA                      
143900                                   RECV-OPEN-AREA                         
144000                                                                          
144100     IF RECV-KDRC > 0                                                     
144200      MOVE RECV-KDRC               TO KDRC-DISP                           
144300      STRING 'WZ01RECV OPEN ERROR RC= ' KDRC-DISP                         
144400        DELIMITED BY SIZE INTO FELTEXT                                    
144500      CALL FELLOG                                                         
144600     END-IF                                                               
144700     .                                                                    
144800     EJECT                                                                
144900 S02-RECV-MESSAGE SECTION.                                                
145000     MOVE 'S02-RECV-MESSAGE'     TO WS-SEKTION                            
145100                                                                          
145200     MOVE 'GET'                  TO RECV-KDFUNC                           
145300     MOVE LENGTH OF RECV-DATA    TO RECV-KVDLEN                           
145400     CALL WZ01RECV USING RECV-CONTROL-AREA                                
145500                         RECV-KVDLEN                                      
145600                         RECV-DATA                                        
145700*                                                                         
145800     IF RECV-KDRC > 1                                                     
145900       MOVE RECV-KDRC            TO KDRC-DISP                             
146000       STRING 'WZ01RECV GET  ERROR RC= ' KDRC-DISP                        
146100         DELIMITED BY SIZE INTO FELTEXT                                   
146200       CALL FELLOG                                                        
146300     END-IF                                                               
146400     .                                                                    
146500     EJECT                                                                
146600 S03-RECV-CLOSE SECTION.                                                  
146700     MOVE 'S03-RECV-CLOSE'       TO WS-SEKTION                            
146800                                                                          
146900     MOVE 'CLOSE'                TO RECV-KDFUNC                           
147000     CALL WZ01RECV     USING        RECV-CONTROL-AREA                     
147100*                                                                         
147200     IF RECV-KDRC > 0                                                     
147300       MOVE RECV-KDRC            TO KDRC-DISP                             
147400       STRING 'WZ01RECV CLOSE ERROR RC= ' KDRC-DISP                       
147500         DELIMITED BY SIZE INTO FELTEXT                                   
147600       CALL FELLOG                                                        
147700     END-IF                                                               
147800     .                                                                    
147900     EJECT                                                                
148000 S13-WDR4-FINNS  SECTION.                                                 
148100                                                                          
148200     PERFORM UNTIL SEGMENT-FINNS                                          
148300        SUBTRACT 1 FROM 4510-IDPURAD                                      
148400                                                                          
148500                                                                          
148600        PERFORM IMS-ISRT-WDGX4510                                         
148700     END-PERFORM                                                          
148800     .                                                                    
148900     EJECT                                                                
149000 S20-SHIPPING-NA-PROFORMA SECTION.                                        
149100                                                                          
149200     MOVE WS-IDDISTR         TO TEST-IDDISTR                              
149300     IF DIST35-NA-CDC-RETURN      OR                                      
149400        DIST35-NA-NDC-RETURNS     OR                                      
149500        DIST35-NA-TRANSFER        OR                                      
149600        DIST35-REFILL-INOM-NA     OR                                      
149700        DIST18-SCRAP-NDC-SC       OR                                      
149800        DIS134-BYTESREN-NA                                                
149900***                                                                       
150000*** DESSA GÄLLER INTE LÄGRE, ATT TAS BORT                                 
150100***    (DIST74-SAUDI AND DCS-CDC)                                         
150200***     DIST74-DUBAI-TRADING                                              
150300***    (DIST10-LEVBIL-KUWAIT AND DCS-CDC)                                 
150400***    (DIST10-LEVBIL-BAREIN AND DCS-CDC)                                 
150500***    (DIST10-LEVBIL-SYRIEN AND DCS-CDC)                                 
150600***    (DIST10-LEVBIL-SAUDI-PV AND DCS-CDC)                               
150700                                                                          
150800*           STARTAR UPP  W4631  (SHIPDOK)                                 
150900*           GÄLLER EJ NYA 'STUDS'-REFILLDISTRIKT                          
151000        PERFORM S21-OPEN-WZ01                                             
151100        PERFORM S22-SEND-WZ01                                             
151200        PERFORM S23-CLOSE-WZ01                                            
151300     END-IF                                                               
151400                                                                          
151500     MOVE WDE2-BILL-IDDC TO WS-IDDC                                       
151600*           STARTAR UPP  W4631  (SHIPDOK)                                 
151700*           FÖR CA-ORDER LEVERERADE FRÅN US                               
151800     IF WS-IDDISTR = 7674 AND NDC-US                                      
151900                                                                          
152000        PERFORM S21-OPEN-WZ01                                             
152100        PERFORM S22-SEND-WZ01                                             
152200        PERFORM S23-CLOSE-WZ01                                            
152300     END-IF                                                               
152400     .                                                                    
152500     EJECT                                                                
152600 S21-OPEN-WZ01 SECTION.                                                   
152700     MOVE 'S21-OPEN-WZ01'       TO WS-SEKTION                             
152800                                                                          
152900     MOVE 'OPEN'                     TO SEND-KDFUNC                       
153000     MOVE 'CARPARTS.PULS.SHIPDOK '   TO SEND-ADDISPABS                    
153100     MOVE SPACE                      TO SEND-ADDISPABS-RETURN             
153200                                                                          
153300     CALL WZ01SEND   USING      SEND-CONTROL-AREA                         
153400                                SEND-OPEN-AREA                            
153500     IF SEND-KDRC > 0                                                     
153600       MOVE SEND-KDRC           TO KDRC-DISP                              
153700       STRING 'WZ01SEND-OPEN RC-ERR = ' KDRC-DISP                         
153800            DELIMITED BY SIZE INTO FELTEXT                                
153900       CALL FELLOG                                                        
154000     ELSE                                                                 
154100       MOVE SEND-IDCOM               TO WS-IDCOM                          
154200     END-IF                                                               
154300     .                                                                    
154400     EJECT                                                                
154500 S22-SEND-WZ01 SECTION.                                                   
154600     MOVE 'S22-SEND-WZ01'    TO WS-SEKTION                                
154700                                                                          
154800     MOVE 'PUT'                      TO SEND-KDFUNC                       
154900     COMPUTE SEND-KVDLEN = LENGTH OF 4631-MID-W4I63101                    
155000     MOVE SHIP-IDSHIPM       TO 4631-MID-IDSHIPM                          
155100     MOVE SPACE              TO 4631-MID-IDPRTLST                         
155200     MOVE SPACE              TO 4631-MID-IDDC-REC                         
155300     MOVE IDPGM              TO 4631-MID-IDPGM                            
155400                                                                          
155500     CALL WZ01SEND   USING      SEND-CONTROL-AREA                         
155600                                SEND-KVDLEN                               
155700                                4631-MID-W4I63101                         
155800                                                                          
155900     IF SEND-KDRC > 0                                                     
156000       MOVE SEND-KDRC           TO KDRC-DISP                              
156100       STRING 'WZ01SEND-PUT RC-ERR = ' KDRC-DISP                          
156200            DELIMITED BY SIZE INTO FELTEXT                                
156300       CALL FELLOG                                                        
156400     END-IF                                                               
156500     .                                                                    
156600     EJECT                                                                
156700 S23-CLOSE-WZ01   SECTION.                                                
156800     MOVE 'S23-CLOSE-WZ01'      TO WS-SEKTION                             
156900                                                                          
157000     MOVE 'CLOSE'               TO SEND-KDFUNC                            
157100                                                                          
157200     CALL WZ01SEND   USING      SEND-CONTROL-AREA                         
157300     IF SEND-KDRC > 0                                                     
157400       MOVE SEND-KDRC           TO KDRC-DISP                              
157500       STRING 'WZ01SEND-CLOSE RC-ERR = ' KDRC-DISP                        
157600            DELIMITED BY SIZE INTO FELTEXT                                
157700       CALL FELLOG                                                        
157800     END-IF                                                               
157900     .                                                                    
158000     EJECT                                                                
158100 S25-OPEN-WZ01 SECTION.                                                   
158200     MOVE 'S25-OPEN-WZ01'       TO WS-SEKTION                             
158300                                                                          
158400     MOVE 'OPEN'                     TO SEND-KDFUNC                       
158500     MOVE 'CARPARTS.PULS.BUILDIT '   TO SEND-ADDISPABS                    
158600     MOVE SPACE                      TO SEND-ADDISPABS-RETURN             
158700                                                                          
158800     CALL WZ01SEND   USING      SEND-CONTROL-AREA                         
158900                                SEND-OPEN-AREA                            
159000     IF SEND-KDRC > 0                                                     
159100       MOVE SEND-KDRC           TO KDRC-DISP                              
159200       STRING 'WZ01SEND-OPEN RC-ERR = ' KDRC-DISP                         
159300            DELIMITED BY SIZE INTO FELTEXT                                
159400       CALL FELLOG                                                        
159500     ELSE                                                                 
159600       MOVE SEND-IDCOM               TO WS-IDCOM                          
159700     END-IF                                                               
159800     .                                                                    
159900     EJECT                                                                
160000 S26-SEND-WZ01 SECTION.                                                   
160100     MOVE 'S26-SEND-WZ01'       TO WS-SEKTION                             
160200                                                                          
160300     MOVE 'PUT'                 TO SEND-KDFUNC                            
160400     COMPUTE SEND-KVDLEN = LENGTH OF MOD-MID-W40637I1                     
160500                                                                          
160600     CALL WZ01SEND   USING      SEND-CONTROL-AREA                         
160700                                SEND-KVDLEN                               
160800                                MOD-MID-W40637I1                          
160900     IF SEND-KDRC > 0                                                     
161000       MOVE SEND-KDRC           TO KDRC-DISP                              
161100       STRING 'WZ01SEND-PUT RC-ERR = ' KDRC-DISP                          
161200            DELIMITED BY SIZE INTO FELTEXT                                
161300       CALL FELLOG                                                        
161400     END-IF                                                               
161500     .                                                                    
161600     EJECT                                                                
161700 S27-OPEN-SHIP2TMS SECTION.                                               
161800     MOVE 'S27-OPEN-SHIP2TMS'    TO WS-SEKTION                            
161900                                                                          
162000     MOVE 'OPEN'                 TO SEND-KDFUNC                           
162100     MOVE 'CARPARTS.PULS.SHIP2TMS'                                        
162200                                 TO SEND-ADDISPABS                        
162300                                                                          
162400     CALL WZ01SEND            USING SEND-CONTROL-AREA                     
162500                                    SEND-OPEN-AREA                        
162600     IF SEND-KDRC > 0                                                     
162700       MOVE SEND-KDRC            TO KDRC-DISP                             
162800       STRING 'WZ01SEND-OPEN SHIP2TMS RC-ERR = ' KDRC-DISP                
162900             DELIMITED BY SIZE INTO FELTEXT                               
163000       CALL FELLOG                                                        
163100     ELSE                                                                 
163200       MOVE SEND-IDCOM           TO WS-IDCOM                              
163300     END-IF                                                               
163400     .                                                                    
163500                                                                          
163600 S28-PUT-SHIP2TMS SECTION.                                                
163700     MOVE 'S28-PUT-SHIP2TMS'     TO WS-SEKTION                            
163800                                                                          
163900     MOVE 'PUT'                  TO SEND-KDFUNC                           
164000     MOVE LENGTH OF 4679-AREA    TO SEND-KVDLEN                           
164100                                                                          
164200     CALL WZ01SEND            USING SEND-CONTROL-AREA                     
164300                                    SEND-KVDLEN                           
164400                                    4679-AREA                             
164500     IF SEND-KDRC > 1                                                     
164600       MOVE SEND-KDRC            TO KDRC-DISP                             
164700       STRING 'WZ01SEND-PUT RC-ERR = ' KDRC-DISP                          
164800             DELIMITED BY SIZE INTO FELTEXT                               
164900       CALL FELLOG                                                        
165000     END-IF                                                               
165100     .                                                                    
165200                                                                          
165300 S29-CLOSE-SHIP2TMS SECTION.                                              
165400     MOVE 'S29-CLOSE-SHIP2TMS'   TO WS-SEKTION                            
165500                                                                          
165600     MOVE 'CLOSE'                TO SEND-KDFUNC                           
165700                                                                          
165800     CALL WZ01SEND            USING SEND-CONTROL-AREA                     
165900     IF SEND-KDRC > 0                                                     
166000       MOVE SEND-KDRC            TO KDRC-DISP                             
166100       STRING 'WZ01SEND-CLOSE SHIP2TMS RC-ERR = ' KDRC-DISP               
166200             DELIMITED BY SIZE INTO FELTEXT                               
166300       CALL FELLOG                                                        
166400     END-IF                                                               
166500     .                                                                    
166600                                                                          
166700 S30-BILL-TILL-4510  SECTION.                                             
166800                                                                          
166900     MOVE BILL-IDPRODNR        TO 4510-IDPRODNR                           
167000     MOVE BILL-IDKOLLI         TO 4510-IDKOLLI                            
167100     MOVE BILL-IDPURAD         TO 4510-IDPURAD                            
167200     MOVE BILL-DAFINDOC        TO 4510-DAFINDOC                           
167300     MOVE BILL-IDDISTR         TO 4510-IDDISTR                            
167400     MOVE BILL-IDKUNDNR        TO 4510-IDKUNDNR                           
167500*    MOVE BILL-IDFAKT          TO 4510-IDFAKT                             
167600     MOVE BILL-IDPARTNR        TO 4510-IDPARTNR                           
167700     MOVE BILL-IDSHIPM         TO 4510-IDSHIPM                            
167800     MOVE BILL-IDORDNR7        TO 4510-IDORDNR5                           
167900*    DC'T SOM ÄR DET FAKTURERANDE DC'T                                    
168000*    SE KOD FÖR WS-IDDC-SEND                                              
168100     MOVE BILL-IDDC            TO 4510-IDDC                               
168200     MOVE BILL-TISKEPPN        TO 4510-TISKEPPN                           
168300     MOVE BILL-KDVALISO-FAKT   TO 4510-KDVALISO                           
168400     MOVE BILL-SUBTO-TOT       TO 4510-SUBTO-TOT                          
168500     MOVE BILL-SUNTO-TOT       TO 4510-SUNTO-TOT                          
168600     MOVE BILL-SUVAT-FAKT      TO 4510-SUVAT-FAKT                         
168700     MOVE BILL-BEART           TO 4510-BEART                              
168800     MOVE BILL-BEART           TO 4510-BEART                              
168900     MOVE BILL-VKARTNTO        TO 4510-VKARTNTO                           
169000     MOVE BILL-SUBTO-LINE      TO 4510-SUBTO-LINE                         
169100     MOVE BILL-SUNTO-LINE      TO 4510-SUNTO-LINE                         
169200     MOVE BILL-SUVAT-LINE      TO 4510-SUVAT-LINE                         
169300     MOVE BILL-IDFKNGRP        TO 4510-IDFKNGRP                           
169400     MOVE BILL-KDARTRAB        TO 4510-KDARTRAB                           
169500     MOVE BILL-KDPRODSL        TO 4510-KDPRODSL                           
169600     MOVE BILL-PRAVCOST        TO 4510-PRAVCOST                           
169700     MOVE BILL-PRAVCOST-CORE   TO 4510-PRAVCOST-CORE                      
169800     MOVE BILL-TIFINDOC        TO 4510-TIFINDOC                           
169900     MOVE BILL-KDVALISO-BET    TO 4510-KDVALISO-BET                       
170000     MOVE BILL-PRKURS-BET      TO 4510-PRKURS-BET                         
170100     MOVE BILL-PRKURS-FAKT     TO 4510-PRKURS-FAKT                        
170200     MOVE BILL-PRKURS-FIKTIV   TO 4510-PRKURS-FIKTIV                      
170300     MOVE BILL-KDLEVVIL        TO 4510-KDLEVVIL                           
170400     MOVE WF-KDVALISO-SND      TO 4510-KDVALISO-SND                       
170500     MOVE WF-PRKURS-SND        TO 4510-PRKURS-SND                         
170600     MOVE BILL-FLCOD           TO 4510-FLCOD                              
170700*    DC'T SOM LEVERERAR DET FYSYSKA GODSET                                
170800     MOVE WDE2-BILL-IDDC       TO 4510-IDDC-LEV                           
170900     IF WS-SOFTWARE = NEJ                                                 
171000       MOVE SHIP-KDFAKSTA-EXP  TO 4510-KDFAKSTA-EXP                       
171100     ELSE                                                                 
171200       MOVE ZERO               TO 4510-KDFAKSTA-EXP                       
171300     END-IF                                                               
171400                                                                          
171500     MOVE BILL-SUNTO-PART-LOC  TO 4510-SUNTO-PART-LOC                     
171600     MOVE BILL-SUVAT-BILLIT-TOT-PART-L                                    
171700                               TO 4510-SUVAT-BILLIT-TOT-PART-L            
171800     MOVE BILL-SUBTO-TOT-PART-LOC                                         
171900                               TO 4510-SUBTO-TOT-PART-LOC                 
172000     MOVE BILL-SUNTO-TOT-LOC   TO 4510-SUNTO-TOT-LOC                      
172100     MOVE BILL-SUVAT-BILLIT-TOT-LOC                                       
172200                               TO 4510-SUVAT-BILLIT-TOT-LOC               
172300     MOVE BILL-SUBTO-TOT-LOC   TO 4510-SUBTO-TOT-LOC                      
172400     MOVE BILL-KDVALISO-LOC    TO 4510-KDVALISO-LOC                       
172500     MOVE BILL-PRKURS-LOC      TO 4510-PRKURS-LOC                         
172600     MOVE BILL-KDTECKEN-LOC    TO 4510-KDTECKEN-LOC                       
172700     MOVE BILL-SUNTO-LOCC      TO 4510-SUNTO-LOCC                         
172800     MOVE BILL-SUNTO-PART-RECALC                                          
172900                               TO 4510-SUNTO-PART-RECALC                  
173000     MOVE BILL-SUVAT-BILLIT-TOT-PART-R                                    
173100                               TO 4510-SUVAT-BILLIT-TOT-PART-R            
173200     MOVE BILL-SUBTO-TOT-PART-RECALC                                      
173300                               TO 4510-SUBTO-TOT-PART-RECALC              
173400     MOVE BILL-SUNTO-TOT-RECALC                                           
173500                               TO 4510-SUNTO-TOT-RECALC                   
173600     MOVE BILL-SUVAT-BILLIT-TOT-RECALC                                    
173700                               TO 4510-SUVAT-BILLIT-TOT-RECALC            
173800     MOVE BILL-SUBTO-TOT-RECALC                                           
173900                               TO 4510-SUBTO-TOT-RECALC                   
174000     MOVE BILL-KDVALISO-RECALC TO 4510-KDVALISO-RECALC                    
174100     MOVE BILL-PRKURS-RECALC   TO 4510-PRKURS-RECALC                      
174200     MOVE BILL-KDTECKEN-RECALC TO 4510-KDTECKEN-RECALC                    
174300     MOVE BILL-SUNTO-LOCC-RECALC                                          
174400                               TO 4510-SUNTO-LOCC-RECALC                  
174500                                                                          
174510*BELOW LINE CHANGE IS FOR CCID-4157508/NEW ITEM TO BILLIT                 
174600     MOVE BILL-PRAVCOST-BILLIT TO 4510-PRAVCOST-BILLIT                    
174700     MOVE BILL-KDVALISO-AVC    TO 4510-KDVALISO-AVC                       
174800     MOVE BILL-PRARTNTO        TO 4510-PRARTNTO                           
174900     MOVE BILL-PRARTNTO-LOC    TO 4510-PRARTNTO-LOC                       
175000     MOVE BILL-KDVALISO-NTO    TO 4510-KDVALISO-NTO                       
175100     MOVE BILL-IDDC            TO 4510-IDDC-BILLIT                        
175200     MOVE BILL-KVLEVART        TO 4510-KVLEVART                           
175300     MOVE BILL-KDARTURS        TO 4510-KDARTURS                           
175400     MOVE BILL-FLPCOO          TO 4510-FLPCOO                             
175500                                                                          
175600     .                                                                    
175700     EJECT                                                                
175800                                                                          
175900* --- IMS SEKTIONER ---                                                   
176000     SKIP3                                                                
176100*IMS-INSERT-ALTMSG-SOP SECTION.                                           
176200*    MOVE 'IMS-INSERT-ALTMSG'    TO WS-SEKTION                            
176300*    MOVE SPACE TO GODK-STATUSKODER                                       
176400*    CALL CBLTDLI USING PURG ALT-PCB PROG-TO-PROG-SW                      
176500*    MOVE ALT-STATUS-CODE TO STATUS-WS                                    
176600*    PERFORM IMS-STATUSKONTROLL                                           
176700*    .                                                                    
176800*    EJECT                                                                
176900 IMS-GHU-WDE101 SECTION.                                                  
177000     MOVE 'IMS-GHU-WDE101'       TO WS-SEKTION                            
177100                                                                          
177200     STRING 'WDE101  (IDSHIPM  =' W-IDSHIPM-X ')'                         
177300          DELIMITED BY SIZE INTO SSA1                                     
177400     MOVE '  GE' TO GODK-STATUSKODER                                      
177500     CALL CBLTDLI USING GHU WDE1-PCB DLI-IO-WDE101 SSA1                   
177600     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
177700     PERFORM IMS-STATUSKONTROLL                                           
177800     .                                                                    
177900     SKIP3                                                                
178000 IMS-REPL-WDE101 SECTION.                                                 
178100     MOVE 'IMS-REPL-WDE101'      TO WS-SEKTION                            
178200                                                                          
178300     MOVE '    ' TO GODK-STATUSKODER                                      
178400     CALL CBLTDLI USING REPL WDE1-PCB DLI-IO-WDE101                       
178500     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
178600     PERFORM IMS-STATUSKONTROLL                                           
178700     .                                                                    
178800     EJECT                                                                
178900 IMS-GU-WDE111 SECTION.                                                   
179000     MOVE 'IMS-GU-WDE111'        TO WS-SEKTION                            
179100                                                                          
179200     STRING 'WDE101  (IDSHIPM  =' W-IDSHIPM-X ')'                         
179300          DELIMITED BY SIZE INTO SSA1                                     
179400     MOVE   'WDE111 ' TO SSA2                                             
179500     MOVE '  GE' TO GODK-STATUSKODER                                      
179600     CALL CBLTDLI USING GU WDE1-PCB DLI-IO-WDE111 SSA1 SSA2               
179700     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
179800     PERFORM IMS-STATUSKONTROLL                                           
179900     .                                                                    
180000     SKIP3                                                                
180100 IMS-GHNP-WDE121 SECTION.                                                 
180200     MOVE 'IMS-GHNP-WDE121'      TO WS-SEKTION                            
180300                                                                          
180400     STRING 'WDE111  (WDE111KY =' W-WDE111KY-X ')'                        
180500          DELIMITED BY SIZE INTO SSA1                                     
180600     STRING 'WDE121  (WDE121KY =' W-WDE121KY-X ')'                        
180700          DELIMITED BY SIZE INTO SSA2                                     
180800     MOVE '  GE' TO GODK-STATUSKODER                                      
180900     CALL CBLTDLI USING GHNP WDE1-PCB DLI-IO-WDE121 SSA1 SSA2             
181000     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
181100     PERFORM IMS-STATUSKONTROLL                                           
181200     .                                                                    
181300     SKIP3                                                                
181400 IMS-REPL-WDE121 SECTION.                                                 
181500     MOVE 'IMS-REPL-WDE121'      TO WS-SEKTION                            
181600                                                                          
181700     MOVE '  ' TO GODK-STATUSKODER                                        
181800     CALL CBLTDLI USING REPL WDE1-PCB DLI-IO-WDE121                       
181900     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
182000     PERFORM IMS-STATUSKONTROLL                                           
182100     .                                                                    
182200     EJECT                                                                
182300 IMS-GU-WDE201 SECTION.                                                   
182400     MOVE 'IMS-GU-WDE201'       TO WS-SEKTION                             
182500                                                                          
182600     STRING 'WDE201  (IDSHIPM  =' W-IDSHIPM-X ')'                         
182700          DELIMITED BY SIZE INTO SSA1                                     
182800     MOVE '  GE' TO GODK-STATUSKODER                                      
182900     CALL CBLTDLI USING GU WDE2-PCB DLI-IO-WDE201 SSA1                    
183000     MOVE WDE2-STATUS-CODE TO STATUS-WS                                   
183100     PERFORM IMS-STATUSKONTROLL                                           
183200     .                                                                    
183300     SKIP3                                                                
183400 IMS-GNP-WDE211 SECTION.                                                  
183500     MOVE 'IMS-GNP-WDE211'       TO WS-SEKTION                            
183600                                                                          
183700     STRING 'WDE211  (WDE211KY =' W-WDE111KY-X ')'                        
183800          DELIMITED BY SIZE INTO SSA1                                     
183900     MOVE '    ' TO GODK-STATUSKODER                                      
184000     CALL CBLTDLI USING GNP WDE2-PCB DLI-IO-WDE211 SSA1                   
184100     MOVE WDE2-STATUS-CODE TO STATUS-WS                                   
184200     PERFORM IMS-STATUSKONTROLL                                           
184300     .                                                                    
184400     SKIP3                                                                
184500 IMS-GNP-WDE231 SECTION.                                                  
184600     MOVE 'IMS-GNP-WDE231'        TO WS-SEKTION                           
184700                                                                          
184800     STRING 'WDE221  (WDE221KY =' W-WDE121KY-X ')'                        
184900          DELIMITED BY SIZE INTO SSA1                                     
185000     STRING 'WDE231  (IDPURAD  =' W-IDPURAD-X ')'                         
185100          DELIMITED BY SIZE INTO SSA2                                     
185200     MOVE '    ' TO GODK-STATUSKODER                                      
185300     CALL CBLTDLI USING GNP WDE2-PCB DLI-IO-WDE231 SSA1 SSA2              
185400     MOVE WDE2-STATUS-CODE TO STATUS-WS                                   
185500     PERFORM IMS-STATUSKONTROLL                                           
185600     .                                                                    
185700     SKIP3                                                                
185800 IMS-GHU-WDB201 SECTION.                                                  
185900     MOVE 'IMS-GHU-WDB201'       TO WS-SEKTION                            
186000                                                                          
186100     STRING 'WDB201  (IDGMT    =' W-IDGMT-X ')'                           
186200          DELIMITED BY SIZE INTO SSA1                                     
186300     MOVE '    ' TO GODK-STATUSKODER                                      
186400     CALL CBLTDLI USING GHU WDB2-PCB DLI-IO-WDB201 SSA1                   
186500     MOVE WDB2-STATUS-CODE TO STATUS-WS                                   
186600     PERFORM IMS-STATUSKONTROLL                                           
186700     .                                                                    
186800     SKIP3                                                                
186900 IMS-REPL-WDB201 SECTION.                                                 
187000     MOVE 'IMS-REPL-WDB201'      TO WS-SEKTION                            
187100                                                                          
187200     MOVE '  ' TO GODK-STATUSKODER                                        
187300     CALL CBLTDLI USING REPL WDB2-PCB DLI-IO-WDB201                       
187400     MOVE WDB2-STATUS-CODE TO STATUS-WS                                   
187500     PERFORM IMS-STATUSKONTROLL                                           
187600     .                                                                    
187700     EJECT                                                                
187800 IMS-GU-WDE401-ESEQ  SECTION.                                             
187900                                                                          
188000     STRING 'WDE401  (WDE4ESEQ =' W-WDE4ESEQ-X ')'                        
188100          DELIMITED BY SIZE INTO SSA1                                     
188200     MOVE '  GE' TO GODK-STATUSKODER                                      
188300     CALL CBLTDLI USING GU WDE4-PCB DLI-IO-WDE401 SSA1                    
188400     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
188500     PERFORM IMS-STATUSKONTROLL                                           
188600     .                                                                    
188700     SKIP3                                                                
188800 IMS-GHU-WDE601 SECTION.                                                  
188900     MOVE 'IMS-GHU-WDE601'       TO WS-SEKTION                            
189000                                                                          
189100     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-E601-X ')'                   
189200          DELIMITED BY SIZE INTO SSA1                                     
189300     MOVE '    ' TO GODK-STATUSKODER                                      
189400     CALL CBLTDLI USING GHU WDE6-PCB DLI-IO-WDE601 SSA1                   
189500     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
189600     PERFORM IMS-STATUSKONTROLL                                           
189700     .                                                                    
189800     SKIP3                                                                
189900 IMS-REPL-WDE601 SECTION.                                                 
190000     MOVE 'IMS-REPL-WDE601'      TO WS-SEKTION                            
190100                                                                          
190200     MOVE '  ' TO GODK-STATUSKODER                                        
190300     CALL CBLTDLI USING REPL WDE6-PCB DLI-IO-WDE601                       
190400     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
190500     PERFORM IMS-STATUSKONTROLL                                           
190600     .                                                                    
190700     EJECT                                                                
190800 IMS-GHNP-WDE611 SECTION.                                                 
190900     MOVE 'IMS-GHNP-WDE611'       TO WS-SEKTION                           
191000                                                                          
191100     STRING 'WDE611  (IDKOLLI  =' W-IDKOLLI-E611-X ')'                    
191200          DELIMITED BY SIZE INTO SSA1                                     
191300     MOVE '    ' TO GODK-STATUSKODER                                      
191400     CALL CBLTDLI USING GHNP WDE6-PCB DLI-IO-WDE611 SSA1                  
191500     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
191600     PERFORM IMS-STATUSKONTROLL                                           
191700     .                                                                    
191800     SKIP3                                                                
191900 IMS-REPL-WDE611 SECTION.                                                 
192000     MOVE 'IMS-REPL-WDE611'      TO WS-SEKTION                            
192100                                                                          
192200     MOVE '  ' TO GODK-STATUSKODER                                        
192300     CALL CBLTDLI USING REPL WDE6-PCB DLI-IO-WDE611                       
192400     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
192500     PERFORM IMS-STATUSKONTROLL                                           
192600     .                                                                    
192700     EJECT                                                                
192800 IMS-GU-WDQ201 SECTION.                                                   
192900                                                                          
193000     STRING 'WDQ201  (IDORDER  =' W-IDORDER-Q2-X ')'                      
193100          DELIMITED BY SIZE INTO SSA1                                     
193200     MOVE '    ' TO GODK-STATUSKODER                                      
193300     CALL CBLTDLI USING GU WDQ2-PCB DLI-IO-WDQ201 SSA1                    
193400     MOVE WDQ2-STATUS-CODE TO STATUS-WS                                   
193500     PERFORM IMS-STATUSKONTROLL                                           
193600     .                                                                    
193700     SKIP3                                                                
193800 IMS-GHNP-WDQ211 SECTION.                                                 
193900     MOVE 'IMS-GHNP-WDQ211'       TO WS-SEKTION                           
194000                                                                          
194100     STRING 'WDQ211  (WDQ211KY =' W-WDQ211KY-X ')'                        
194200          DELIMITED BY SIZE INTO SSA1                                     
194300     MOVE '  GE' TO GODK-STATUSKODER                                      
194400     CALL CBLTDLI USING GHNP WDQ2-PCB DLI-IO-WDQ211 SSA1                  
194500     MOVE WDQ2-STATUS-CODE TO STATUS-WS                                   
194600     PERFORM IMS-STATUSKONTROLL                                           
194700     .                                                                    
194800     SKIP3                                                                
194900 IMS-REPL-WDQ211 SECTION.                                                 
195000     MOVE 'IMS-REPL-WDQ211'      TO WS-SEKTION                            
195100                                                                          
195200     MOVE '  ' TO GODK-STATUSKODER                                        
195300     CALL CBLTDLI USING REPL WDQ2-PCB DLI-IO-WDQ211                       
195400     MOVE WDQ2-STATUS-CODE TO STATUS-WS                                   
195500     PERFORM IMS-STATUSKONTROLL                                           
195600     .                                                                    
195700     EJECT                                                                
195800 IMS-GHNP-WDQ212 SECTION.                                                 
195900     MOVE 'IMS-GHNP-WDQ212'       TO WS-SEKTION                           
196000                                                                          
196100     STRING 'WDQ212  (IDDC     =' W-Q2-IDDC-X ')'                         
196200          DELIMITED BY SIZE INTO SSA1                                     
196300     MOVE '  ' TO GODK-STATUSKODER                                        
196400     CALL CBLTDLI USING GHNP WDQ2-PCB DLI-IO-WDQ212 SSA1                  
196500     MOVE WDQ2-STATUS-CODE TO STATUS-WS                                   
196600     PERFORM IMS-STATUSKONTROLL                                           
196700     .                                                                    
196800     SKIP3                                                                
196900 IMS-REPL-WDQ212 SECTION.                                                 
197000     MOVE 'IMS-REPL-WDQ212'      TO WS-SEKTION                            
197100                                                                          
197200     MOVE '  ' TO GODK-STATUSKODER                                        
197300     CALL CBLTDLI USING REPL WDQ2-PCB DLI-IO-WDQ212                       
197400     MOVE WDQ2-STATUS-CODE TO STATUS-WS                                   
197500     PERFORM IMS-STATUSKONTROLL                                           
197600     .                                                                    
197700     EJECT                                                                
197800 IMS-GU-WDK601 SECTION.                                                   
197900     MOVE 'IMS-GU-WDK601'       TO WS-SEKTION                             
198000                                                                          
198100     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
198200          DELIMITED BY SIZE INTO SSA1                                     
198300     MOVE '    ' TO GODK-STATUSKODER                                      
198400     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
198500     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
198600     PERFORM IMS-STATUSKONTROLL                                           
198700     .                                                                    
198800     SKIP3                                                                
198900 IMS-ISRT-WDR701 SECTION.                                                 
199000     MOVE 'IMS-ISRT-WDR701'      TO WS-SEKTION                            
199100                                                                          
199200     MOVE 'WDR701 ' TO SSA1                                               
199300     MOVE '  II' TO GODK-STATUSKODER                                      
199400     CALL CBLTDLI USING ISRT WDR7-PCB DLI-IO-WDR701 SSA1                  
199500     MOVE WDR7-STATUS-CODE TO STATUS-WS                                   
199600     PERFORM IMS-STATUSKONTROLL                                           
199700     .                                                                    
199800     EJECT                                                                
199900*IMS-GHU-WDR4-4507 SECTION.                                               
200000*    MOVE 'IMS-GHU-WDR4-4507'    TO WS-SEKTION                            
200100*                                                                         
200200*    STRING 'WDR401  (WDGXKEY  =' W-WDGXKEY-4507-X ')'                    
200300*         DELIMITED BY SIZE INTO SSA1                                     
200400*    MOVE '    ' TO GODK-STATUSKODER                                      
200500*    CALL CBLTDLI USING GHU 4507-PCB DLI-IO-WDGX4510 SSA1                 
200600*    MOVE 4507-STATUS-CODE TO STATUS-WS                                   
200700*    PERFORM IMS-STATUSKONTROLL                                           
200800*    .                                                                    
200900*    SKIP3                                                                
201000 IMS-ISRT-WDGX4508 SECTION.                                               
201100     MOVE 'IMS-ISRT-WDGX4508'    TO WS-SEKTION                            
201200                                                                          
201300     STRING 'WDR401  (WDGXKEY  =' W-WDGXKEY-4507-X ')'                    
201400          DELIMITED BY SIZE INTO SSA1                                     
201500     MOVE 'WDGX4508  '          TO SSA2                                   
201600     MOVE '  II' TO GODK-STATUSKODER                                      
201700     CALL CBLTDLI USING ISRT 4507-PCB DLI-IO-WDGX4508 SSA1 SSA2           
201800     MOVE 4507-STATUS-CODE TO STATUS-WS                                   
201900     PERFORM IMS-STATUSKONTROLL                                           
202000     .                                                                    
202100     EJECT                                                                
202200 IMS-GHU-WDGX4508 SECTION.                                                
202300     MOVE 'IMS-GHU-WDGX4508'    TO WS-SEKTION                             
202400                                                                          
202500     STRING 'WDR401  (WDGXKEY  =' W-WDGXKEY-4507-X ')'                    
202600          DELIMITED BY SIZE INTO SSA1                                     
202700     STRING 'WDGX4508*F(IDFAKT   =' W-IDFAKT-4508-X ')'                   
202800          DELIMITED BY SIZE INTO SSA2                                     
202900     MOVE '    ' TO GODK-STATUSKODER                                      
203000     CALL CBLTDLI USING GHU 4507-PCB DLI-IO-WDGX4508 SSA1 SSA2            
203100     MOVE 4507-STATUS-CODE TO STATUS-WS                                   
203200     PERFORM IMS-STATUSKONTROLL                                           
203300     .                                                                    
203400     EJECT                                                                
203500 IMS-REPL-WDGX4508 SECTION.                                               
203600     MOVE 'IMS-REPL-WDGX4508'    TO WS-SEKTION                            
203700                                                                          
203800     MOVE '    ' TO GODK-STATUSKODER                                      
203900     CALL CBLTDLI USING REPL 4507-PCB DLI-IO-WDGX4508                     
204000     MOVE 4507-STATUS-CODE TO STATUS-WS                                   
204100     PERFORM IMS-STATUSKONTROLL                                           
204200     .                                                                    
204300     EJECT                                                                
204400 IMS-ISRT-WDGX4510 SECTION.                                               
204500     MOVE 'IMS-ISRT-WDGX4510'    TO WS-SEKTION                            
204600                                                                          
204700     STRING 'WDGX4508(IDFAKT   =' W-IDFAKT-4508-X ')'                     
204800          DELIMITED BY SIZE INTO SSA1                                     
204900     MOVE 'WDGX4510'  TO SSA2                                             
205000     MOVE '    ' TO GODK-STATUSKODER                                      
205100     IF WS-IDKOLLI = ZERO AND WS-IDPURAD = ZERO                           
205200       MOVE '  II' TO GODK-STATUSKODER                                    
205300     END-IF                                                               
205400     CALL CBLTDLI USING ISRT 4507-PCB DLI-IO-WDGX4510 SSA1 SSA2           
205500     MOVE 4507-STATUS-CODE TO STATUS-WS                                   
205600     PERFORM IMS-STATUSKONTROLL                                           
205700     .                                                                    
205800     EJECT                                                                
205900 IMS-GU-WDB601    SECTION.                                                
206000     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
206100          DELIMITED BY SIZE INTO SSA1                                     
206200     MOVE '  GE' TO GODK-STATUSKODER                                      
206300     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
206400     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
206500     PERFORM IMS-STATUSKONTROLL                                           
206600     IF SEGMENT-SAKNAS                                                    
206700         MOVE SPACE TO DCS-KDDC                                           
206800     END-IF                                                               
206900     .                                                                    
207000 IMS-STATUSKONTROLL SECTION.                                              
207100                                                                          
207200     SET STATUS-IX TO 1                                                   
207300     SEARCH GODK-STATUS                                                   
207400       AT END                                                             
207500         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
207600         DELIMITED BY SIZE INTO FELTEXT                                   
207700         CALL FELLOG                                                      
207800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
207900         CONTINUE                                                         
208000     END-SEARCH                                                           
208100     .                                                                    
208200     SKIP2                                                                
