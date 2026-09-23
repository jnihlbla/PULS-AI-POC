000100*                                                                         
000200*****************************************************************         
000300*                                                                         
000400*     THIS PROGRAM ALSO HAS A WEB-LDC VERSION CALLED WL0187               
000500*                                                                         
000600*****************************************************************         
000700*                                                                         
000800 ID DIVISION.                                                             
000900                                                                          
001000 PROGRAM-ID.     W4066400.                                                
001100 AUTHOR.         LARS THELL.                                              
001200 DATE-WRITTEN.   94/09/05.                                                
001300 DATE-COMPILED.                                                           
001400                                                                          
001500*    FUNKTION:                                                            
001600*        DE KOLLIN PÅ EN VISS TRANSPORT SOM ÄR VALDA FÖR EN               
001700*        FAKT/LAST VISAS.                                                 
001800*        GENOM ATT AVSLUTA STARTAS FAKTURA RELEASEN                       
001900*               W40675 FÖR BILLIT-FAKT. RESP.                             
002000*               W40677 FÖR BILLIT-FAKT--PROFORMA.                         
002100*        VALDA KOLLIN KAN BACKAS SÅ ATT DE EJ LIGGER UNDER DENNA          
002200*        TRANSPORT/LASTBÄRARE SOM LASTNINGSRELEASADE LÄNGRE.              
002300*                                                                         
002400*        OBS! UTSKRIFT AV LASTLISTA STARTAS VIA PF4-TANGENT SOM           
002500*             STARTAR TRANS/PGM 4699.                                     
002600*                                                                         
002700*        PROGRAMMET UPPDATERAR WDE6                                       
002800*        PROGRAMMET UPPDATERAR WDE7                                       
002900*        PROGRAMMET UPPDATERAR 4495 (WDR4)                                
003000*                                                                         
003100*    INDATA.                                                              
003200*        TRANSAKTION: W4T664                                              
003300*        MID:         W4I66401                                            
003400*                                                                         
003500*    UTDATA.                                                              
003600*        MOD:         W4O66401                                            
003700                                                                          
003800     SKIP3                                                                
003900*    041110  SM   TILLÄGG AV LASTDOKUMENT-FLAGGA FÖR SYD-AMERIKA          
004000*                 W40639    STARTAS UPP                                   
004100*    161201  CO   TILLÄGG - NY BEHANDLING AV SAMLINGSKOLLIN SOM           
004200*                 MAN HAR SKANNAT IN PÅ 4673 ELLER VALT PÅ 4663           
004300*                                                                         
004400 ENVIRONMENT DIVISION.                                                    
004500     EJECT                                                                
004600 DATA DIVISION.                                                           
004700 WORKING-STORAGE SECTION.                                                 
004800                                                                          
004900*    -- CHECKED BY WY2000                                                 
005000 77  IDPGM                       PIC X(08)   VALUE 'W4066400'.            
005100                                                                          
005200*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
005300 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
005400                                                                          
005500 77  JA                          PIC X       VALUE 'J'.                   
005600 77  YES                         PIC X       VALUE 'Y'.                   
005700 77  NEJ                         PIC X       VALUE 'N'.                   
005800                                                                          
005900 77  WS-IDDC                     PIC X(2)    VALUE 'N'.                   
006000                                                                          
006100 77  W-SPAR-IDKUNDNR             PIC S9(7)   VALUE ZERO COMP-3.           
006200 77  W-DATE-AAMM                 PIC 9(4)    VALUE ZERO.                  
006300 77  WS-KDVALISO-HUV             PIC X(3)    VALUE 'SEK'.                 
006400*    --- INDEX FÖR BLÄDDRINGSRADER                                        
006500 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
006600 77  MAX-INDX                    PIC S9(4)  VALUE +10   COMP SYNC.        
006700 77  START-INDX                  PIC S9(4)  VALUE +0    COMP SYNC.        
006800 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
006900 77  TAB-IX                      PIC S9(9)  VALUE +0    COMP SYNC.        
007000*    --- DET RÄTTA VÄRDET PÅ NEDANSTÅENDE FÄLT SÄTTS I A-INIT             
007100 77  LNG-P-TO-P-PREFIX           PIC S9(4)  VALUE +17   COMP SYNC.        
007200                                                                          
007300*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
007400 77  WS-IDTRPTNR                 PIC X(3)    VALUE SPACE.                 
007500 77  WS-IDLBBET                  PIC X(12)   VALUE SPACE.                 
007600 77  WS-FLFARLIG                 PIC X(1)    VALUE SPACE.                 
007700 77  WS-FLTRPDOK                 PIC X(1)    VALUE SPACE.                 
007800 77  WS-FLLSTDOK                 PIC X(1)    VALUE SPACE.                 
007900 77  WS-FLPROFORMA               PIC X(1)    VALUE SPACE.                 
008000                                                                          
008100 77  INDATA-SW                   PIC X       VALUE 'J'.                   
008200     88  INDATA-OK                           VALUE 'J'.                   
008300     88  INDATA-FEL                          VALUE 'N'.                   
008400                                                                          
008500 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
008600     88  NYCKLAR-OK                          VALUE 'J'.                   
008700     88  NYCKLAR-FEL                         VALUE 'N'.                   
008800                                                                          
008900 77  UPDATE-SAVE-SW              PIC X(01)   VALUE 'N'.                   
009000     88  UPDATE-SAVE                         VALUE 'J'.                   
009100                                                                          
009200 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
009300     88  EGEN-MID                            VALUE '4664'.                
009400     88  GODK-MID                            VALUE '4663' '4664'          
009500                                                   '4675'                 
009600                                                   '4677'                 
009700                                                   '4687'.                
009800     88  HELP-MID                            VALUE '0551'.                
009900     EJECT                                                                
010000 01  FILLER                  PIC X(16) VALUE 'SWITCHAR'.                  
010100 77  W-FUNKTION              PIC S9(1) COMP-3 VALUE +0.                   
010200     88  AVSLUTA                              VALUE +1.                   
010300     88  BACKA-VALDA-RADER                    VALUE +2.                   
010400     88  PROFORMA                             VALUE +4.                   
010500     88  LASTDOK                              VALUE +5.                   
010600                                                                          
010700 77  SW-RAD-INPUT            PIC  X(1)        VALUE 'N'.                  
010800                                                                          
010900 77  SW-OMSTART              PIC  X(1)        VALUE 'N'.                  
011000     88 OMSTART                               VALUE 'J'.                  
011100                                                                          
011200 77  SW-FLTRANS              PIC  X(1)        VALUE 'N'.                  
011300     88 FL-XTRANS                             VALUE 'J'.                  
011400                                                                          
011500 77  TRAFF-SW                PIC  X(1)        VALUE 'N'.                  
011600     88  TRAFF                                VALUE 'J'.                  
011700                                                                          
011800 77  4498-FINNS-SW           PIC  X(1)        VALUE 'N'.                  
011900     88  4498-FINNS                           VALUE 'J'.                  
012000                                                                          
012100 01  FILLER                  PIC X(16) VALUE 'ARBETSFALT'.                
012200 77  W-IDDISTR-ALFA          PIC  X(4)   VALUE SPACE.                     
012300 77  W-REG-IDPRODNR              PIC S9(7)   VALUE ZERO COMP-3.           
012400 77  W-KVKOLLI                   PIC S9(3)   VALUE ZERO COMP-3.           
012500 77  W-VLORDBTO            PIC S9(4)V9(3)    VALUE ZERO COMP-3.           
012600 77  W-VKORDBTO            PIC S9(6)V9(1)    VALUE ZERO COMP-3.           
012700 77  W-SUORDV              PIC S9(9)V9(2)    VALUE ZERO COMP-3.           
012800 77  W-SUORDV-TOT          PIC S9(9)V9(2)    VALUE ZERO COMP-3.           
012900 77  W-SUORDV-EXP          PIC S9(9)V9(2)    VALUE ZERO COMP-3.           
013000 77  W-SUORDV-TOT-EXP      PIC S9(9)V9(2)    VALUE ZERO COMP-3.           
013100 77  W-IDPSN-NUM           PIC  9(3)         VALUE ZERO.                  
013200 77  W-SPAR-VLORDBTO       PIC S9(4)V9(3)    VALUE ZERO COMP-3.           
013300 77  W-SPAR-VKORDBTO       PIC S9(6)V9(1)    VALUE ZERO COMP-3.           
013400 77  W-SPAR-SUORDV         PIC S9(9)V9(2)    VALUE ZERO COMP-3.           
013500 77  W-SPAR-SUORDV-LOC     PIC S9(9)V9(2)    VALUE ZERO COMP-3.           
013600 77  W-SPAR-SUORDV-LOCPREL PIC S9(9)V9(2)    VALUE ZERO COMP-3.           
013700 77  W-UPD-RAKNARE           PIC S9(3)   VALUE ZERO.                      
013800 77  W-UPD-COPY              PIC S9(3)   VALUE ZERO.                      
013900 77  W-UPD-MAX               PIC S9(3)   VALUE +100.                      
014000 77  WS-IDTIDZON             PIC 9(2).                                    
014100 77  WS-VLORDBTO             PIC S9(4)V9(3)  VALUE ZERO COMP-3.           
014200 77  WS-VKORDBTO             PIC S9(6)V9(1)  VALUE ZERO COMP-3.           
014300 77  WS-KDMATT               PIC X.                                       
014400 77  WS-IDDC-IN              PIC X(2) VALUE ZERO.                         
014500 77  RKOD-ABEND-MED-DUMP     PIC S9(4)   VALUE +33 COMP SYNC.             
014600                                                                          
014700 01  FILLER                  PIC X(16) VALUE 'KONSTANTER'.                
014800 01  KONSTANTER.                                                          
014900     03  KLI-PACK            PIC S9(1) COMP-3 VALUE +1.                   
015000     03  KLI-PACK-FAKT       PIC S9(1) COMP-3 VALUE +6.                   
015100     03  SK-CLOSED           PIC  X(1)        VALUE 'C'.                  
015200     03  W-AVSLUTA           PIC S9(1) COMP-3 VALUE +1.                   
015300     03  W-BACKA-VALDA-RADER PIC S9(1) COMP-3 VALUE +2.                   
015400     03  W-PROFORMA          PIC S9(1) COMP-3 VALUE +4.                   
015500     03  W-LASTDOK           PIC S9(1) COMP-3 VALUE +5.                   
015600     03  W-VALD              PIC  X(1)        VALUE 'X'.                  
015700                                                                          
015800*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
015900 01  GENERELLA-SUBPROGRAM.                                                
016000     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
016100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
016200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
016300     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
016400     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
016500     03  W411EXCH                PIC X(8)    VALUE 'W411EXCH'.            
016600     03  W510CURR                PIC X(8)    VALUE 'W510CURR'.            
016700     EJECT                                                                
016800*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
016900*01 -COPY WMSGINIT                                                        
017000     EJECT                                                                
017100*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
017200*01 -COPY WMEDAREA                                                        
017300     EJECT                                                                
017400*    --- PARAMETRAR TILL COPYTEXT   WWOMVAND                              
017500*01 -COPY WWOMVAND                                                        
017600     SKIP3                                                                
017700*    --- PARAMETRAR TILL COPYTEXT   W510CURR                              
017800*01 -COPY W510CURR                                                        
017900     SKIP3                                                                
018000*    --- PARAMETRAR TILL COPYTEXT   W411EXCH                              
018100 01  FILLER         PIC X(16)   VALUE 'OMRÄKNA VALUTA'.                   
018200*    -COPY W411EXCH                                                       
018300     EJECT                                                                
018400 01  MESSAGE-CODES.                                                       
018500     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
018600     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
018700     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
018800     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
018900     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
019000     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
019100     03  INF-PROFORMA-REL        PIC X(3)    VALUE '096'.                 
019200     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
019300     EJECT                                                                
019400 01  TEST-IDDISTR            PIC  9(5) COMP-3 VALUE ZERO.                 
019500*01  FILLER  -COPY WWDIST79 -RED TEST-IDDISTR.                            
019600*    ----DISTR-DEALER-/BILLIT-FAKT------                                  
019700     EJECT                                                                
019800*01  FILLER  -COPY W476DIST.                                              
019900*    ----TABELL FÖR X-TRANS--------                                       
020000     EJECT                                                                
020100*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
020200*                                                                         
020300 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
020400     SKIP3                                                                
020500*01  MID -COPY W4I66401                                                   
020600     EJECT                                                                
020700 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
020800     SKIP3                                                                
020900*01  -COPY WMSGAREA                                                       
021000     EJECT                                                                
021100     03  MOD REDEFINES MSG-AREA.                                          
021200*      05  -COPY W4O66401     -PRE MOD-                                   
021300     EJECT                                                                
021400 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
021500     SKIP3                                                                
021600*01  -COPY WMFSAREA                                                       
021700     EJECT                                                                
021800 01  FILLER                      PIC X(16)   VALUE 'P-TO-P-AREA'.         
021900 01      P-TO-P-SW.                                                       
022000                                                                          
022100  02     P-TO-P-KVLL             PIC S9(4)           COMP SYNC.           
022200  02     P-TO-P-KDZ1             PIC X(1)  VALUE LOW-VALUE.               
022300  02     P-TO-P-KDZ2             PIC X(1)  VALUE LOW-VALUE.               
022400  02     P-TO-P-KDTRANS          PIC X(8).                                
022500  02     P-TO-P-IDTRANS          PIC X(4).                                
022600  02     P-TO-P-KDMFSFOR         PIC X(1).                                
022700  02     P-TO-P-DATA             PIC X(1000).                             
022800     EJECT                                                                
022900*    -COPY W4I67501   -PRE MOD4675-  -RED P-TO-P-DATA                     
023000     EJECT                                                                
023100*    -COPY W4I67701   -PRE MOD4677-  -RED P-TO-P-DATA                     
023200     EJECT                                                                
023300*    -COPY W4I69801   -PRE MOD4698-  -RED P-TO-P-DATA                     
023400     EJECT                                                                
023500*    -COPY W4I63901   -PRE MOD4639-  -RED P-TO-P-DATA                     
023600*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
023700*                                                                         
023800     EJECT                                                                
023900 01  SAVE-AREA.                                                           
024000     03  SAVE-IDTRANS            PIC X(4)    VALUE '4664'.                
024100     03  SAVE-IDSEGM             PIC X(4)    VALUE '4497'.                
024200     03  SAVE-PROFORMA.                                                   
024300       05 SAVE-IDDISTR           PIC 9(4).                                
024400       05 SAVE-IDKUNDNR          PIC 9(6).                                
024500       05 SAVE-KDFAKTYP          PIC X(01).                               
024600       05 SAVE-IDKUNDRF          PIC X(10).                               
024700       05 SAVE-IDPRODNR          PIC 9(07).                               
024800       05 SAVE-IDKOLLI           PIC 9(05).                               
024900       05 SAVE-RESTART-COPY      PIC X(01)   VALUE SPACE.                 
025000           88 RESTART-COPY                   VALUE 'Y'.                   
025100     03  SAVE-IDKOLLIS-ENTER     PIC S9(5)   VALUE +0 COMP-3.             
025200     03  SAVE-IDKOLLIS-NEXT      PIC S9(5)   VALUE +0 COMP-3.             
025300     03  SAVE-WDGXKEY-4498-ENTER.                                         
025400       05 SAVE-IDDISTR-ENTER     PIC S9(5)   COMP-3.                      
025500       05 SAVE-IDKUNDNR-ENTER    PIC S9(7)   COMP-3.                      
025600       05 SAVE-KDFAKTYP-ENTER    PIC X(01).                               
025700       05 SAVE-IDKUNDRF-ENTER    PIC X(10).                               
025800       05 SAVE-IDPRODNR-ENTER    PIC S9(7)   COMP-3.                      
025900       05 SAVE-IDKOLLI-ENTER     PIC S9(5)   COMP-3.                      
026000     03  SAVE-WDGXKEY-4498-NEXT.                                          
026100       05 SAVE-IDDISTR-NEXT      PIC S9(5)   COMP-3.                      
026200       05 SAVE-IDKUNDNR-NEXT     PIC S9(7)   COMP-3.                      
026300       05 SAVE-KDFAKTYP-NEXT     PIC X(01).                               
026400       05 SAVE-IDKUNDRF-NEXT     PIC X(10).                               
026500       05 SAVE-IDPRODNR-NEXT     PIC S9(7)   COMP-3.                      
026600       05 SAVE-IDKOLLI-NEXT      PIC S9(5)   COMP-3.                      
026700     EJECT                                                                
026800                                                                          
026900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
027000     SKIP3                                                                
027100 01  NYCKLAR-TILL-DLI.                                                    
027200     03  W-4495-X.                                                        
027300         05  W-IDHTR             PIC X(4)    VALUE '4495'.                
027400         05  W-IDDC-4495         PIC X(2)    VALUE SPACE.                 
027500         05  W-IDTRPTNR          PIC S9(3)   VALUE ZERO   COMP-3.         
027600         05  W-IDLBBET           PIC X(12)   VALUE SPACE.                 
027700         05  FILLER              PIC X(10)   VALUE LOW-VALUE.             
027800                                                                          
027900     03  W-IDKOLLIS-X.                                                    
028000         05  W-IDKOLLIS          PIC S9(5)   VALUE ZERO  COMP-3.          
028100                                                                          
028200     03  W-4498-X.                                                        
028300         05  W-IDDISTR           PIC S9(5)   VALUE ZERO  COMP-3.          
028400         05  W-IDKUNDNR          PIC S9(7)   VALUE ZERO  COMP-3.          
028500         05  W-KDFAKTYP          PIC  X(1)   VALUE SPACE.                 
028600         05  W-IDKUNDRF          PIC  X(10).                              
028700         05  W-IDKUNDRF-IDORDNR-FILLER REDEFINES W-IDKUNDRF.              
028800           07  W-IDORDNR7        PIC  9(7).                               
028900           07  FILLER            PIC  X(3).                               
029000         05  W-IDPRODNR          PIC S9(7)   VALUE ZERO  COMP-3.          
029100         05  W-IDKOLLI           PIC S9(5)   VALUE ZERO  COMP-3.          
029200                                                                          
029300     03  W-4479-X.                                                        
029400         05  W-IDHTYP-4479       PIC X(4)    VALUE '4479'.                
029500         05  W-IDDC-4479         PIC X(2)    VALUE SPACE.                 
029600         05  W-IDTRPTNR-4479     PIC S9(3)   VALUE ZERO   COMP-3.         
029700         05  W-IDLBBET-4479      PIC X(12)   VALUE SPACE.                 
029800         05  FILLER              PIC X(10)   VALUE LOW-VALUE.             
029900                                                                          
030000     03  W-IDDC-X.                                                        
030100         05  W-IDDC              PIC  X(2)   VALUE SPACE.                 
030200                                                                          
030300     03  W-IDPRODNR-X.                                                    
030400         05  W-IDPRODNR-KOLLI    PIC S9(7)   VALUE ZERO  COMP-3.          
030500                                                                          
030600     03  W-IDKOLLI-X.                                                     
030700         05  W-IDKOLLI-KOLLI     PIC S9(5)   VALUE ZERO  COMP-3.          
030800                                                                          
030900     03  W-WDE4ASEQ-X.                                                    
031000      05  W-4A1-IDDISTR          PIC S9(5)   VALUE ZERO  COMP-3.          
031100      05  W-4A1-IDKUNDNR         PIC S9(7)   VALUE ZERO  COMP-3.          
031200      05  W-4A1-IDKUNDRF.                                                 
031300       07  W-4A1-IDORDNR         PIC X(5).                                
031400       07  FILLER                PIC X(5).                                
031500                                                                          
031600     03  W-IDGMT-X.                                                       
031700      05  W-IDDISTR-WDB2         PIC S9(5)   VALUE ZERO  COMP-3.          
031800      05  W-IDKUNDNR-WDB2        PIC S9(7)   VALUE ZERO  COMP-3.          
031900                                                                          
032000     03  W-IDDC-B6-X.                                                     
032100         05 W-IDDC-B6            PIC X(2).                                
032200                                                                          
032300     03  W-WDB101KY-X.                                                    
032400         05  W-WDB1-IDPARTNR     PIC X(9)     VALUE SPACE.                
032500         05  W-WDB1-IDFTG        PIC 9(2)     VALUE ZERO.                 
032600                                                                          
032700     03 W-WDE7ASEQ-X.                                                     
032800         05 W-IDDC-E7            PIC  X(2)    VALUE SPACE.                
032900         05 W-IDKOLLIS-E7        PIC S9(5)    VALUE ZERO  COMP-3.         
033000                                                                          
033100*    --- STATUS-KOD FRÅN IMS                                              
033200 01  STATUS-WS                   PIC XX.                                  
033300     88  SEGMENT-FINNS                       VALUE '  '.                  
033400     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
033500     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
033600     88  BASEN-SLUT                          VALUE 'GB'.                  
033700     SKIP2                                                                
033800 01  GODK-STATUSKODER.                                                    
033900     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
034000     SKIP3                                                                
034100 01  SSA1                        PIC X(128).                              
034200 01  SSA2                        PIC X(128).                              
034300 01  SSA3                        PIC X(64).                               
034400     EJECT                                                                
034500*    --- IMS FUNKTIONSKODER                                               
034600*01  -COPY W0003                                                          
034700     EJECT                                                                
034800*    ---  DLI INPUT-OUTPUT AREA                                           
034900 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
035000     SKIP3                                                                
035100 01  FILLER         PIC X(16) VALUE 'DLI-IO-E401'.                        
035200 01  DLI-IO-E401.                                                         
035300*    03  -COPY WDE401                                                     
035400     EJECT                                                                
035500 01  FILLER         PIC X(16) VALUE 'DLI-IO-E601'.                        
035600 01  DLI-IO-E601.                                                         
035700*    03  -COPY WDE601                                                     
035800     EJECT                                                                
035900 01  FILLER         PIC X(16) VALUE 'DLI-IO-E611'.                        
036000 01  DLI-IO-E611.                                                         
036100*    03  -COPY WDE611                                                     
036200     EJECT                                                                
036300 01  FILLER         PIC X(16) VALUE 'DLI-IO-4495'.                        
036400 01  DLI-IO-4495.                                                         
036500*    03  -COPY WDGX4495                                                   
036600     EJECT                                                                
036700 01  FILLER         PIC X(16) VALUE 'DLI-IO-4496'.                        
036800 01  DLI-IO-4496.                                                         
036900*    03  -COPY WDGX4496                                                   
037000     EJECT                                                                
037100 01  FILLER         PIC X(16) VALUE 'DLI-IO-4497'.                        
037200 01  DLI-IO-4497.                                                         
037300*    03  -COPY WDGX4497                                                   
037400     EJECT                                                                
037500 01  FILLER         PIC X(16) VALUE 'DLI-IO-4498'.                        
037600 01  DLI-IO-4498.                                                         
037700*    03  -COPY WDGX4498                                                   
037800     EJECT                                                                
037900 01  FILLER         PIC X(16) VALUE 'DLI-IO-4479'.                        
038000 01  DLI-IO-4479.                                                         
038100*    03  -COPY WDGX4479                                                   
038200     EJECT                                                                
038300 01  FILLER         PIC X(16) VALUE 'DLI-IO-4480'.                        
038400 01  DLI-IO-4480.                                                         
038500*    03  -COPY WDGX4480                                                   
038600     EJECT                                                                
038700 01  FILLER         PIC X(16) VALUE 'DLI-IO-4482'.                        
038800 01  DLI-IO-4482.                                                         
038900*    03  -COPY WDGX4482                                                   
039000     EJECT                                                                
039100 01  FILLER         PIC X(16) VALUE 'DLI-IO-B201'.                        
039200 01  DLI-IO-B201.                                                         
039300*    03  -COPY WDB201                                                     
039400     EJECT                                                                
039500                                                                          
039600 01  FILLER         PIC X(16) VALUE 'WDB601  AREA'.                       
039700 01  DLI-IO-AREA-B601.                                                    
039800*    03  -COPY WDB601                                                     
039900     EJECT                                                                
040000                                                                          
040100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB101'.                      
040200 01  DLI-IO-WDB101.                                                       
040300*    03  -COPY WDB101                                                     
040400                                                                          
040500 01  DLI-IO-E711.                                                         
040600*    03  -COPY WDE711                                                     
040700     EJECT                                                                
040800                                                                          
040900 LINKAGE SECTION.                                                         
041000                                                                          
041100*01  -COPY W0009   -PRE MSG-                                              
041200     EJECT                                                                
041300*01  -COPY W0009  -PRE ALT-                                               
041400     EJECT                                                                
041500*01  -COPY W0009  -PRE ALT2-                                              
041600     EJECT                                                                
041700*01  -COPY W0009  -PRE ALT3-                                              
041800     EJECT                                                                
041900*01  -COPY W0009  -PRE ALT4-                                              
042000     EJECT                                                                
042100*01  -COPY W0009  -PRE ALT5-                                              
042200     EJECT                                                                
042300*01  -COPY W0009  -PRE ALT6-                                              
042400     EJECT                                                                
042500*01  -COPY W0009  -PRE ALT7-                                              
042600     EJECT                                                                
042700*01  -COPY W0009  -PRE ALT8-                                              
042800     EJECT                                                                
042900*01  -COPY W0008  -PRE USEA-                                              
043000     05  FILLER                  PIC X.                                   
043100     EJECT                                                                
043200*01  -COPY W0008  -PRE WDE6-                                              
043300     05  FILLER                  PIC X.                                   
043400     EJECT                                                                
043500*01  -COPY W0008  -PRE WDE4-                                              
043600     05  FILLER                  PIC X.                                   
043700     EJECT                                                                
043800*01  -COPY W0008  -PRE 4495-                                              
043900     05  FILLER                  PIC X.                                   
044000     EJECT                                                                
044100*01  -COPY W0008  -PRE WDB2-                                              
044200     05  FILLER                  PIC X.                                   
044300*01  -COPY W0008  -PRE 4479-                                              
044400     05  FILLER                  PIC X.                                   
044500     EJECT                                                                
044600*01  -COPY W0008  -PRE WDB6-                                              
044700     05  FILLER                  PIC X.                                   
044800     EJECT                                                                
044900*01  -COPY W0008  -PRE WDB1-                                              
045000     05  FILLER                  PIC X.                                   
045100     EJECT                                                                
045200*01  -COPY W0008  -PRE WDG2-                                              
045300     05  FILLER                  PIC X.                                   
045400     EJECT                                                                
045500*01  -COPY W0008  -PRE WDE7-                                              
045600     05  FILLER                  PIC X.                                   
045700     EJECT                                                                
045800 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB  ALT3-PCB                      
045900                          ALT4-PCB ALT5-PCB ALT6-PCB ALT7-PCB             
046000                          ALT8-PCB                                        
046100                          USEA-PCB WDE6-PCB WDE4-PCB                      
046200                          4495-PCB WDB2-PCB 4479-PCB WDB6-PCB             
046300                          WDB1-PCB WDG2-PCB WDE7-PCB.                     
046400     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB  ALT3-PCB                      
046500                          ALT4-PCB ALT5-PCB ALT6-PCB ALT7-PCB             
046600                          ALT8-PCB                                        
046700                          USEA-PCB WDE6-PCB WDE4-PCB                      
046800                          4495-PCB WDB2-PCB 4479-PCB WDB6-PCB             
046900                          WDB1-PCB WDG2-PCB WDE7-PCB.                     
047000                                                                          
047100     PERFORM IMS-GET-MSG                                                  
047200     IF SEGMENT-FINNS                                                     
047300       PERFORM A-INIT                                                     
047400       PERFORM B-KOLLA-NYCKLAR                                            
047500       IF NYCKLAR-OK                                                      
047600         IF MFS-UPDATE                                                    
047700           IF NOT OMSTART AND NOT RESTART-COPY                            
047800              PERFORM G-KOLLA-INPUT                                       
047900           END-IF                                                         
048000           IF INDATA-OK                                                   
048100             PERFORM H-UPPDATERA                                          
048200           END-IF                                                         
048300         ELSE                                                             
048400           IF MFS-FIRST                                                   
048500             PERFORM C-FOERSTA-SIDA                                       
048600           ELSE                                                           
048700             IF MFS-NEXT                                                  
048800               PERFORM D-NAESTA-SIDA                                      
048900             ELSE                                                         
049000               PERFORM E-SAMMA-SIDA                                       
049100             END-IF                                                       
049200           END-IF                                                         
049300         END-IF                                                           
049400         IF INDATA-OK AND NOT OMSTART AND NOT RESTART-COPY                
049500           PERFORM F-LAES-VISA-INFO                                       
049600         END-IF                                                           
049700       END-IF                                                             
049800       IF (OMSTART OR RESTART-COPY)                                       
049900          PERFORM I-STARTA-W40664                                         
050000       ELSE                                                               
050100          IF (AVSLUTA OR PROFORMA OR LASTDOK) AND INDATA-OK               
050200            CONTINUE                                                      
050300*           BILD LÄGGS UT AV W40675 FOR AVSLUTA                           
050400          ELSE                                                            
050500            IF W-IDTRANS = '4675'                                         
050600                MOVE INF-UPDATE-DONE TO MED-IDMFSINF                      
050700                CALL WMEDKONV USING MED-WMEDAREA                          
050800                MOVE MED-MFSINF      TO MOD-TEMFSINF                      
050900            END-IF                                                        
051000            IF W-IDTRANS = '4677'                                         
051100                MOVE INF-PROFORMA-REL TO MED-IDMFSINF                     
051200                CALL WMEDKONV USING MED-WMEDAREA                          
051300                MOVE MED-MFSINF      TO MOD-TEMFSINF                      
051400            END-IF                                                        
051500            IF W-IDTRANS = '4639'                                         
051600*               MOVE INF-TRPDOK-REL TO MED-IDMFSINF                       
051700*               CALL WMEDKONV USING MED-WMEDAREA                          
051800                MOVE 'LAST-INFO    ' TO MED-MFSINF                        
051900                MOVE MED-MFSINF      TO MOD-TEMFSINF                      
052000            END-IF                                                        
052100            COMPUTE MSG-KVLL         = LENGTH OF MOD-W4O66401 + 4         
052200            IF NOT LASTDOK                                                
052300            PERFORM IMS-INSERT-MSG                                        
052400            END-IF                                                        
052500          END-IF                                                          
052600       END-IF                                                             
052700                                                                          
052800       IF UPDATE-SAVE                                                     
052900         MOVE '002'       TO MSGI-KDCALL                                  
053000         MOVE '4664'      TO SAVE-IDTRANS                                 
053100         MOVE SAVE-AREA   TO MSGI-SPAR-AREA                               
053200         CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                       
053300       END-IF                                                             
053400                                                                          
053500     END-IF                                                               
053600                                                                          
053700     MOVE ZERO TO RETURN-CODE                                             
053800     GOBACK                                                               
053900     .                                                                    
054000     EJECT                                                                
054100 A-INIT SECTION.                                                          
054200                                                                          
054300     IF MSG-DUBBLA-TRANSKODER                                             
054400       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I66401                 
054500       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
054600       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
054700     ELSE                                                                 
054800       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I66401                  
054900       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
055000       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
055100     END-IF                                                               
055200                                                                          
055300     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
055400     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
055500     MOVE MFS-IDTRANS TO W-IDTRANS                                        
055600                                                                          
055700     MOVE LOW-VALUE TO MSG-AREA                                           
055800     MOVE 'W4O664N1' TO MFS-IDMOD                                         
055900     MOVE '4664' TO MOD-IDTRANS                                           
056000     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
056100                                                                          
056200     IF EGEN-MID OR HELP-MID                                              
056300       CONTINUE                                                           
056400     ELSE                                                                 
056500       MOVE SPACE TO MFS-KDTRTYP                                          
056600       MOVE '7' TO MFS-IDPFK                                              
056700     END-IF                                                               
056800     MOVE FUNCTION CURRENT-DATE(3:2) TO W-DATE-AAMM(1:2)                  
056900     MOVE FUNCTION CURRENT-DATE(5:2) TO W-DATE-AAMM(3:2)                  
057000                                                                          
057100     .                                                                    
057200     EJECT                                                                
057300 B-KOLLA-NYCKLAR SECTION.                                                 
057400                                                                          
057500     MOVE ALL '+'           TO MSGI-WMSGINIT                              
057600     MOVE '001'             TO MSGI-KDCALL                                
057700     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
057800     MOVE '4664'            TO MSGI-IDTRANS                               
057900     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
058000     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
058100     MOVE MSGI-IDLAND-SPR   TO MED-IDSKYLT                                
058200     MOVE MSGI-IDTIDZON     TO WS-IDTIDZON                                
058300     MOVE MSGI-KDMATT       TO WS-KDMATT                                  
058400                                                                          
058500     MOVE MSGI-SPAR-AREA    TO SAVE-AREA                                  
058600                                                                          
058700     MOVE JA TO NYCKLAR-SW                                                
058800                                                                          
058900*    -- KONTROLL AV IDTRPTNR                                              
059000     MOVE MFS-RENSA-FAELT TO MOD-IDTRPTNR-IN                              
059100                             MOD-IDLBBET-IN                               
059200                             MOD-FLFARLIG-IN                              
059300                             MOD-IDDC-IN                                  
059400                                                                          
059500     IF MID-IDTRPTNR-IN     =  ALL '+'                                    
059600       MOVE MID-IDTRPTNR-UT TO WS-IDTRPTNR                                
059700       INSPECT WS-IDTRPTNR REPLACING LEADING SPACE BY ZERO                
059800     ELSE                                                                 
059900       MOVE MID-IDTRPTNR-IN TO WS-IDTRPTNR                                
060000       MOVE '7'             TO MFS-IDPFK                                  
060100       MOVE SPACE           TO MFS-KDTRTYP                                
060200     END-IF                                                               
060300                                                                          
060400     IF WS-IDTRPTNR NUMERIC AND WS-IDTRPTNR > ZERO                        
060500       MOVE WS-IDTRPTNR     TO W-IDTRPTNR                                 
060600     ELSE                                                                 
060700       MOVE NEJ             TO NYCKLAR-SW                                 
060800     END-IF                                                               
060900                                                                          
061000     IF MID-IDLBBET-IN       = ALL '+'                                    
061100       MOVE MID-IDLBBET-UT   TO WS-IDLBBET                                
061200     ELSE                                                                 
061300       MOVE MID-IDLBBET-IN   TO WS-IDLBBET                                
061400       MOVE '7'              TO MFS-IDPFK                                 
061500       MOVE SPACE            TO MFS-KDTRTYP                               
061600     END-IF                                                               
061700     IF WS-IDLBBET           NOT = SPACE                                  
061800       MOVE WS-IDLBBET       TO W-IDLBBET                                 
061900     ELSE                                                                 
062000       MOVE NEJ              TO NYCKLAR-SW                                
062100     END-IF                                                               
062200                                                                          
062300     MOVE MSGI-IDDC         TO WS-IDDC                                    
062400     MOVE WS-IDDC           TO W-IDDC-B6                                  
062500     PERFORM IMS-GU-WDB601                                                
062600     IF DCS-CDC                                                           
062700        IF MID-IDDC-IN NOT = ALL '+'                                      
062800          MOVE MID-IDDC-IN      TO WS-IDDC                                
062900          IF WS-IDDC NOT = W-IDDC-B6                                      
063000             MOVE WS-IDDC TO W-IDDC-B6                                    
063100             PERFORM IMS-GU-WDB601                                        
063200          END-IF                                                          
063300          IF (DCS-DDC AND DCS-IDLANDX2 = 'SE') OR                         
063400              DCS-CDC                                                     
063500             MOVE MID-IDDC-IN   TO WS-IDDC-IN                             
063600             MOVE '7'              TO MFS-IDPFK                           
063700             MOVE SPACE            TO MFS-KDTRTYP                         
063800          ELSE                                                            
063900             MOVE MSGI-IDDC     TO WS-IDDC-IN                             
064000          END-IF                                                          
064100        ELSE                                                              
064200          MOVE MID-IDDC-UT      TO WS-IDDC                                
064300          IF WS-IDDC NOT = W-IDDC-B6                                      
064400             MOVE WS-IDDC TO W-IDDC-B6                                    
064500             PERFORM IMS-GU-WDB601                                        
064600          END-IF                                                          
064700          IF DCS-DDC AND DCS-IDLANDX2 = 'SE'                              
064800             MOVE MID-IDDC-UT   TO WS-IDDC-IN                             
064900          ELSE                                                            
065000             MOVE MSGI-IDDC     TO WS-IDDC-IN                             
065100          END-IF                                                          
065200        END-IF                                                            
065300     ELSE                                                                 
065400        MOVE MSGI-IDDC          TO WS-IDDC-IN                             
065500     END-IF                                                               
065600                                                                          
065700     MOVE WS-IDDC-IN        TO WS-IDDC                                    
065800     IF WS-IDDC-IN NOT = W-IDDC-B6                                        
065900        MOVE WS-IDDC-IN TO W-IDDC-B6                                      
066000        PERFORM IMS-GU-WDB601                                             
066100     END-IF                                                               
066200     IF DCS-KDDC = SPACE                                                  
066300        MOVE NEJ              TO NYCKLAR-SW                               
066400     ELSE                                                                 
066500        MOVE WS-IDDC-IN     TO W-IDDC                                     
066600                               W-IDDC-4495                                
066700                               W-IDDC-E7                                  
066800                               MOD-IDDC-UT                                
066900     END-IF                                                               
067000                                                                          
067100                                                                          
067200     IF MID-FLFARLIG-IN      = ALL '+'                                    
067300       IF DCS-NDC-PF                                                      
067400         MOVE 'M'            TO WS-FLFARLIG                               
067500       ELSE                                                               
067600         MOVE MID-FLFARLIG-UT TO WS-FLFARLIG                              
067700       END-IF                                                             
067800     ELSE                                                                 
067900       MOVE MID-FLFARLIG-IN  TO WS-FLFARLIG                               
068000       MOVE '7'              TO MFS-IDPFK                                 
068100       MOVE SPACE            TO MFS-KDTRTYP                               
068200     END-IF                                                               
068300     IF WS-FLFARLIG          = SPACE                                      
068400       MOVE NEJ              TO NYCKLAR-SW                                
068500     END-IF                                                               
068600     IF MID-IDDISTR-DOLD NOT NUMERIC                                      
068700       MOVE ZERO             TO MID-IDDISTR-DOLD                          
068800                                W-IDDISTR-ALFA                            
068900     ELSE                                                                 
069000       MOVE MID-IDDISTR-DOLD TO TEST-IDDISTR                              
069100                                W-IDDISTR-ALFA                            
069200     END-IF                                                               
069300                                                                          
069400     IF GODK-MID OR NYCKLAR-OK                                            
069500       MOVE WS-IDLBBET       TO MOD-IDLBBET-UT                            
069600       MOVE WS-FLFARLIG      TO MOD-FLFARLIG-UT                           
069700       MOVE WS-IDTRPTNR      TO MOD-IDTRPTNR-UT                           
069800       INSPECT MOD-IDTRPTNR-UT REPLACING LEADING ZERO BY SPACE            
069900       INSPECT MOD-IDDC-UT   REPLACING LEADING ZERO BY SPACE              
070000     ELSE                                                                 
070100       MOVE MFS-RENSA-FAELT  TO MOD-IDTRPTNR-UT                           
070200                                MOD-FLFARLIG-UT                           
070300                                MOD-IDTRPTNR-UT                           
070400     END-IF                                                               
070500     IF MFS-FIRST                                                         
070600       MOVE SPACE TO SAVE-RESTART-COPY                                    
070700       MOVE JA    TO UPDATE-SAVE-SW                                       
070800     END-IF                                                               
070900                                                                          
071000     IF NYCKLAR-FEL                                                       
071100       MOVE ERR-WRONG-KEY    TO MED-IDMFSFEL                              
071200       CALL WMEDKONV USING MED-WMEDAREA                                   
071300       MOVE MED-MFSFEL       TO MOD-TEMFSFEL                              
071400       PERFORM MFS-RENSA-FAELT-IN                                         
071500       PERFORM MFS-RENSA-FAELT-UT                                         
071600     END-IF                                                               
071700     .                                                                    
071800     EJECT                                                                
071900 C-FOERSTA-SIDA SECTION.                                                  
072000                                                                          
072100     MOVE INF-FIRST-PAGE     TO MED-IDMFSINF                              
072200     CALL WMEDKONV USING MED-WMEDAREA                                     
072300     MOVE MED-MFSINF         TO MOD-TEMFSFEL                              
072400     MOVE '4497'             TO SAVE-IDSEGM                               
072500*                                                                         
072600     PERFORM MFS-RENSA-FAELT-IN                                           
072700     .                                                                    
072800     EJECT                                                                
072900 D-NAESTA-SIDA SECTION.                                                   
073000                                                                          
073100     IF SAVE-IDTRANS = '4664'                                             
073200                                                                          
073300       IF SAVE-IDKOLLIS-NEXT > ZERO                                       
073400         MOVE SAVE-IDKOLLIS-NEXT     TO W-IDKOLLIS                        
073500         MOVE '4497'                 TO SAVE-IDSEGM                       
073600       ELSE                                                               
073700         MOVE ZERO                   TO W-IDKOLLIS                        
073800         MOVE SAVE-WDGXKEY-4498-NEXT TO W-4498-X                          
073900       END-IF                                                             
074000     END-IF                                                               
074100     PERFORM MFS-RENSA-FAELT-IN                                           
074200     .                                                                    
074300     EJECT                                                                
074400 E-SAMMA-SIDA SECTION.                                                    
074500                                                                          
074600     MOVE SPACE                       TO W-IDKUNDRF                       
074700     IF EGEN-MID OR HELP-MID                                              
074800                                                                          
074900        IF SAVE-IDTRANS = '4664'                                          
075000                                                                          
075100          IF SAVE-IDKOLLIS-ENTER > ZERO                                   
075200            MOVE SAVE-IDKOLLIS-ENTER  TO W-IDKOLLIS                       
075300            MOVE '4497'               TO  SAVE-IDSEGM                     
075400            MOVE ZERO                 TO W-4498-X                         
075500          ELSE                                                            
075600            MOVE ZERO                 TO W-IDKOLLIS                       
075700            MOVE SAVE-WDGXKEY-4498-ENTER TO W-4498-X                      
075800            MOVE '4498'               TO  SAVE-IDSEGM                     
075900          END-IF                                                          
076000        END-IF                                                            
076100                                                                          
076200        MOVE NEJ                      TO SW-RAD-INPUT                     
076300        MOVE +1                       TO INDX                             
076400        PERFORM UNTIL (INDX > MAX-INDX)                                   
076500                                                                          
076600          IF MID-FLBACKA-RAD (INDX)    = ALL '+'                          
076700             CONTINUE                                                     
076800          ELSE                                                            
076900             MOVE JA                  TO SW-RAD-INPUT                     
077000          END-IF                                                          
077100                                                                          
077200         ADD +1                       TO INDX                             
077300        END-PERFORM                                                       
077400                                                                          
077500        IF MID-FLAVSLUTA = ALL '+' AND SW-RAD-INPUT  = NEJ                
077600          PERFORM MFS-RENSA-FAELT-IN                                      
077700        ELSE                                                              
077800          MOVE INF-PRESS-PF11 TO MED-IDMFSINF                             
077900          CALL WMEDKONV USING MED-WMEDAREA                                
078000          MOVE MED-MFSINF TO MOD-TEMFSFEL                                 
078100          PERFORM EA-MID-INDATA-TILL-MOD                                  
078200        END-IF                                                            
078300     ELSE                                                                 
078400        PERFORM MFS-RENSA-FAELT-IN                                        
078500     END-IF                                                               
078600     .                                                                    
078700     EJECT                                                                
078800 EA-MID-INDATA-TILL-MOD SECTION.                                          
078900                                                                          
079000     IF  MID-FLAVSLUTA            =  ALL '+'                              
079100       MOVE MFS-RENSA-FAELT       TO MOD-FLAVSLUTA                        
079200     ELSE                                                                 
079300       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLAVSLUTA-ATTR                   
079400       MOVE MID-FLAVSLUTA         TO MOD-FLAVSLUTA                        
079500     END-IF                                                               
079600                                                                          
079700     MOVE MID-IDDISTR-DOLD        TO MOD-IDDISTR-DOLD                     
079800                                                                          
079900     IF  MID-FLLSTDOK             =  ALL '+'                              
080000       MOVE MFS-RENSA-FAELT       TO MOD-FLLSTDOK                         
080100     ELSE                                                                 
080200       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLLSTDOK-ATTR                    
080300       MOVE MID-FLLSTDOK          TO MOD-FLLSTDOK                         
080400     END-IF                                                               
080500                                                                          
080600     IF  MID-FLTRPDOK             =  ALL '+'                              
080700       MOVE MFS-RENSA-FAELT       TO MOD-FLTRPDOK                         
080800     ELSE                                                                 
080900       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLTRPDOK-ATTR                    
081000       MOVE MID-FLTRPDOK          TO MOD-FLTRPDOK                         
081100     END-IF                                                               
081200                                                                          
081300     IF  MID-FLPROFORMA           =  ALL '+'                              
081400       MOVE MFS-RENSA-FAELT       TO MOD-FLPROFORMA                       
081500     ELSE                                                                 
081600       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLPROFORMA-ATTR                  
081700       MOVE MID-FLPROFORMA        TO MOD-FLPROFORMA                       
081800     END-IF                                                               
081900                                                                          
082000     MOVE +1                      TO INDX                                 
082100                                                                          
082200     PERFORM UNTIL (INDX > MAX-INDX)                                      
082300                                                                          
082400       IF  MID-FLBACKA-RAD (INDX)    = ALL '+'                            
082500         MOVE MFS-RENSA-FAELT     TO MOD-FLBACKA-RAD (INDX)               
082600       ELSE                                                               
082700         MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-FLBACKA-RAD-ATTR(INDX)        
082800         MOVE MID-FLBACKA-RAD (INDX) TO MOD-FLBACKA-RAD    (INDX)         
082900       END-IF                                                             
083000                                                                          
083100       ADD +1                     TO INDX                                 
083200     END-PERFORM                                                          
083300                                                                          
083400     .                                                                    
083500     EJECT                                                                
083600 F-LAES-VISA-INFO SECTION.                                                
083700                                                                          
083800     IF WS-IDDC NOT = W-IDDC-B6                                           
083900        MOVE WS-IDDC TO W-IDDC-B6                                         
084000        PERFORM IMS-GU-WDB601                                             
084100     END-IF                                                               
084200     PERFORM FA-LAES-LASTAT-HITTILLS                                      
084300                                                                          
084400     IF SEGMENT-FINNS                                                     
084500       MOVE +1                    TO INDX                                 
084600       IF SAVE-IDSEGM = '4497'                                            
084700         IF MFS-FIRST                                                     
084800           PERFORM IMS-GNP-WDGX4497                                       
084900         ELSE                                                             
085000           IF MFS-NEXT                                                    
085100             IF SAVE-IDKOLLIS-NEXT > ZERO                                 
085200               MOVE SAVE-IDKOLLIS-NEXT TO W-IDKOLLIS                      
085300               PERFORM IMS-GNP-WDGX4497-KVAL                              
085400             ELSE                                                         
085500               PERFORM IMS-GNP-WDGX4497                                   
085600             END-IF                                                       
085700           ELSE                                                           
085800             MOVE SAVE-IDKOLLIS-ENTER TO W-IDKOLLIS                       
085900             PERFORM IMS-GNP-WDGX4497-KVAL                                
086000           END-IF                                                         
086100         END-IF                                                           
086200         PERFORM UNTIL SEGMENT-SAKNAS OR                                  
086300                       INDX > MAX-INDX                                    
086400           IF INDX = +1                                                   
086500             MOVE 4497-IDKOLLI-SAMP  TO SAVE-IDKOLLIS-ENTER               
086600           END-IF                                                         
086700           PERFORM FB-FLYTTA-MOD-KOLLI-SAMP                               
086800                                                                          
086900           PERFORM IMS-GNP-WDGX4497                                       
087000           ADD 1 TO INDX                                                  
087100         END-PERFORM                                                      
087200                                                                          
087300         IF SEGMENT-FINNS                                                 
087400           MOVE 4497-IDKOLLI-SAMP  TO SAVE-IDKOLLIS-NEXT                  
087500           MOVE ZERO               TO SAVE-WDGXKEY-4498-NEXT              
087600         ELSE                                                             
087700           MOVE ZERO               TO SAVE-IDKOLLIS-NEXT                  
087800           MOVE '4498'             TO SAVE-IDSEGM                         
087900         END-IF                                                           
088000                                                                          
088100         IF INDX <= MAX-INDX                                              
088200           MOVE INDX TO START-INDX                                        
088300*          START-INDX = DET INDEX SOM 4498-SEG BÖRJAR LÄGGAS              
088400*                       UT I MODEN OM DET FINNS SK FÖRE                   
088500         END-IF                                                           
088600       END-IF                                                             
088700                                                                          
088800       IF SAVE-IDSEGM = '4498'                                            
088900         IF MFS-FIRST                                                     
089000           PERFORM IMS-GHNP-WDGX4498                                      
089100         ELSE                                                             
089200           IF MFS-NEXT                                                    
089300             IF SAVE-IDKOLLIS-NEXT = ZERO                                 
089400               IF SAVE-WDGXKEY-4498-NEXT = ZERO                           
089500                 PERFORM IMS-GHNP-WDGX4498                                
089600               ELSE                                                       
089700                 MOVE SAVE-WDGXKEY-4498-NEXT TO W-4498-X                  
089800                 PERFORM IMS-GHNP-WDGX4498-KVAL                           
089900               END-IF                                                     
090000             ELSE                                                         
090100               MOVE SAVE-WDGXKEY-4498-NEXT TO W-4498-X                    
090200               PERFORM IMS-GHNP-WDGX4498-KVAL                             
090300             END-IF                                                       
090400           ELSE                                                           
090500             MOVE SAVE-WDGXKEY-4498-ENTER TO W-4498-X                     
090600             PERFORM IMS-GHNP-WDGX4498-KVAL                               
090700           END-IF                                                         
090800         END-IF                                                           
090900         MOVE 4498-IDDISTR TO MOD-IDDISTR-DOLD                            
091000         PERFORM UNTIL SEGMENT-SAKNAS OR                                  
091100                       INDX > MAX-INDX                                    
091200           IF 4498-IDKOLLI-SAMP = ZERO                                    
091300             IF INDX = +1 OR                                              
091400                START-INDX > +1                                           
091500               MOVE 4498-WDGX4498(1:25)                                   
091600                                    TO SAVE-WDGXKEY-4498-ENTER            
091700               MOVE ZERO            TO START-INDX                         
091800               IF INDX = +1                                               
091900                 MOVE ZERO          TO SAVE-IDKOLLIS-ENTER                
092000*                DET BETYDER ATT INGA SK FINNS ATT VISAS PÅ SIDAN         
092100               END-IF                                                     
092200             END-IF                                                       
092300             PERFORM FC-FLYTTA-MOD-KOLLI                                  
092400             ADD +1                 TO INDX                               
092500           END-IF                                                         
092600                                                                          
092700           PERFORM IMS-GHNP-WDGX4498                                      
092800         END-PERFORM                                                      
092900         IF SEGMENT-FINNS                                                 
093000           MOVE 4498-WDGX4498(1:25) TO SAVE-WDGXKEY-4498-NEXT             
093100         ELSE                                                             
093200           IF SAVE-IDKOLLIS-ENTER NUMERIC AND                             
093300              SAVE-IDKOLLIS-ENTER > ZERO                                  
093400             MOVE ZERO              TO SAVE-WDGXKEY-4498-NEXT             
093500             MOVE SAVE-IDKOLLIS-ENTER                                     
093600                                    TO SAVE-IDKOLLIS-NEXT                 
093700           ELSE                                                           
093800             MOVE SAVE-WDGXKEY-4498-ENTER                                 
093900                                    TO SAVE-WDGXKEY-4498-NEXT             
094000           END-IF                                                         
094100           PERFORM UNTIL INDX > MAX-INDX                                  
094200             MOVE MFS-STAENG-FAELT  TO MOD-FLBACKA-RAD-ATTR(INDX)         
094300             MOVE MFS-RENSA-FAELT   TO MOD-IDDISTR  (INDX)                
094400                                       MOD-IDKUNDNR (INDX)                
094500                                       MOD-KDFAKTYP (INDX)                
094600                                       MOD-IDORDNR7 (INDX)                
094700                                       MOD-IDKOLLI (INDX)                 
094800                                       MOD-TIRFS (INDX)                   
094900                                       MOD-KDKOLLI (INDX)                 
095000                                       MOD-VKORDBTO (INDX)                
095100                                       MOD-VLORDBTO (INDX)                
095200                                       MOD-IDPSN (INDX)                   
095300                                       MOD-IDPRODNR (INDX)                
095400             ADD 1 TO INDX                                                
095500           END-PERFORM                                                    
095600         END-IF                                                           
095700       END-IF                                                             
095800                                                                          
095900       IF SEGMENT-FINNS                                                   
096000         MOVE INF-MORE-INFO-EXISTS  TO MED-IDMFSINF                       
096100         CALL WMEDKONV USING MED-WMEDAREA                                 
096200         MOVE MED-TEMFSINF          TO MOD-TEMFSINF                       
096300       END-IF                                                             
096400       MOVE '002'                   TO MSGI-KDCALL                        
096500       MOVE '4664'                  TO SAVE-IDTRANS                       
096600       MOVE SAVE-AREA               TO MSGI-SPAR-AREA                     
096700       CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                         
096800     ELSE                                                                 
096900                                                                          
097000       MOVE MFS-STAENG-FAELT     TO MOD-FLAVSLUTA-ATTR                    
097100       MOVE ZERO TO SAVE-WDGXKEY-4498-ENTER                               
097200       MOVE ZERO TO SAVE-WDGXKEY-4498-NEXT                                
097300       MOVE ZERO TO SAVE-IDKOLLIS-ENTER                                   
097400       MOVE ZERO TO SAVE-IDKOLLIS-NEXT                                    
097500                    MOD-IDDISTR-DOLD                                      
097600                                                                          
097700       PERFORM UNTIL INDX > MAX-INDX                                      
097800         MOVE +1                 TO INDX                                  
097900         MOVE MFS-RENSA-FAELT    TO MOD-IDDISTR (INDX)                    
098000                                    MOD-IDKUNDNR (INDX)                   
098100                                    MOD-KDFAKTYP (INDX)                   
098200                                    MOD-IDORDNR7 (INDX)                   
098300                                    MOD-IDKOLLI (INDX)                    
098400                                    MOD-TIRFS (INDX)                      
098500                                    MOD-KDKOLLI (INDX)                    
098600                                    MOD-VKORDBTO (INDX)                   
098700                                    MOD-VLORDBTO (INDX)                   
098800                                    MOD-IDPSN (INDX)                      
098900                                    MOD-IDPRODNR (INDX)                   
099000         ADD 1 TO INDX                                                    
099100       END-PERFORM                                                        
099200     END-IF                                                               
099300                                                                          
099400     PERFORM FD-VISA-FLAGGA-DDI                                           
099500                                                                          
099600     MOVE SPACE                  TO SAVE-RESTART-COPY                     
099700     MOVE JA                     TO UPDATE-SAVE-SW                        
099800     .                                                                    
099900     EJECT                                                                
100000 FA-LAES-LASTAT-HITTILLS SECTION.                                         
100100                                                                          
100200     PERFORM IMS-GHU-WDGX4496                                             
100300     IF SEGMENT-FINNS                                                     
100400       MOVE 4496-KVKOLLI-LAST      TO MOD-KVKOLLI-TOT                     
100500       MOVE 4496-SUORDV-LASTB      TO MOD-SUORDV-TOT                      
100600                                                                          
100700       IF WS-KDMATT = 'U'                                                 
100800         COMPUTE WS-VKORDBTO          =                                   
100900                 4496-VKORDBTO-LASTB  *                                   
101000                 CONV-KG-TO-LB                                            
101100         COMPUTE WS-VLORDBTO          =                                   
101200                 4496-VLORDBTO-LASTB  *                                   
101300                 CONV-M3-TO-YD3                                           
101400         MOVE WS-VKORDBTO          TO MOD-VKORDBTO-TOT                    
101500         MOVE WS-VLORDBTO          TO MOD-VLORDBTO-TOT                    
101600                                                                          
101700       ELSE                                                               
101800         MOVE 4496-VLORDBTO-LASTB  TO MOD-VLORDBTO-TOT                    
101900         MOVE 4496-VKORDBTO-LASTB  TO MOD-VKORDBTO-TOT                    
102000       END-IF                                                             
102100                                                                          
102200     ELSE                                                                 
102300       MOVE ZERO                   TO MOD-KVKOLLI-TOT                     
102400                                      MOD-SUORDV-TOT                      
102500                                      MOD-VLORDBTO-TOT                    
102600                                      MOD-VKORDBTO-TOT                    
102700     END-IF                                                               
102800     .                                                                    
102900     EJECT                                                                
103000 FB-FLYTTA-MOD-KOLLI-SAMP SECTION.                                        
103100                                                                          
103200     MOVE 4497-IDKOLLI-SAMP        TO MOD-IDKOLLI (INDX)                  
103300     MOVE 4497-KDKOLLI-SAMP        TO MOD-KDKOLLI (INDX)                  
103400     IF WS-KDMATT = 'U'                                                   
103500       COMPUTE WS-VKORDBTO         =                                      
103600               4497-VKKOLLIB-SAMP  *                                      
103700               CONV-KG-TO-LB                                              
103800       COMPUTE WS-VLORDBTO         =                                      
103900               4497-VLKOLLIB-SAMP  *                                      
104000               CONV-M3-TO-YD3                                             
104100       MOVE WS-VKORDBTO            TO MOD-VKORDBTO (INDX)                 
104200       MOVE WS-VLORDBTO            TO MOD-VLORDBTO (INDX)                 
104300     ELSE                                                                 
104400       MOVE 4497-VKKOLLIB-SAMP     TO MOD-VKORDBTO (INDX)                 
104500       MOVE 4497-VLKOLLIB-SAMP     TO MOD-VLORDBTO (INDX)                 
104600     END-IF                                                               
104700     IF 4497-FLFARLIG              = JA                                   
104800       MOVE '*'                    TO MOD-IDPSN(INDX)                     
104900     ELSE                                                                 
105000       MOVE MFS-RENSA-FAELT        TO MOD-IDPSN(INDX)                     
105100     END-IF                                                               
105200     MOVE MFS-RENSA-FAELT          TO MOD-IDDISTR(INDX)                   
105300                                      MOD-IDKUNDNR (INDX)                 
105400                                      MOD-KDFAKTYP (INDX)                 
105500                                      MOD-IDORDNR7 (INDX)                 
105600                                      MOD-TIRFS (INDX)                    
105700                                      MOD-IDPRODNR(INDX)                  
105800       .                                                                  
105900       EJECT                                                              
106000 FC-FLYTTA-MOD-KOLLI SECTION.                                             
106100       MOVE 4498-IDDISTR        TO MOD-IDDISTR (INDX)                     
106200                                   TEST-IDDISTR                           
106300       MOVE 4498-IDKUNDNR TO MOD-IDKUNDNR (INDX)                          
106400       MOVE 4498-KDFAKTYP TO MOD-KDFAKTYP (INDX)                          
106500       MOVE 4498-IDORDNR7 TO MOD-IDORDNR7 (INDX)                          
106600       MOVE 4498-IDKOLLI        TO MOD-IDKOLLI (INDX)                     
106700                                                                          
106800       MOVE 4498-TIRFS          TO MOD-TIRFS (INDX)                       
106900                                                                          
107000       MOVE 4498-KDKOLLI        TO MOD-KDKOLLI (INDX)                     
107100                                                                          
107200       IF WS-KDMATT = 'U'                                                 
107300         COMPUTE WS-VKORDBTO             =                                
107400                 4498-VKORDBTO           *                                
107500                 CONV-KG-TO-LB                                            
107600         COMPUTE WS-VLORDBTO             =                                
107700                 4498-VLORDBTO           *                                
107800                 CONV-M3-TO-YD3                                           
107900         MOVE WS-VKORDBTO       TO MOD-VKORDBTO (INDX)                    
108000         MOVE WS-VLORDBTO       TO MOD-VLORDBTO (INDX)                    
108100                                                                          
108200       ELSE                                                               
108300         MOVE 4498-VKORDBTO     TO MOD-VKORDBTO (INDX)                    
108400         MOVE 4498-VLORDBTO     TO MOD-VLORDBTO (INDX)                    
108500       END-IF                                                             
108600                                                                          
108700       IF 4498-IDPSN(1)         >  ZERO                                   
108800          MOVE 4498-IDPSN(1)    TO W-IDPSN-NUM                            
108900          IF 4498-IDPSN(2) > ZERO                                         
109000             MOVE '*'           TO MOD-IDPSN(INDX) (1:1)                  
109100             MOVE W-IDPSN-NUM   TO MOD-IDPSN(INDX) (2:3)                  
109200          ELSE                                                            
109300             MOVE W-IDPSN-NUM   TO MOD-IDPSN(INDX) (2:3)                  
109400          END-IF                                                          
109500       ELSE                                                               
109600          MOVE SPACE            TO MOD-IDPSN(INDX)                        
109700       END-IF                                                             
109800                                                                          
109900       MOVE 4498-IDPRODNR       TO W-IDPRODNR                             
110000                                   MOD-IDPRODNR(INDX)                     
110100       .                                                                  
110200       EJECT                                                              
110300 FD-VISA-FLAGGA-DDI SECTION.                                              
110400                                                                          
110500       IF ENGLISH-TEXT                                                    
110600       MOVE 'LOAD INFO'               TO MOD-TELSTDOK                     
110700       ELSE                                                               
110800       MOVE 'LAST INFO'               TO MOD-TELSTDOK                     
110900       END-IF                                                             
111000       MOVE MFS-CLOSE-FIELD-NOMOD     TO MOD-TELSTDOK-ATTR                
111100       MOVE MFS-ADD-LAES-IN-FAELT     TO MOD-FLLSTDOK-ATTR                
111200       IF ENGLISH-TEXT                                                    
111300       MOVE 'TRANSPORTDOCUMENT NOW'   TO MOD-TETRPDOK                     
111400       ELSE                                                               
111500       MOVE 'TRANSPORTDOKUMENT NU'    TO MOD-TETRPDOK                     
111600       END-IF                                                             
111700       MOVE MFS-CLOSE-FIELD-NOMOD     TO MOD-TETRPDOK-ATTR                
111800       MOVE MFS-ADD-LAES-IN-FAELT     TO MOD-FLTRPDOK-ATTR                
111900       MOVE 'PROFORMA'                TO MOD-TEPROFORMA                   
112000       MOVE MFS-CLOSE-FIELD-NOMOD     TO MOD-TEPROFORMA-ATTR              
112100       MOVE MFS-ADD-LAES-IN-FAELT     TO MOD-FLPROFORMA-ATTR              
112200       IF DCS-NDC-NA                                                      
112300         MOVE SPACE                   TO MOD-TETRPDOK                     
112400         MOVE MFS-FORMAT-DEFAULT-ATTR TO MOD-TETRPDOK-ATTR                
112500         MOVE MFS-STAENG-FAELT        TO MOD-FLTRPDOK-ATTR                
112600       END-IF                                                             
112700                                                                          
112800                                                                          
112900     .                                                                    
113000     EJECT                                                                
113100 G-KOLLA-INPUT SECTION.                                                   
113200                                                                          
113300     MOVE ZERO                   TO W-FUNKTION                            
113400     MOVE JA                     TO INDATA-SW                             
113500     MOVE NEJ                    TO SW-RAD-INPUT                          
113600     MOVE +1                     TO INDX                                  
113700     PERFORM UNTIL (INDX > MAX-INDX)                                      
113800                                                                          
113900       IF MID-FLBACKA-RAD (INDX) = ALL '+' OR SPACE                       
114000          CONTINUE                                                        
114100       ELSE                                                               
114200          MOVE JA                TO SW-RAD-INPUT                          
114300       END-IF                                                             
114400                                                                          
114500      ADD +1                     TO INDX                                  
114600     END-PERFORM                                                          
114700                                                                          
114800     IF MID-FLAVSLUTA = ALL '+'                                           
114900                        AND  MID-FLPROFORMA  = ALL '+'                    
115000                        AND  MID-FLLSTDOK   = ALL '+'                     
115100                        AND  SW-RAD-INPUT     = NEJ                       
115200       MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                          
115300       CALL WMEDKONV USING MED-WMEDAREA                                   
115400       MOVE MED-MFSFEL           TO MOD-TEMFSFEL                          
115500       PERFORM MFS-ROER-EJ-FAELT-IN                                       
115600       PERFORM MFS-ROER-EJ-FAELT-UT                                       
115700       MOVE NEJ                  TO INDATA-SW                             
115800     ELSE                                                                 
115900                                                                          
116000       IF MID-FLAVSLUTA            =  JA OR YES                           
116100         MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLAVSLUTA-ATTR                  
116200         MOVE W-AVSLUTA            TO W-FUNKTION                          
116210*CHECK IF ANY CASE IS SHIPPED/INVOICED B4 PROCESSING                      
116220         MOVE +1                      TO  INDX                            
116230         PERFORM UNTIL INDX           >   MAX-INDX                        
116240            PERFORM GA-CHECK-CASE-STATUS                                  
116250            ADD +1                    TO INDX                             
116260         END-PERFORM                                                      
116300       ELSE                                                               
116400         IF MID-FLAVSLUTA             =  ALL '+' OR SPACE                 
116500            MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLAVSLUTA-ATTR               
116600         ELSE                                                             
116700            MOVE MFS-ALFA-FAELT-FEL   TO MOD-FLAVSLUTA-ATTR               
116800            MOVE NEJ                  TO INDATA-SW                        
116900         END-IF                                                           
117000       END-IF                                                             
117100                                                                          
117200       IF MID-FLLSTDOK             =  JA  OR YES OR NEJ                   
117300         MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLLSTDOK-ATTR                   
117400         MOVE W-LASTDOK            TO W-FUNKTION                          
117500         IF MID-FLLSTDOK = JA OR YES OR 'J' OR 'Y'                        
117600            MOVE JA                TO WS-FLLSTDOK                         
117700         ELSE                                                             
117800            MOVE MID-FLLSTDOK      TO WS-FLLSTDOK                         
117900         END-IF                                                           
118000       ELSE                                                               
118100         IF MID-FLLSTDOK              =  ALL '+' OR SPACE                 
118200            MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLLSTDOK-ATTR                
118300            MOVE NEJ                  TO WS-FLLSTDOK                      
118400         ELSE                                                             
118500            MOVE MFS-ALFA-FAELT-FEL   TO MOD-FLLSTDOK-ATTR                
118600            MOVE NEJ                  TO INDATA-SW                        
118700         END-IF                                                           
118800       END-IF                                                             
118900                                                                          
119000       IF MID-FLTRPDOK             =  JA  OR YES OR NEJ OR                
119100                                      'J' OR 'Y'                          
119200         MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLTRPDOK-ATTR                   
119300         IF MID-FLTRPDOK = JA OR YES OR 'J' OR 'Y'                        
119400            MOVE JA                TO WS-FLTRPDOK                         
119500         ELSE                                                             
119600            MOVE MID-FLTRPDOK      TO WS-FLTRPDOK                         
119700         END-IF                                                           
119800       ELSE                                                               
119900         IF MID-FLTRPDOK              =  ALL '+' OR SPACE                 
120000            MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLTRPDOK-ATTR                
120100            MOVE NEJ                  TO WS-FLTRPDOK                      
120200         ELSE                                                             
120300            MOVE MFS-ALFA-FAELT-FEL   TO MOD-FLTRPDOK-ATTR                
120400            MOVE NEJ                  TO INDATA-SW                        
120500         END-IF                                                           
120600       END-IF                                                             
120700                                                                          
120800       IF MID-FLPROFORMA           =  JA OR YES OR NEJ                    
120900         MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLPROFORMA-ATTR                 
121000         MOVE MID-FLPROFORMA       TO WS-FLPROFORMA                       
121100         IF MID-FLPROFORMA         = JA OR YES                            
121200           MOVE W-PROFORMA           TO W-FUNKTION                        
121300         END-IF                                                           
121400       ELSE                                                               
121500         IF MID-FLPROFORMA            =  ALL '+' OR SPACE                 
121600            MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLPROFORMA-ATTR              
121700            MOVE NEJ                  TO WS-FLPROFORMA                    
121800         ELSE                                                             
121900            MOVE MFS-ALFA-FAELT-FEL   TO MOD-FLPROFORMA-ATTR              
122000            MOVE NEJ                  TO INDATA-SW                        
122100         END-IF                                                           
122200       END-IF                                                             
122300                                                                          
122400       IF ( MID-FLAVSLUTA      =  YES OR JA ) AND                         
122500          ( MID-FLPROFORMA     =  YES OR JA )                             
122600         MOVE MFS-ALFA-FAELT-FEL   TO MOD-FLPROFORMA-ATTR                 
122700                                      MOD-FLAVSLUTA-ATTR                  
122800         MOVE NEJ                  TO INDATA-SW                           
122900         MOVE ZERO                 TO W-FUNKTION                          
123000         MOVE SPACE                TO WS-FLPROFORMA                       
123100       END-IF                                                             
123200                                                                          
123300       IF ( MID-FLAVSLUTA      =  YES OR JA ) AND                         
123400          ( MID-FLLSTDOK       =  YES OR JA )                             
123500         MOVE MFS-ALFA-FAELT-FEL   TO MOD-FLLSTDOK-ATTR                   
123600                                      MOD-FLAVSLUTA-ATTR                  
123700         MOVE NEJ                  TO INDATA-SW                           
123800         MOVE ZERO                 TO W-FUNKTION                          
123900         MOVE SPACE                TO WS-FLLSTDOK                         
124000       END-IF                                                             
124100                                                                          
124200       IF (( MID-FLLSTDOK    =  YES OR JA ) AND                           
124300          ( MID-FLPROFORMA   =  YES OR JA )) OR                           
124400          (( MID-FLLSTDOK    =  YES OR JA ) AND                           
124500          ( MID-FLTRPDOK     =  YES OR JA ))                              
124600         MOVE MFS-ALFA-FAELT-FEL   TO MOD-FLLSTDOK-ATTR                   
124700         MOVE NEJ                  TO INDATA-SW                           
124800         MOVE ZERO                 TO W-FUNKTION                          
124900         MOVE SPACE                TO WS-FLPROFORMA                       
125000                                      WS-FLLSTDOK                         
125100       END-IF                                                             
125200                                                                          
125300       IF ( MID-FLTRPDOK       =  YES OR JA ) AND                         
125400          ( MID-FLPROFORMA     =  YES OR JA )                             
125500         MOVE MFS-ALFA-FAELT-FEL   TO MOD-FLTRPDOK-ATTR                   
125600         MOVE NEJ                  TO INDATA-SW                           
125700         MOVE ZERO                 TO W-FUNKTION                          
125800         MOVE SPACE                TO WS-FLPROFORMA                       
125900       END-IF                                                             
126000                                                                          
126100       MOVE +1                      TO  INDX                              
126200       PERFORM UNTIL INDX           >   MAX-INDX                          
126300          IF MID-FLBACKA-RAD(INDX)  NOT = ALL '+'                         
126400             IF AVSLUTA                                                   
126500               MOVE MFS-ALFA-FAELT-FEL                                    
126600                                   TO MOD-FLBACKA-RAD-ATTR(INDX)          
126700               MOVE NEJ            TO INDATA-SW                           
126800             ELSE                                                         
126900               IF MID-FLBACKA-RAD(INDX)  = W-VALD                         
127000                 MOVE MFS-ALFA-FAELT-RAETT                                
127100                                   TO MOD-FLBACKA-RAD-ATTR(INDX)          
127200                 MOVE W-BACKA-VALDA-RADER                                 
127300                                   TO W-FUNKTION                          
127400                 PERFORM GB-KOLLA-KOLLI                                   
127500             ELSE                                                         
127600               IF MID-FLBACKA-RAD(INDX)  = SPACE                          
127700                 MOVE MFS-ALFA-FAELT-RAETT                                
127800                                    TO MOD-FLBACKA-RAD-ATTR(INDX)         
127900               ELSE                                                       
128000                 MOVE MFS-ALFA-FAELT-FEL                                  
128100                                    TO MOD-FLBACKA-RAD-ATTR(INDX)         
128200                MOVE NEJ            TO INDATA-SW                          
128300                   END-IF                                                 
128400                END-IF                                                    
128500              END-IF                                                      
128600          END-IF                                                          
128700          ADD +1                    TO INDX                               
128800       END-PERFORM                                                        
128900                                                                          
129000       IF INDATA-FEL                                                      
129100         MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                        
129200         CALL WMEDKONV USING MED-WMEDAREA                                 
129300         MOVE MED-MFSFEL           TO MOD-TEMFSFEL                        
129400         PERFORM MFS-ROER-EJ-FAELT-UT                                     
129500         PERFORM MFS-ROER-EJ-FAELT-IN                                     
129600       END-IF                                                             
129700     END-IF                                                               
129800     .                                                                    
129900     EJECT                                                                
129910 GA-CHECK-CASE-STATUS SECTION.                                            
129920                                                                          
129930     IF MID-IDDISTR(INDX) NUMERIC AND MID-IDKOLLI(INDX) > ZERO            
129940       MOVE MID-IDPRODNR(INDX)        TO W-IDPRODNR-KOLLI                 
129950       MOVE MID-IDKOLLI(INDX)         TO W-IDKOLLI-KOLLI                  
129960                                                                          
129970       PERFORM IMS-GU-WDE611                                              
129980       IF SEGMENT-FINNS                                                   
129981*STATUS OF SHIPPED OR INVOICE CASE WILL BE 7 OR 9                         
129990         IF KOLLI-KDKOLSTA > KLI-PACK-FAKT                                
129991            MOVE MFS-ALFA-FAELT-FEL   TO MOD-FLAVSLUTA-ATTR               
129992            MOVE NEJ                  TO INDATA-SW                        
129993         END-IF                                                           
129994       ELSE                                                               
129995         MOVE MFS-ALFA-FAELT-FEL      TO MOD-FLAVSLUTA-ATTR               
129996         MOVE NEJ                     TO INDATA-SW                        
129997       END-IF                                                             
129998     END-IF                                                               
129999     .                                                                    
130000     EJECT                                                                
130010 GB-KOLLA-KOLLI   SECTION.                                                
130100                                                                          
130200     IF MID-IDDISTR(INDX) NOT NUMERIC                                     
130300*      KONTROLL AV SK                                                     
130400       IF MID-IDKOLLI(INDX) > ZERO                                        
130500         CONTINUE                                                         
130600       ELSE                                                               
130700         MOVE MFS-ALFA-FAELT-FEL TO MOD-FLBACKA-RAD-ATTR(INDX)            
130800         MOVE NEJ                    TO INDATA-SW                         
130900       END-IF                                                             
131000     ELSE                                                                 
131100       MOVE MID-IDPRODNR(INDX)   TO W-IDPRODNR-KOLLI                      
131200       MOVE MID-IDKOLLI(INDX)    TO W-IDKOLLI-KOLLI                       
131300                                                                          
131400       PERFORM IMS-GU-WDE611                                              
131500       IF SEGMENT-FINNS                                                   
131600         IF KOLLI-KDKOLSTA = KLI-PACK-FAKT                                
131700           CONTINUE                                                       
131800         ELSE                                                             
131900           MOVE NEJ                TO INDATA-SW                           
132000           MOVE MFS-ALFA-FAELT-FEL TO MOD-FLBACKA-RAD-ATTR(INDX)          
132100         END-IF                                                           
132200       ELSE                                                               
132300         MOVE NEJ                  TO INDATA-SW                           
132400         MOVE MFS-ALFA-FAELT-FEL   TO MOD-FLBACKA-RAD-ATTR(INDX)          
132500       END-IF                                                             
132600     END-IF                                                               
132700     .                                                                    
132800     EJECT                                                                
132900     EJECT                                                                
133000 H-UPPDATERA SECTION.                                                     
133100                                                                          
133200     MOVE ZERO           TO W-KVKOLLI                                     
133300                            W-SUORDV                                      
133400                            W-SUORDV-TOT                                  
133500                            W-SUORDV-EXP                                  
133600                            W-SUORDV-TOT-EXP                              
133700                            W-VKORDBTO                                    
133800                            W-VLORDBTO                                    
133900                            W-UPD-RAKNARE                                 
134000                            W-UPD-COPY                                    
134100                                                                          
134200     MOVE NEJ            TO SW-OMSTART                                    
134300                                                                          
134400     IF RESTART-COPY                                                      
134500       MOVE W-PROFORMA  TO W-FUNKTION                                     
134600       MOVE MID-IDDISTR-DOLD     TO  TEST-IDDISTR                         
134700     END-IF                                                               
134800                                                                          
134900     EVALUATE TRUE                                                        
135000                                                                          
135100     WHEN AVSLUTA                                                         
135200         PERFORM HD-STARTA-FAKTURA-BILLIT-BOLLA                           
135300                                                                          
135400     WHEN PROFORMA                                                        
135500         PERFORM HE-STARTA-PROFORMA-BILLIT                                
135600                                                                          
135700     WHEN LASTDOK                                                         
135800         PERFORM HF-STARTA-LASTDOK                                        
135900                                                                          
136000     WHEN BACKA-VALDA-RADER                                               
136100         PERFORM HB-BACKA-VALDA-KOLLIN                                    
136200         PERFORM S04-TA-EV-BORT-ARBREG-ROT                                
136300                                                                          
136400     END-EVALUATE                                                         
136500                                                                          
136600     IF W-FUNKTION            NOT = 0                                     
136700        MOVE INF-UPDATE-DONE  TO MED-IDMFSINF                             
136800        CALL WMEDKONV USING MED-WMEDAREA                                  
136900        MOVE MED-MFSINF       TO MOD-TEMFSINF                             
137000     END-IF                                                               
137100     PERFORM MFS-FORM-ATTR                                                
137200     PERFORM MFS-RENSA-FAELT-IN                                           
137300     .                                                                    
137400     EJECT                                                                
137500 HB-BACKA-VALDA-KOLLIN SECTION.                                           
137600                                                                          
137700     MOVE +1                      TO INDX                                 
137800     PERFORM UNTIL INDX           >  MAX-INDX OR OMSTART                  
137900       IF MID-FLBACKA-RAD(INDX)   =  W-VALD                               
138000         IF MID-IDDISTR(INDX) NUMERIC                                     
138100           MOVE MID-IDPRODNR(INDX) TO W-IDPRODNR-KOLLI                    
138200           MOVE MID-IDKOLLI(INDX) TO W-IDKOLLI-KOLLI                      
138300           PERFORM S01-UPPDATERA-WDE6                                     
138400                                                                          
138500           MOVE MID-IDDISTR(INDX)  TO W-IDDISTR                           
138600                                      TEST-IDDISTR                        
138700           MOVE MID-IDKUNDNR(INDX) TO W-IDKUNDNR                          
138800                                                                          
138900           MOVE MID-KDFAKTYP(INDX)      TO W-KDFAKTYP                     
139000           MOVE SPACE                   TO W-IDKUNDRF                     
139100           INSPECT MID-IDORDNR7(INDX)                                     
139200                     REPLACING LEADING SPACE BY ZERO                      
139300           MOVE MID-IDORDNR7(INDX)      TO W-IDORDNR7                     
139400           MOVE MID-IDPRODNR(INDX)      TO W-IDPRODNR                     
139500           MOVE MID-IDKOLLI(INDX)       TO W-IDKOLLI                      
139600           PERFORM S02-TAG-BORT-ARBREG                                    
139700         ELSE                                                             
139800           PERFORM HBA-BACKA-SAMKOLLI                                     
139900         END-IF                                                           
140000       END-IF                                                             
140100       IF NOT OMSTART                                                     
140200          MOVE MFS-RENSA-FAELT    TO MID-FLBACKA-RAD(INDX)                
140300       END-IF                                                             
140400       ADD +1                     TO INDX                                 
140500     END-PERFORM                                                          
140600     PERFORM S03-UPPDATERA-ARBREG-TOT                                     
140700     .                                                                    
140800     EJECT                                                                
140900 HBA-BACKA-SAMKOLLI SECTION.                                              
141000                                                                          
141100     PERFORM IMS-GHU-4495                                                 
141200     IF SEGMENT-FINNS                                                     
141300       MOVE MID-IDKOLLI (INDX)  TO W-IDKOLLIS                             
141400       PERFORM IMS-GHNP-WDGX4498-SKOLLI                                   
141500                                                                          
141600       PERFORM UNTIL SEGMENT-SAKNAS OR OMSTART                            
141700         MOVE 4498-IDPRODNR     TO W-IDPRODNR-KOLLI                       
141800         MOVE 4498-IDKOLLI      TO W-IDKOLLI-KOLLI                        
141900         MOVE ZERO              TO W-SUORDV                               
142000                                   W-SUORDV-EXP                           
142100         PERFORM IMS-GHU-WDE611                                           
142200                                                                          
142300         IF SEGMENT-FINNS                                                 
142400           MOVE SPACE                  TO KOLLI-IDLBBET                   
142500           MOVE ZERO                   TO KOLLI-TILASTN                   
142600           MOVE KLI-PACK               TO KOLLI-KDKOLSTA                  
142700           MOVE SK-CLOSED              TO KOLLI-KDSTASKLI                 
142800           PERFORM IMS-REPL-WDE611                                        
142900                                                                          
143000           MOVE KOLLI-VLORDBTO-KOLLI   TO W-SPAR-VLORDBTO                 
143100           MOVE KOLLI-VKORDBTO-KOLLI   TO W-SPAR-VKORDBTO                 
143200           MOVE 4498-IDDISTR           TO  TEST-IDDISTR                   
143300           IF DIST79-DEALER-PRICE                                         
143400             MOVE KOLLI-SUORDV-LOC     TO W-SPAR-SUORDV-LOC               
143500             MOVE KOLLI-SUORDV-LOCPREL TO W-SPAR-SUORDV-LOCPREL           
143600             ADD KOLLI-SUORDV-LOC      TO W-SUORDV                        
143700             ADD KOLLI-SUORDV-LOCPREL  TO W-SUORDV                        
143800             PERFORM S01A-OMRAKNING-LOC-TO-SEK                            
143900             MOVE EXCH-SUORDV-UT       TO W-SUORDV                        
144000           ELSE                                                           
144110             IF DIST79-ECOM-PRICE                                         
144200               MOVE KOLLI-SUORDV-LOC   TO W-SPAR-SUORDV-LOC               
144300               ADD KOLLI-SUORDV-LOC    TO W-SUORDV                        
144400             ELSE                                                         
144500               IF KOLLI-SUORDV-KLI-EXP > ZERO                             
144600                 MOVE KOLLI-SUORDV-KLI-EXP TO W-SPAR-SUORDV               
144700               ELSE                                                       
144800                 MOVE KOLLI-SUORDV-KOLLI TO W-SPAR-SUORDV                 
144900               END-IF                                                     
145000               ADD KOLLI-SUORDV-KOLLI    TO W-SUORDV                      
145100               ADD KOLLI-SUORDV-KLI-EXP  TO W-SUORDV-EXP                  
145200             END-IF                                                       
145300           END-IF                                                         
145400         END-IF                                                           
145500                                                                          
145600         PERFORM IMS-GHU-WDE601                                           
145700         SUBTRACT +1             FROM VORD-KVKOLLI-FL                     
145800         IF DIST79-DEALER-PRICE                                           
145900           SUBTRACT W-SPAR-SUORDV-LOC    FROM                             
146000                                       VORD-SUORDV-FL-LOC                 
146100           SUBTRACT W-SPAR-SUORDV-LOCPREL FROM                            
146200                                       VORD-SUORDV-FL-LOCPREL             
146300         ELSE                                                             
146410           IF DIST79-ECOM-PRICE                                           
146500             SUBTRACT W-SPAR-SUORDV-LOC  FROM                             
146600                                         VORD-SUORDV-FL-LOC               
146700           ELSE                                                           
146800             SUBTRACT W-SPAR-SUORDV FROM VORD-SUORDV-FL                   
146900           END-IF                                                         
147000         END-IF                                                           
147100         SUBTRACT W-SPAR-VKORDBTO FROM VORD-VKORDBTO-FL                   
147200         SUBTRACT W-SPAR-VLORDBTO FROM VORD-VLORDBTO-FL                   
147300                                                                          
147400         PERFORM IMS-REPL-WDE601                                          
147500         ADD W-SUORDV         TO W-SUORDV-TOT                             
147600         ADD W-SUORDV-EXP     TO W-SUORDV-TOT-EXP                         
147700         MOVE ZERO            TO W-SUORDV                                 
147800                                 W-SUORDV-EXP                             
147900         ADD +2               TO W-UPD-RAKNARE                            
148000                                                                          
148100         PERFORM IMS-DLET-WDGX4498                                        
148200         ADD +1               TO W-UPD-RAKNARE                            
148300                                                                          
148400         PERFORM IMS-GHNP-WDGX4498-SKOLLI                                 
148500         IF W-UPD-RAKNARE      > W-UPD-MAX                                
148600           MOVE JA            TO SW-OMSTART                               
148700         END-IF                                                           
148800       END-PERFORM                                                        
148900                                                                          
149000*      TAR ÄVEN BORT SAMLINGSKOLLISEG.- 4497                              
149100                                                                          
149200       MOVE MID-IDKOLLI (INDX) TO W-IDKOLLIS                              
149300       PERFORM IMS-GHU-WDGX4497                                           
149400       IF SEGMENT-FINNS                                                   
149500         ADD 4497-VLKOLLIB-SAMP TO W-VLORDBTO                             
149600         ADD 4497-VKKOLLIB-SAMP TO W-VKORDBTO                             
149700*        MAN ADDERAR BARA 1 / PER SAMLINGSKOLLIN TILL ANTAL KOLLIN        
149800         ADD +1                TO  W-KVKOLLI                              
149900         PERFORM IMS-DLET-WDGX4497                                        
150000         ADD +1                TO W-UPD-RAKNARE                           
150100                                                                          
150200*        UPPDATERAR SAMLINGSKOLLISEG.- WDE711                             
150300         MOVE MID-IDKOLLI (INDX) TO W-IDKOLLIS-E7                         
150400         PERFORM IMS-GHU-WDE711-ASEQ                                      
150500         MOVE SK-CLOSED        TO SKLI-KDSTASKLI                          
150600         PERFORM IMS-REPL-WDE711                                          
150700         ADD +1                TO W-UPD-RAKNARE                           
150800       END-IF                                                             
150900                                                                          
151000     END-IF                                                               
151100     .                                                                    
151200     EJECT                                                                
151300 HD-STARTA-FAKTURA-BILLIT-BOLLA SECTION.                                  
151400                                                                          
151500     MOVE ALL '+'              TO  MOD4675-MID-W4I67501                   
151600     MOVE WS-IDTRPTNR          TO  MOD4675-MID-IDTRPTNR-IN                
151700                                   MOD4698-MID-IDTRPTNR                   
151800     MOVE WS-IDLBBET           TO  MOD4675-MID-IDLBBET-IN                 
151900                                   MOD4698-MID-IDLBBET                    
152000     MOVE WS-FLFARLIG          TO  MOD4675-MID-FLFARLIG-IN                
152100                                   MOD4698-MID-FLFARLIG                   
152200     MOVE W-IDDC               TO  MOD4675-MID-IDDC-IN                    
152300                                   MOD4698-MID-IDDC                       
152400     MOVE ZERO                 TO  MOD4675-MID-IDSHIPM                    
152500     MOVE WS-FLTRPDOK          TO  MOD4675-MID-FLSKRIV-NU                 
152600                                   MOD4698-MID-FLSKRIV-NU                 
152700                                                                          
152800     MOVE LOW-VALUE            TO  P-TO-P-KDZ1                            
152900     MOVE LOW-VALUE            TO  P-TO-P-KDZ2                            
153000                                                                          
153100     MOVE MID-IDDISTR-DOLD     TO  TEST-IDDISTR                           
153200     MOVE WS-IDDC-IN           TO  WS-IDDC                                
153300                                                                          
153400     IF WS-IDDC NOT = W-IDDC-B6                                           
153500        MOVE WS-IDDC TO W-IDDC-B6                                         
153600        PERFORM IMS-GU-WDB601                                             
153700     END-IF                                                               
153800                                                                          
153900     IF DCS-SDC AND DCS-IDLANDX2 = 'IT'                                   
154000       IF WS-IDTRPTNR = '90 ' OR '090'                                    
154100****               INGEN BOLLA SKALL SKAPAS FÖR TRANSPORT 90! ****        
154200         PERFORM HDA-STARTA-4675                                          
154300       ELSE                                                               
154400         MOVE 'W4T698X '         TO P-TO-P-KDTRANS                        
154500         MOVE '4664'             TO P-TO-P-IDTRANS                        
154600         MOVE MFS-KDMFSFOR       TO P-TO-P-KDMFSFOR                       
154700         COMPUTE P-TO-P-KVLL =                                            
154800            LENGTH OF MOD4698-MID-W4I69801 + 17                           
154900                                                                          
155000         MOVE MOD4698-MID-W4I69801 TO P-TO-P-DATA                         
155100         PERFORM IMS-ISRT-ALT-MSG-4698                                    
155200       END-IF                                                             
155300     ELSE                                                                 
155400       PERFORM HDA-STARTA-4675                                            
155500     END-IF                                                               
155600     .                                                                    
155700     EJECT                                                                
155800 HDA-STARTA-4675  SECTION.                                                
155900     PERFORM S10-VILKEN-TRANS                                             
156000     IF FL-XTRANS                                                         
156100*            BAKGRUNDS-TRANS                                              
156200       MOVE 'W4T675X '        TO P-TO-P-KDTRANS                           
156300       MOVE '466D'            TO P-TO-P-IDTRANS                           
156400       MOVE MFS-KDMFSFOR      TO P-TO-P-KDMFSFOR                          
156500                                                                          
156600       COMPUTE P-TO-P-KVLL =  LENGTH OF MOD4675-MID-W4I67501 + 17         
156700       MOVE MOD4675-MID-W4I67501 TO P-TO-P-DATA                           
156800       PERFORM IMS-ISRT-ALT-MSG-4675X                                     
156900     ELSE                                                                 
157000*             UPPDATERINGS-TRANS FÖR IMPORTÖRER                           
157100       MOVE 'W4T675  '           TO P-TO-P-KDTRANS                        
157200       MOVE '466D'               TO P-TO-P-IDTRANS                        
157300       MOVE MFS-KDMFSFOR         TO P-TO-P-KDMFSFOR                       
157400       COMPUTE P-TO-P-KVLL =  LENGTH OF MOD4675-MID-W4I67501 + 17         
157500       MOVE MOD4675-MID-W4I67501 TO P-TO-P-DATA                           
157600       PERFORM IMS-ISRT-ALT-MSG-4675                                      
157700     END-IF                                                               
157800     .                                                                    
157900     EJECT                                                                
158000 HE-STARTA-PROFORMA-BILLIT SECTION.                                       
158100                                                                          
158200** COPY 4495 TO 4479                                                      
158300     PERFORM IMS-GHU-4495                                                 
158400     IF SEGMENT-FINNS                                                     
158500       MOVE W-IDHTYP-4479    TO 4479-IDHTYP                               
158600       MOVE 4495-IDDC        TO 4479-IDDC                                 
158700                                W-IDDC-4479                               
158800       MOVE 4495-IDTRPTNR    TO 4479-IDTRPTNR                             
158900                                W-IDTRPTNR-4479                           
159000       MOVE 4495-IDLBBET     TO 4479-IDLBBET                              
159100                                W-IDLBBET-4479                            
159200       MOVE LOW-VALUE        TO 4479-LOW-VALUE                            
159300       IF RESTART-COPY                                                    
159400         MOVE SPACE          TO SAVE-RESTART-COPY                         
159500         MOVE JA             TO UPDATE-SAVE-SW                            
159600** READ 4498 WITH KEY FROM SAVE-ARE                                       
159700         MOVE SAVE-IDDISTR   TO W-IDDISTR                                 
159800         MOVE SAVE-IDKUNDNR  TO W-IDKUNDNR                                
159900         MOVE SAVE-KDFAKTYP  TO W-KDFAKTYP                                
160000         MOVE SAVE-IDKUNDRF  TO W-IDKUNDRF                                
160100         MOVE SAVE-IDPRODNR  TO W-IDPRODNR                                
160200         MOVE SAVE-IDKOLLI   TO W-IDKOLLI                                 
160300         PERFORM  IMS-GHNP-WDGX4498-FIRST                                 
160400       ELSE                                                               
160500         PERFORM  IMS-ISRT-4479                                           
160600         ADD 1 TO W-UPD-COPY                                              
160700         PERFORM  IMS-GHU-WDGX4496                                        
160800         IF SEGMENT-FINNS                                                 
160900           MOVE 4496-KDSEGKEY         TO 4480-KDSEGKEY                    
161000           MOVE 4496-KVKOLLI-LAST     TO 4480-KVKOLLI-LAST                
161100           MOVE 4496-VKORDBTO-LASTB   TO 4480-VKORDBTO-LASTB              
161200           MOVE 4496-VLORDBTO-LASTB   TO 4480-VLORDBTO-LASTB              
161300           PERFORM IMS-ISRT-WDGX4480                                      
161400           ADD 1 TO W-UPD-COPY                                            
161500         END-IF                                                           
161600         PERFORM  IMS-GHNP-WDGX4498                                       
161700       END-IF                                                             
161800                                                                          
161900       PERFORM UNTIL NOT SEGMENT-FINNS OR RESTART-COPY                    
162000** COPY 4498 TO 4482                                                      
162100         MOVE 4498-IDDISTR         TO    4482-IDDISTR                     
162200         MOVE 4498-IDKUNDNR        TO    4482-IDKUNDNR                    
162300         MOVE 4498-KDFAKTYP        TO    4482-KDFAKTYP                    
162400         MOVE 4498-IDKUNDRF        TO    4482-IDKUNDRF                    
162500         MOVE 4498-IDPRODNR        TO    4482-IDPRODNR                    
162600         MOVE 4498-IDKOLLI         TO    4482-IDKOLLI                     
162700         MOVE 4498-IDDEALER        TO    4482-IDDEALER                    
162800         MOVE 4498-IDPSN (1)       TO    4482-IDPSN (1)                   
162900         MOVE 4498-IDPSN (2)       TO    4482-IDPSN (2)                   
163000         MOVE 4498-KDKOLLI         TO    4482-KDKOLLI                     
163100         MOVE 4498-TIRFS           TO    4482-TIRFS                       
163200         MOVE 4498-VKORDBTO        TO    4482-VKORDBTO                    
163300         MOVE 4498-VLORDBTO        TO    4482-VLORDBTO                    
163400         PERFORM IMS-ISRT-WDGX4482                                        
163500         ADD 1 TO W-UPD-COPY                                              
163600                                                                          
163700         PERFORM IMS-GHNP-WDGX4498                                        
163800                                                                          
163900         IF SEGMENT-FINNS                                                 
164000           IF W-UPD-COPY > W-UPD-MAX                                      
164100             MOVE JA             TO SAVE-RESTART-COPY                     
164200                                    UPDATE-SAVE-SW                        
164300             MOVE 4498-IDDISTR   TO SAVE-IDDISTR                          
164400             MOVE 4498-IDKUNDNR  TO SAVE-IDKUNDNR                         
164500             MOVE 4498-KDFAKTYP  TO SAVE-KDFAKTYP                         
164600             MOVE 4498-IDKUNDRF  TO SAVE-IDKUNDRF                         
164700             MOVE 4498-IDPRODNR  TO SAVE-IDPRODNR                         
164800             MOVE 4498-IDKOLLI   TO SAVE-IDKOLLI                          
164900           END-IF                                                         
165000         END-IF                                                           
165100       END-PERFORM                                                        
165200                                                                          
165300       IF NOT RESTART-COPY                                                
165400                                                                          
165500         MOVE ALL '+'              TO  MOD4677-MID-W4I67701               
165600         MOVE WS-IDTRPTNR          TO  MOD4677-MID-IDTRPTNR-IN            
165700         MOVE WS-IDLBBET           TO  MOD4677-MID-IDLBBET-IN             
165800         MOVE WS-FLFARLIG          TO  MOD4677-MID-FLFARLIG-IN            
165900         MOVE W-IDDC               TO  MOD4677-MID-IDDC-IN                
166000         MOVE ZERO                 TO  MOD4677-MID-IDSHIPM                
166100         MOVE LOW-VALUE            TO  P-TO-P-KDZ1                        
166200         MOVE LOW-VALUE            TO  P-TO-P-KDZ2                        
166300                                                                          
166400         MOVE MID-IDDISTR-DOLD     TO  TEST-IDDISTR                       
166500         MOVE WS-IDDC-IN           TO  WS-IDDC                            
166600                                                                          
166700         PERFORM S10-VILKEN-TRANS                                         
166800         IF FL-XTRANS                                                     
166900****           BAKGRUNDS-TRANS                                            
167000           MOVE 'Y'               TO MOD4677-MID-FLAVSLUTA                
167100           MOVE 'W4T677X '        TO P-TO-P-KDTRANS                       
167200           MOVE '466D'            TO P-TO-P-IDTRANS                       
167300           MOVE MFS-KDMFSFOR      TO P-TO-P-KDMFSFOR                      
167400           COMPUTE P-TO-P-KVLL =                                          
167500                        LENGTH OF MOD4677-MID-W4I67701 + 17               
167600           MOVE MOD4677-MID-W4I67701 TO P-TO-P-DATA                       
167700           PERFORM IMS-ISRT-ALT-MSG-4677X                                 
167800         ELSE                                                             
167900**            UPPDATERINGS-TRANS FÖR IMPORTÖRER                           
168000           MOVE 'N'                  TO MOD4677-MID-FLAVSLUTA             
168100           MOVE 'W4T677  '           TO P-TO-P-KDTRANS                    
168200           MOVE '466D'               TO P-TO-P-IDTRANS                    
168300           MOVE MFS-KDMFSFOR         TO P-TO-P-KDMFSFOR                   
168400           COMPUTE P-TO-P-KVLL =                                          
168500                        LENGTH OF MOD4677-MID-W4I67701 + 17               
168600           MOVE MOD4677-MID-W4I67701 TO P-TO-P-DATA                       
168700           PERFORM IMS-ISRT-ALT-MSG-4677                                  
168800         END-IF                                                           
168900       END-IF                                                             
169000     END-IF                                                               
169100     .                                                                    
169200     EJECT                                                                
169300 HF-STARTA-LASTDOK  SECTION.                                              
169400                                                                          
169500     MOVE ALL '+'              TO  MOD4639-MID-W4I63901                   
169600     MOVE WS-IDTRPTNR          TO  MOD4639-MID-IDTRPTNR-IN                
169700     MOVE WS-IDLBBET           TO  MOD4639-MID-IDLBBET-IN                 
169800     MOVE WS-FLFARLIG          TO  MOD4639-MID-FLFARLIG-IN                
169900     MOVE W-IDDC               TO  MOD4639-MID-IDDC-IN                    
170000     MOVE ZERO                 TO  MOD4639-MID-IDSHIPM                    
170100     MOVE LOW-VALUE            TO  P-TO-P-KDZ1                            
170200     MOVE LOW-VALUE            TO  P-TO-P-KDZ2                            
170300                                                                          
170400     MOVE MID-IDDISTR-DOLD     TO  TEST-IDDISTR                           
170500     MOVE WS-IDDC-IN           TO  WS-IDDC                                
170600                                                                          
170700     MOVE 'Y'                  TO MOD4639-MID-FLAVSLUTA                   
170800     MOVE 'W40639X '           TO P-TO-P-KDTRANS                          
170900     MOVE '466D'               TO P-TO-P-IDTRANS                          
171000     MOVE MFS-KDMFSFOR         TO P-TO-P-KDMFSFOR                         
171100     COMPUTE P-TO-P-KVLL =                                                
171200                    LENGTH OF MOD4639-MID-W4I63901 + 17                   
171300     MOVE MOD4639-MID-W4I63901 TO P-TO-P-DATA                             
171400     PERFORM IMS-ISRT-ALT-MSG-4639X                                       
171500     .                                                                    
171600     EJECT                                                                
171700 I-STARTA-W40664          SECTION.                                        
171800                                                                          
171900     COMPUTE P-TO-P-KVLL       =   LNG-P-TO-P-PREFIX +                    
172000                                   LENGTH OF MID-W4I66401                 
172100                                                                          
172200     MOVE LOW-VALUE            TO P-TO-P-KDZ1                             
172300     MOVE LOW-VALUE            TO P-TO-P-KDZ2                             
172400     MOVE 'W4T664U '           TO P-TO-P-KDTRANS                          
172500     MOVE '4664'               TO P-TO-P-IDTRANS                          
172600     MOVE MFS-KDMFSFOR         TO P-TO-P-KDMFSFOR                         
172700                                                                          
172800     MOVE MID-W4I66401         TO P-TO-P-DATA                             
172900     PERFORM IMS-ISRT-ALT-MSG-4664                                        
173000     .                                                                    
173100     EJECT                                                                
173200 S01-UPPDATERA-WDE6    SECTION.                                           
173300                                                                          
173400     MOVE ZERO TO W-SUORDV                                                
173500                  W-SUORDV-EXP                                            
173600     PERFORM IMS-GHU-WDE611                                               
173700                                                                          
173800     IF SEGMENT-FINNS                                                     
173900       MOVE JA                  TO KOLLI-FLUTLAST                         
174000       MOVE SPACE               TO KOLLI-IDLBBET                          
174100       MOVE ZERO                TO KOLLI-TILASTN                          
174200       MOVE KLI-PACK            TO KOLLI-KDKOLSTA                         
174300       PERFORM IMS-REPL-WDE611                                            
174400                                                                          
174500       ADD 1                     TO W-KVKOLLI                             
174600       ADD  KOLLI-VLORDBTO-KOLLI TO W-VLORDBTO                            
174700       ADD  KOLLI-VKORDBTO-KOLLI TO W-VKORDBTO                            
174800       MOVE KOLLI-VLORDBTO-KOLLI TO W-SPAR-VLORDBTO                       
174900       MOVE KOLLI-VKORDBTO-KOLLI TO W-SPAR-VKORDBTO                       
175000       IF DIST79-DEALER-PRICE                                             
175100         MOVE KOLLI-SUORDV-LOC     TO W-SPAR-SUORDV-LOC                   
175200         MOVE KOLLI-SUORDV-LOCPREL TO W-SPAR-SUORDV-LOCPREL               
175300         ADD  KOLLI-SUORDV-LOC     TO W-SUORDV                            
175400         ADD  KOLLI-SUORDV-LOCPREL TO W-SUORDV                            
175500         PERFORM S01A-OMRAKNING-LOC-TO-SEK                                
175600         MOVE EXCH-SUORDV-UT       TO W-SUORDV                            
175700       ELSE                                                               
175810         IF DIST79-ECOM-PRICE                                             
175900           MOVE KOLLI-SUORDV-LOC   TO W-SPAR-SUORDV-LOC                   
176000           ADD KOLLI-SUORDV-LOC    TO W-SUORDV                            
176100         ELSE                                                             
176200           IF KOLLI-SUORDV-KLI-EXP > ZERO                                 
176300             MOVE KOLLI-SUORDV-KLI-EXP TO W-SPAR-SUORDV                   
176400           ELSE                                                           
176500             MOVE KOLLI-SUORDV-KOLLI TO W-SPAR-SUORDV                     
176600           END-IF                                                         
176700           ADD KOLLI-SUORDV-KOLLI  TO W-SUORDV                            
176800           ADD KOLLI-SUORDV-KLI-EXP TO W-SUORDV-EXP                       
176900         END-IF                                                           
177000       END-IF                                                             
177100     END-IF                                                               
177200                                                                          
177300     PERFORM IMS-GHU-WDE601                                               
177400     SUBTRACT +1               FROM VORD-KVKOLLI-FL                       
177500     MOVE MID-IDDISTR-DOLD     TO  TEST-IDDISTR                           
177600     IF DIST79-DEALER-PRICE                                               
177700       SUBTRACT W-SPAR-SUORDV-LOC    FROM VORD-SUORDV-FL-LOC              
177800       SUBTRACT W-SPAR-SUORDV-LOCPREL FROM VORD-SUORDV-FL-LOCPREL         
177900     ELSE                                                                 
178010       IF DIST79-ECOM-PRICE                                               
178100         SUBTRACT W-SPAR-SUORDV-LOC  FROM VORD-SUORDV-FL-LOC              
178200       ELSE                                                               
178300         SUBTRACT W-SPAR-SUORDV      FROM VORD-SUORDV-FL                  
178400       END-IF                                                             
178500     END-IF                                                               
178600     SUBTRACT W-SPAR-VKORDBTO  FROM VORD-VKORDBTO-FL                      
178700     SUBTRACT W-SPAR-VLORDBTO  FROM VORD-VLORDBTO-FL                      
178800                                                                          
178900     PERFORM IMS-REPL-WDE601                                              
179000     ADD W-SUORDV     TO W-SUORDV-TOT                                     
179100     ADD W-SUORDV-EXP TO W-SUORDV-TOT-EXP                                 
179200     MOVE ZERO        TO W-SUORDV                                         
179300                         W-SUORDV-EXP                                     
179400     .                                                                    
179500     EJECT                                                                
179600 S01A-OMRAKNING-LOC-TO-SEK SECTION.                                       
179700                                                                          
179800     MOVE KOLLI-KDVALISO        TO CURR-KDVALISO-ROW                      
179900     MOVE W-DATE-AAMM           TO CURR-TIAAMM                            
180000     MOVE WS-KDVALISO-HUV       TO CURR-KDVALISO-HUV                      
180100     MOVE 'M'                   TO CURR-KDVALTYP                          
180200                                                                          
180300     IF CURR-KDVALISO-ROW =  'EUR'  OR 'GBP' OR 'SEK'                     
180400       OR 'CHF' OR 'NOK' OR 'DKK'                                         
180500       OR 'USD' OR 'CAD'                                                  
180600       CONTINUE                                                           
180700     ELSE                                                                 
180800       IF DIST79-DEALER-PRICE                                             
180900          MOVE KOLLI-IDDISTR    TO W-IDDISTR-WDB2                         
181000          MOVE KOLLI-IDKUNDNR   TO W-IDKUNDNR-WDB2                        
181100          PERFORM IMS-GU-WDB201                                           
181200          MOVE GMT-IDPARTNR     TO W-WDB1-IDPARTNR                        
181300          MOVE GMT-IDFTG        TO W-WDB1-IDFTG                           
181400          PERFORM IMS-GU-WDB101                                           
181500          MOVE BET-KDVALISO     TO CURR-KDVALISO-ROW                      
181600       ELSE                                                               
181700          MOVE 'SEK'            TO CURR-KDVALISO-ROW                      
181800       END-IF                                                             
181900     END-IF                                                               
182000                                                                          
182100     CALL W510CURR USING CURR-W510CURR WDG2-PCB                           
182200     IF CURR-KDSVAR = ' '                                                 
182300       MOVE CURR-PRKURS-NEW  TO EXCH-PRKURS                               
182400     ELSE                                                                 
182500       MOVE 1                TO EXCH-PRKURS                               
182600     END-IF                                                               
182700*      +1 KDCALL = LOKAL VALUTA TILL SEK                                  
182800     MOVE +1                 TO EXCH-KDCALL                               
182900     MOVE W-SUORDV           TO EXCH-SUORDV-IN                            
183000     MOVE +0                 TO EXCH-PRARTNTO-IN                          
183100     CALL W411EXCH USING    EXCH-W411EXCH                                 
183200     .                                                                    
183300     EJECT                                                                
183400 S02-TAG-BORT-ARBREG       SECTION.                                       
183500                                                                          
183600     PERFORM IMS-GHU-WDGX4498                                             
183700     IF SEGMENT-FINNS                                                     
183800        PERFORM IMS-DLET-WDGX4498                                         
183900     END-IF                                                               
184000     .                                                                    
184100     EJECT                                                                
184200 S03-UPPDATERA-ARBREG-TOT  SECTION.                                       
184300                                                                          
184400     PERFORM IMS-GHU-WDGX4496                                             
184500     IF SEGMENT-FINNS                                                     
184600       SUBTRACT W-KVKOLLI         FROM 4496-KVKOLLI-LAST                  
184700       IF W-SUORDV-TOT-EXP > ZERO                                         
184800         SUBTRACT W-SUORDV-TOT-EXP FROM 4496-SUORDV-LASTB                 
184900       ELSE                                                               
185000         SUBTRACT W-SUORDV-TOT    FROM 4496-SUORDV-LASTB                  
185100       END-IF                                                             
185200       SUBTRACT W-VKORDBTO        FROM 4496-VKORDBTO-LASTB                
185300       SUBTRACT W-VLORDBTO        FROM 4496-VLORDBTO-LASTB                
185400       PERFORM IMS-REPL-WDGX4496                                          
185500     END-IF                                                               
185600     .                                                                    
185700     EJECT                                                                
185800 S04-TA-EV-BORT-ARBREG-ROT SECTION.                                       
185900                                                                          
186000     PERFORM IMS-GU-WDGX4498-OKVAL                                        
186100     IF SEGMENT-SAKNAS                                                    
186200        PERFORM IMS-GHU-4495                                              
186300        IF SEGMENT-FINNS                                                  
186400           PERFORM IMS-DLET-4495                                          
186500        END-IF                                                            
186600     END-IF                                                               
186700                                                                          
186800     .                                                                    
186900     EJECT                                                                
187000 S10-VILKEN-TRANS   SECTION.                                              
187100                                                                          
187200     MOVE NEJ                            TO SW-FLTRANS                    
187300     SET IDDC-IX  TO  1                                                   
187400     SEARCH TRANS-TABELL                                                  
187500              AT END                                                      
187600                     MOVE NEJ            TO SW-FLTRANS                    
187700            WHEN TRA-IDDC (IDDC-IX) = W-IDDC                              
187800       AND                                                                
187900            TRA-IDDISTR-FOM (IDDC-IX) NOT > W-IDDISTR-ALFA                
188000       AND                                                                
188100            TRA-IDDISTR-TOM (IDDC-IX) NOT < W-IDDISTR-ALFA                
188200                                                                          
188300                   MOVE JA               TO SW-FLTRANS                    
188400     END-SEARCH                                                           
188500     .                                                                    
188600     EJECT                                                                
188700 MFS-RENSA-FAELT-UT SECTION.                                              
188800                                                                          
188900*    --- ALLA UTDATA-FÄLT                                                 
189000*    --- INKL. BLÄDDRINGSNYCKLAR                                          
189100     MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-ENTER                            
189200                             MOD-IDDISTR-NEXT                             
189300                             MOD-IDKUNDNR-ENTER                           
189400                             MOD-IDKUNDNR-NEXT                            
189500                             MOD-KDFAKTYP-ENTER                           
189600                             MOD-KDFAKTYP-NEXT                            
189700                             MOD-IDORDNR7-ENTER                           
189800                             MOD-IDORDNR7-NEXT                            
189900                             MOD-IDPRODNR-ENTER                           
190000                             MOD-IDPRODNR-NEXT                            
190100                             MOD-IDKOLLI-ENTER                            
190200                             MOD-IDKOLLI-NEXT                             
190300                             MOD-IDDISTR-DOLD                             
190400                             MOD-FLTRPDOK                                 
190500                             MOD-FLLSTDOK                                 
190600                             MOD-FLPROFORMA                               
190700                                                                          
190800     MOVE +1              TO INDX                                         
190900     PERFORM  UNTIL INDX  > MAX-INDX                                      
191000       PERFORM MFS-RENSA-RAD-FAELT-UT                                     
191100       ADD +1             TO INDX                                         
191200     END-PERFORM                                                          
191300     .                                                                    
191400     SKIP3                                                                
191500 MFS-RENSA-RAD-FAELT-UT SECTION.                                          
191600                                                                          
191700*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
191800     MOVE MFS-RENSA-FAELT TO MOD-KVKOLLI-TOT                              
191900                             MOD-SUORDV-TOT                               
192000                             MOD-VKORDBTO-TOT                             
192100                             MOD-VLORDBTO-TOT                             
192200                             MOD-IDDISTR  (INDX)                          
192300                             MOD-IDKUNDNR (INDX)                          
192400                             MOD-KDFAKTYP (INDX)                          
192500                             MOD-IDORDNR7 (INDX)                          
192600                             MOD-IDKOLLI  (INDX)                          
192700                             MOD-TIRFS    (INDX)                          
192800                             MOD-KDKOLLI  (INDX)                          
192900                             MOD-VKORDBTO (INDX)                          
193000                             MOD-VLORDBTO (INDX)                          
193100                             MOD-IDPSN    (INDX)                          
193200                             MOD-IDPRODNR (INDX)                          
193300     .                                                                    
193400     SKIP3                                                                
193500 MFS-RENSA-FAELT-IN SECTION.                                              
193600                                                                          
193700*    --- ALLA INDATA-FÄLT                                                 
193800     MOVE MFS-RENSA-FAELT   TO MOD-FLAVSLUTA                              
193900                               MOD-FLTRPDOK                               
194000                               MOD-FLLSTDOK                               
194100                               MOD-FLPROFORMA                             
194200     MOVE +1                TO INDX                                       
194300     PERFORM  UNTIL INDX    > MAX-INDX                                    
194400       MOVE MFS-RENSA-FAELT TO MOD-FLBACKA-RAD(INDX)                      
194500       ADD +1               TO INDX                                       
194600     END-PERFORM                                                          
194700     .                                                                    
194800     EJECT                                                                
194900 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
195000                                                                          
195100*    --- ALLA UTDATA-FÄLT                                                 
195200*    --- INKL BLÄDDRINGSNYCKLAR OCH RAD-DATA                              
195300     MOVE MFS-ROER-EJ-FAELT TO MOD-KVKOLLI-TOT                            
195400                               MOD-SUORDV-TOT                             
195500                               MOD-VKORDBTO-TOT                           
195600                               MOD-VLORDBTO-TOT                           
195700                               MOD-IDDISTR-ENTER                          
195800                               MOD-IDDISTR-NEXT                           
195900                               MOD-IDKUNDNR-ENTER                         
196000                               MOD-IDKUNDNR-NEXT                          
196100                               MOD-KDFAKTYP-ENTER                         
196200                               MOD-KDFAKTYP-NEXT                          
196300                               MOD-IDORDNR7-ENTER                         
196400                               MOD-IDORDNR7-NEXT                          
196500                               MOD-IDPRODNR-ENTER                         
196600                               MOD-IDPRODNR-NEXT                          
196700                               MOD-IDKOLLI-ENTER                          
196800                               MOD-IDKOLLI-NEXT                           
196900                               MOD-IDDISTR-DOLD                           
197000                               MOD-TETRPDOK                               
197100                               MOD-FLTRPDOK                               
197200                               MOD-TELSTDOK                               
197300                               MOD-FLLSTDOK                               
197400                               MOD-FLPROFORMA                             
197500                               MOD-TEPROFORMA                             
197600                                                                          
197700     MOVE +1                TO INDX                                       
197800     PERFORM UNTIL INDX     > MAX-INDX                                    
197900       PERFORM MFS-ROER-EJ-RAD-FAELT-UT                                   
198000       ADD +1               TO INDX                                       
198100     END-PERFORM                                                          
198200     .                                                                    
198300     SKIP2                                                                
198400 MFS-ROER-EJ-RAD-FAELT-UT  SECTION.                                       
198500                                                                          
198600*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
198700                                                                          
198800     MOVE MFS-ROER-EJ-FAELT TO MOD-IDDISTR  (INDX)                        
198900                               MOD-IDKUNDNR (INDX)                        
199000                               MOD-KDFAKTYP (INDX)                        
199100                               MOD-IDORDNR7 (INDX)                        
199200                               MOD-IDKOLLI  (INDX)                        
199300                               MOD-TIRFS    (INDX)                        
199400                               MOD-KDKOLLI  (INDX)                        
199500                               MOD-VKORDBTO (INDX)                        
199600                               MOD-VLORDBTO (INDX)                        
199700                               MOD-IDPSN    (INDX)                        
199800                               MOD-IDPRODNR (INDX)                        
199900     .                                                                    
200000     SKIP3                                                                
200100 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
200200                                                                          
200300*    --- ALLA INDATA-FÄLT                                                 
200400     MOVE MFS-ROER-EJ-FAELT   TO MOD-FLAVSLUTA                            
200500                                 MOD-FLTRPDOK                             
200600                                 MOD-FLLSTDOK                             
200700                                 MOD-FLPROFORMA                           
200800     MOVE +1                  TO INDX                                     
200900     PERFORM  UNTIL INDX      > MAX-INDX                                  
201000       MOVE MFS-ROER-EJ-FAELT TO MOD-FLBACKA-RAD(INDX)                    
201100       ADD +1                 TO INDX                                     
201200     END-PERFORM                                                          
201300     .                                                                    
201400     EJECT                                                                
201500 MFS-FORM-ATTR SECTION.                                                   
201600                                                                          
201700*    --- ALLA INDATA-FÄLT                                                 
201800     MOVE MFS-FORMATETS-ATTR    TO MOD-FLAVSLUTA-ATTR                     
201900                                   MOD-FLTRPDOK-ATTR                      
202000                                   MOD-FLLSTDOK-ATTR                      
202100                                   MOD-FLPROFORMA-ATTR                    
202200     MOVE +1                    TO INDX                                   
202300     PERFORM  UNTIL INDX        > MAX-INDX                                
202400       MOVE MFS-FORMATETS-ATTR  TO MOD-FLBACKA-RAD-ATTR(INDX)             
202500       ADD +1                   TO INDX                                   
202600     END-PERFORM                                                          
202700     .                                                                    
202800     EJECT                                                                
202900* --- IMS SEKTIONER ---                                                   
203000     SKIP3                                                                
203100 IMS-GET-MSG SECTION.                                                     
203200                                                                          
203300     MOVE '  QC' TO GODK-STATUSKODER                                      
203400     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
203500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
203600     PERFORM IMS-STATUSKONTROLL                                           
203700     .                                                                    
203800     SKIP3                                                                
203900 IMS-INSERT-MSG SECTION.                                                  
204000                                                                          
204100     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
204200       MOVE '0' TO MFS-KDHUVOMR                                           
204300     END-IF                                                               
204400     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
204500     MOVE SPACE TO GODK-STATUSKODER                                       
204600     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
204700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
204800     PERFORM IMS-STATUSKONTROLL                                           
204900     .                                                                    
205000     EJECT                                                                
205100 IMS-ISRT-ALT-MSG-4664 SECTION.                                           
205200                                                                          
205300     MOVE SPACE TO GODK-STATUSKODER                                       
205400     CALL CBLTDLI USING ISRT ALT-PCB P-TO-P-SW                            
205500     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
205600     PERFORM IMS-STATUSKONTROLL                                           
205700     .                                                                    
205800     SKIP2                                                                
205900 IMS-ISRT-ALT-MSG-4698 SECTION.                                           
206000                                                                          
206100     MOVE SPACE TO GODK-STATUSKODER                                       
206200     CALL CBLTDLI USING ISRT ALT3-PCB P-TO-P-SW                           
206300     MOVE ALT3-STATUS-CODE TO STATUS-WS                                   
206400     PERFORM IMS-STATUSKONTROLL                                           
206500     .                                                                    
206600     SKIP2                                                                
206700 IMS-ISRT-ALT-MSG-4675 SECTION.                                           
206800                                                                          
206900     MOVE SPACE TO GODK-STATUSKODER                                       
207000     CALL CBLTDLI USING ISRT ALT4-PCB P-TO-P-SW                           
207100     MOVE ALT4-STATUS-CODE TO STATUS-WS                                   
207200     PERFORM IMS-STATUSKONTROLL                                           
207300     .                                                                    
207400     SKIP2                                                                
207500 IMS-ISRT-ALT-MSG-4675X SECTION.                                          
207600                                                                          
207700     MOVE SPACE TO GODK-STATUSKODER                                       
207800     CALL CBLTDLI USING ISRT ALT5-PCB P-TO-P-SW                           
207900     MOVE ALT5-STATUS-CODE TO STATUS-WS                                   
208000     PERFORM IMS-STATUSKONTROLL                                           
208100     .                                                                    
208200     SKIP2                                                                
208300 IMS-ISRT-ALT-MSG-4677 SECTION.                                           
208400                                                                          
208500     MOVE SPACE TO GODK-STATUSKODER                                       
208600     CALL CBLTDLI USING ISRT ALT6-PCB P-TO-P-SW                           
208700     MOVE ALT6-STATUS-CODE TO STATUS-WS                                   
208800     PERFORM IMS-STATUSKONTROLL                                           
208900     .                                                                    
209000     SKIP2                                                                
209100 IMS-ISRT-ALT-MSG-4677X SECTION.                                          
209200                                                                          
209300     MOVE SPACE TO GODK-STATUSKODER                                       
209400     CALL CBLTDLI USING ISRT ALT7-PCB P-TO-P-SW                           
209500     MOVE ALT7-STATUS-CODE TO STATUS-WS                                   
209600     PERFORM IMS-STATUSKONTROLL                                           
209700     .                                                                    
209800     SKIP2                                                                
209900 IMS-ISRT-ALT-MSG-4639X  SECTION.                                         
210000                                                                          
210100     MOVE SPACE TO GODK-STATUSKODER                                       
210200     CALL CBLTDLI USING ISRT ALT8-PCB P-TO-P-SW                           
210300     MOVE ALT8-STATUS-CODE TO STATUS-WS                                   
210400     PERFORM IMS-STATUSKONTROLL                                           
210500     .                                                                    
210600     SKIP2                                                                
210700 IMS-GHU-4495  SECTION.                                                   
210800     STRING 'WDR401  (WDGXKEY  =' W-4495-X ')'                            
210900                      DELIMITED BY SIZE INTO SSA1                         
211000     MOVE '  GE' TO GODK-STATUSKODER                                      
211100     CALL CBLTDLI USING GHU 4495-PCB DLI-IO-4495 SSA1                     
211200     MOVE 4495-STATUS-CODE TO STATUS-WS                                   
211300     PERFORM IMS-STATUSKONTROLL                                           
211400     .                                                                    
211500     SKIP2                                                                
211600 IMS-DLET-4495  SECTION.                                                  
211700     MOVE '    ' TO GODK-STATUSKODER                                      
211800     CALL CBLTDLI USING DLET 4495-PCB DLI-IO-4495                         
211900     MOVE 4495-STATUS-CODE TO STATUS-WS                                   
212000     PERFORM IMS-STATUSKONTROLL                                           
212100     .                                                                    
212200     SKIP2                                                                
212300 IMS-GHU-WDGX4496 SECTION.                                                
212400     STRING 'WDR401  *P(WDGXKEY  =' W-4495-X ')'                          
212500                      DELIMITED BY SIZE INTO SSA1                         
212600     MOVE  'WDGX4496 ' TO SSA2                                            
212700     MOVE '  GE' TO GODK-STATUSKODER                                      
212800     CALL CBLTDLI USING GHU 4495-PCB DLI-IO-4496 SSA1 SSA2                
212900     MOVE 4495-STATUS-CODE TO STATUS-WS                                   
213000     PERFORM IMS-STATUSKONTROLL                                           
213100     .                                                                    
213200     SKIP2                                                                
213300 IMS-REPL-WDGX4496 SECTION.                                               
213400     MOVE '  ' TO GODK-STATUSKODER                                        
213500     CALL CBLTDLI USING REPL 4495-PCB DLI-IO-4496                         
213600     MOVE 4495-STATUS-CODE TO STATUS-WS                                   
213700     PERFORM IMS-STATUSKONTROLL                                           
213800     .                                                                    
213900     SKIP2                                                                
214000 IMS-GHNP-WDGX4498-FIRST SECTION.                                         
214100     STRING 'WDGX4498(WDGXKEY >=' W-4498-X ')'                            
214200                      DELIMITED BY SIZE INTO SSA1                         
214300     MOVE '  GE' TO GODK-STATUSKODER                                      
214400     CALL CBLTDLI USING GHNP 4495-PCB DLI-IO-4498 SSA1                    
214500     MOVE 4495-STATUS-CODE TO STATUS-WS                                   
214600     PERFORM IMS-STATUSKONTROLL                                           
214700     .                                                                    
214800     SKIP2                                                                
214900 IMS-GHNP-WDGX4498-KVAL SECTION.                                          
215000     STRING 'WDGX4498(WDGXKEY >=' W-4498-X ')'                            
215100                      DELIMITED BY SIZE INTO SSA1                         
215200     MOVE '  GE' TO GODK-STATUSKODER                                      
215300     CALL CBLTDLI USING GHNP 4495-PCB DLI-IO-4498 SSA1                    
215400     MOVE 4495-STATUS-CODE TO STATUS-WS                                   
215500     PERFORM IMS-STATUSKONTROLL                                           
215600     .                                                                    
215700     SKIP2                                                                
215800 IMS-GHNP-WDGX4498-SKOLLI SECTION.                                        
215900     STRING 'WDGX4498(IDKOLLIS =' W-IDKOLLIS-X ')'                        
216000                      DELIMITED BY SIZE INTO SSA1                         
216100     MOVE '  GE' TO GODK-STATUSKODER                                      
216200     CALL CBLTDLI USING GHNP 4495-PCB DLI-IO-4498 SSA1                    
216300     MOVE 4495-STATUS-CODE TO STATUS-WS                                   
216400     PERFORM IMS-STATUSKONTROLL                                           
216500     .                                                                    
216600     SKIP2                                                                
216700 IMS-GHNP-WDGX4498      SECTION.                                          
216800     MOVE 'WDGX4498 ' TO SSA1                                             
216900     MOVE '  GE' TO GODK-STATUSKODER                                      
217000     CALL CBLTDLI USING GHNP 4495-PCB DLI-IO-4498 SSA1                    
217100     MOVE 4495-STATUS-CODE TO STATUS-WS                                   
217200     PERFORM IMS-STATUSKONTROLL                                           
217300     .                                                                    
217400     SKIP2                                                                
217500 IMS-GHU-WDGX4498 SECTION.                                                
217600     STRING 'WDR401  (WDGXKEY  =' W-4495-X ')'                            
217700                      DELIMITED BY SIZE INTO SSA1                         
217800     STRING 'WDGX4498(WDGXKEY  =' W-4498-X ')'                            
217900                      DELIMITED BY SIZE INTO SSA2                         
218000     MOVE '  GE' TO GODK-STATUSKODER                                      
218100     CALL CBLTDLI USING GHU 4495-PCB DLI-IO-4498 SSA1 SSA2                
218200     MOVE 4495-STATUS-CODE TO STATUS-WS                                   
218300     PERFORM IMS-STATUSKONTROLL                                           
218400     .                                                                    
218500     SKIP2                                                                
218600 IMS-GU-WDGX4498-OKVAL SECTION.                                           
218700     STRING 'WDR401  (WDGXKEY  =' W-4495-X ')'                            
218800                      DELIMITED BY SIZE INTO SSA1                         
218900     MOVE   'WDGX4498'       TO SSA2                                      
219000     MOVE '  GE' TO GODK-STATUSKODER                                      
219100     CALL CBLTDLI USING GU 4495-PCB DLI-IO-4498 SSA1 SSA2                 
219200     MOVE 4495-STATUS-CODE TO STATUS-WS                                   
219300     PERFORM IMS-STATUSKONTROLL                                           
219400     .                                                                    
219500     SKIP2                                                                
219600 IMS-DLET-WDGX4498 SECTION.                                               
219700                                                                          
219800     MOVE '  ' TO GODK-STATUSKODER                                        
219900     CALL CBLTDLI USING DLET 4495-PCB DLI-IO-4498                         
220000     MOVE 4495-STATUS-CODE TO STATUS-WS                                   
220100     PERFORM IMS-STATUSKONTROLL                                           
220200     .                                                                    
220300     SKIP2                                                                
220400 IMS-GNP-WDGX4497      SECTION.                                           
220500     MOVE 'WDGX4497 ' TO SSA1                                             
220600     MOVE '  GE' TO GODK-STATUSKODER                                      
220700     CALL CBLTDLI USING GNP 4495-PCB DLI-IO-4497 SSA1                     
220800     MOVE 4495-STATUS-CODE TO STATUS-WS                                   
220900     PERFORM IMS-STATUSKONTROLL                                           
221000     .                                                                    
221100     SKIP2                                                                
221200 IMS-GNP-WDGX4497-KVAL SECTION.                                           
221300     STRING 'WDGX4497(IDKOLLIS>=' W-IDKOLLIS-X ')'                        
221400                      DELIMITED BY SIZE INTO SSA1                         
221500     MOVE '    ' TO GODK-STATUSKODER                                      
221600     CALL CBLTDLI USING GNP 4495-PCB DLI-IO-4497 SSA1                     
221700     MOVE 4495-STATUS-CODE TO STATUS-WS                                   
221800     PERFORM IMS-STATUSKONTROLL                                           
221900     .                                                                    
222000     SKIP2                                                                
222100 IMS-GHU-WDGX4497 SECTION.                                                
222200     STRING 'WDGX4497(IDKOLLIS =' W-IDKOLLIS-X ')'                        
222300                      DELIMITED BY SIZE INTO SSA1                         
222400     MOVE '  GE' TO GODK-STATUSKODER                                      
222500     CALL CBLTDLI USING GHU 4495-PCB DLI-IO-4497 SSA1                     
222600     MOVE 4495-STATUS-CODE TO STATUS-WS                                   
222700     PERFORM IMS-STATUSKONTROLL                                           
222800     .                                                                    
222900     SKIP2                                                                
223000 IMS-DLET-WDGX4497 SECTION.                                               
223100                                                                          
223200     MOVE '  ' TO GODK-STATUSKODER                                        
223300     CALL CBLTDLI USING DLET 4495-PCB DLI-IO-4497                         
223400     MOVE 4495-STATUS-CODE TO STATUS-WS                                   
223500     PERFORM IMS-STATUSKONTROLL                                           
223600     .                                                                    
223700     SKIP2                                                                
223800 IMS-GHU-WDE601 SECTION.                                                  
223900                                                                          
224000     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-X ')'                        
224100            DELIMITED BY SIZE INTO SSA1                                   
224200     MOVE '  ' TO GODK-STATUSKODER                                        
224300     CALL CBLTDLI USING GHU WDE6-PCB DLI-IO-E601 SSA1                     
224400     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
224500     PERFORM IMS-STATUSKONTROLL                                           
224600     .                                                                    
224700     EJECT                                                                
224710 IMS-GU-WDE611 SECTION.                                                   
224720                                                                          
224730     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-X ')'                        
224740            DELIMITED BY SIZE INTO SSA1                                   
224750     STRING 'WDE611  (IDKOLLI  =' W-IDKOLLI-X ')'                         
224760            DELIMITED BY SIZE INTO SSA2                                   
224770     MOVE '  ' TO GODK-STATUSKODER                                        
224780     CALL CBLTDLI USING GU WDE6-PCB DLI-IO-E611 SSA1 SSA2                 
224790     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
224791     PERFORM IMS-STATUSKONTROLL                                           
224792     .                                                                    
224793     EJECT                                                                
224800 IMS-GHU-WDE611 SECTION.                                                  
224900                                                                          
225000     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-X ')'                        
225100            DELIMITED BY SIZE INTO SSA1                                   
225200     STRING 'WDE611  (IDKOLLI  =' W-IDKOLLI-X ')'                         
225300            DELIMITED BY SIZE INTO SSA2                                   
225400     MOVE '  ' TO GODK-STATUSKODER                                        
225500     CALL CBLTDLI USING GHU WDE6-PCB DLI-IO-E611 SSA1 SSA2                
225600     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
225700     PERFORM IMS-STATUSKONTROLL                                           
225800     .                                                                    
225900     EJECT                                                                
226000 IMS-REPL-WDE601 SECTION.                                                 
226100                                                                          
226200     MOVE '  ' TO GODK-STATUSKODER                                        
226300     CALL CBLTDLI USING REPL WDE6-PCB DLI-IO-E601                         
226400     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
226500     PERFORM IMS-STATUSKONTROLL                                           
226600     .                                                                    
226700                                                                          
226800 IMS-REPL-WDE611 SECTION.                                                 
226900                                                                          
227000     MOVE '  ' TO GODK-STATUSKODER                                        
227100     CALL CBLTDLI USING REPL WDE6-PCB DLI-IO-E611                         
227200     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
227300     PERFORM IMS-STATUSKONTROLL                                           
227400     .                                                                    
227500     EJECT                                                                
227600 IMS-GHU-WDE711-ASEQ SECTION.                                             
227700                                                                          
227800     STRING 'WDE711  (WDE7ASEQ =' W-WDE7ASEQ-X ')'                        
227900            DELIMITED BY SIZE INTO SSA1                                   
228000     MOVE '  ' TO GODK-STATUSKODER                                        
228100     CALL CBLTDLI USING GHU WDE7-PCB DLI-IO-E711 SSA1                     
228200     MOVE WDE7-STATUS-CODE TO STATUS-WS                                   
228300                                                                          
228400     PERFORM IMS-STATUSKONTROLL                                           
228500     .                                                                    
228600     SKIP2                                                                
228700 IMS-REPL-WDE711 SECTION.                                                 
228800                                                                          
228900     MOVE '  ' TO GODK-STATUSKODER                                        
229000     CALL CBLTDLI USING REPL WDE7-PCB DLI-IO-E711                         
229100     MOVE WDE7-STATUS-CODE TO STATUS-WS                                   
229200     PERFORM IMS-STATUSKONTROLL                                           
229300     .                                                                    
229400     SKIP2                                                                
229500 IMS-GU-WDE401-ASEK       SECTION.                                        
229600                                                                          
229700     STRING 'WDE401  (WDE4ASEQ =' W-WDE4ASEQ-X ')'                        
229800     DELIMITED BY SIZE INTO SSA1                                          
229900     MOVE '  GE' TO GODK-STATUSKODER                                      
230000     CALL CBLTDLI USING GU WDE4-PCB DLI-IO-E401 SSA1                      
230100     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
230200     PERFORM IMS-STATUSKONTROLL                                           
230300     .                                                                    
230400     SKIP3                                                                
230500 IMS-GN-WDE401-ASEK       SECTION.                                        
230600                                                                          
230700     STRING 'WDE401  (WDE4ASEQ =' W-WDE4ASEQ-X ')'                        
230800     DELIMITED BY SIZE INTO SSA1                                          
230900     MOVE '  GEGB' TO GODK-STATUSKODER                                    
231000     CALL CBLTDLI USING GN WDE4-PCB DLI-IO-E401 SSA1                      
231100     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
231200     PERFORM IMS-STATUSKONTROLL                                           
231300     .                                                                    
231400     EJECT                                                                
231500 IMS-GU-WDB201      SECTION.                                              
231600                                                                          
231700     STRING 'WDB201  (IDGMT   >=' W-IDGMT-X ')'                           
231800                      DELIMITED BY SIZE INTO SSA1                         
231900     MOVE '    ' TO GODK-STATUSKODER                                      
232000     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-B201 SSA1                      
232100     MOVE WDB2-STATUS-CODE TO STATUS-WS                                   
232200     PERFORM IMS-STATUSKONTROLL                                           
232300     .                                                                    
232400     SKIP3                                                                
232500 IMS-ISRT-4479 SECTION.                                                   
232600                                                                          
232700     MOVE SPACE TO SSA1 SSA2                                              
232800                                                                          
232900     MOVE 'WDR401  ' TO SSA1                                              
233000     MOVE '  II' TO GODK-STATUSKODER                                      
233100     CALL CBLTDLI USING ISRT 4479-PCB DLI-IO-4479 SSA1                    
233200     MOVE 4479-STATUS-CODE TO STATUS-WS                                   
233300     PERFORM IMS-STATUSKONTROLL                                           
233400     .                                                                    
233500     EJECT                                                                
233600 IMS-ISRT-WDGX4480 SECTION.                                               
233700                                                                          
233800     MOVE SPACE TO SSA1 SSA2                                              
233900                                                                          
234000     STRING 'WDR401  (WDGXKEY  =' W-4479-X ')'                            
234100                      DELIMITED BY SIZE INTO SSA1                         
234200     MOVE 'WDGX4480' TO SSA2                                              
234300     MOVE '  II' TO GODK-STATUSKODER                                      
234400     CALL CBLTDLI USING ISRT 4479-PCB DLI-IO-4480 SSA1 SSA2               
234500     MOVE 4479-STATUS-CODE TO STATUS-WS                                   
234600     PERFORM IMS-STATUSKONTROLL                                           
234700     .                                                                    
234800     EJECT                                                                
234900 IMS-ISRT-WDGX4482 SECTION.                                               
235000                                                                          
235100     STRING 'WDR401  (WDGXKEY  =' W-4479-X ')'                            
235200                      DELIMITED BY SIZE INTO SSA1                         
235300     MOVE 'WDGX4482' TO SSA2                                              
235400     MOVE '  II' TO GODK-STATUSKODER                                      
235500     CALL CBLTDLI USING ISRT 4479-PCB DLI-IO-4482 SSA1 SSA2               
235600     MOVE 4479-STATUS-CODE TO STATUS-WS                                   
235700     PERFORM IMS-STATUSKONTROLL                                           
235800     .                                                                    
235900     EJECT                                                                
236000 IMS-GU-WDB601    SECTION.                                                
236100     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
236200          DELIMITED BY SIZE INTO SSA1                                     
236300     MOVE '  GE' TO GODK-STATUSKODER                                      
236400     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
236500     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
236600     PERFORM IMS-STATUSKONTROLL                                           
236700     IF SEGMENT-SAKNAS                                                    
236800        MOVE SPACE TO DCS-KDDC                                            
236900     END-IF                                                               
237000     .                                                                    
237100     EJECT                                                                
237200 IMS-GU-WDB101 SECTION.                                                   
237300                                                                          
237400     STRING 'WDB101  (WDB101KY =' W-WDB101KY-X ')'                        
237500          DELIMITED BY SIZE INTO SSA1                                     
237600     MOVE '  ' TO GODK-STATUSKODER                                        
237700     CALL CBLTDLI USING GU WDB1-PCB DLI-IO-WDB101 SSA1                    
237800     MOVE WDB1-STATUS-CODE TO STATUS-WS                                   
237900     PERFORM IMS-STATUSKONTROLL                                           
238000     .                                                                    
238100     EJECT                                                                
238200 IMS-STATUSKONTROLL SECTION.                                              
238300                                                                          
238400     SET STATUS-IX TO 1                                                   
238500     SEARCH GODK-STATUS                                                   
238600       AT END                                                             
238700         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
238800         DELIMITED BY SIZE INTO FELTEXT                                   
238900         CALL FELLOG                                                      
239000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
239100         CONTINUE                                                         
239200     END-SEARCH                                                           
239300     .                                                                    
