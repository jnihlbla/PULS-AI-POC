000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     WL018700.                                                
000300 AUTHOR.         SUBBARAO PARUCHURI V.                                    
000400 DATE-WRITTEN.   04/11/22.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    NAME:       'CARPARTS.LDC.CLOSETRANSPORT'                            
000800*                                                                         
000900*    FUNCTION:                                                            
001000*        DE KOLLIN PÅ EN VISS TRANSPORT SOM ÄR VALDA FÖR EN               
001100*        FAKT/LAST VISAS.                                                 
001200*        GENOM ATT AVSLUTA STARTAS FAKTURA RELEASEN W40665,               
001300*        ELLER W40675 FÖR BILLIT-FAKT. RESP.                              
001400*        W40676 FÖR BILLIT-FAKT-PROFORMA.                                 
001500*        VALDA KOLLIN KAN BACKAS SÅ ATT DE EJ LIGGER UNDER DENNA          
001600*        TRANSPORT/LASTBÄRARE SOM LASTNINGSRELEASADE LÄNGRE.              
001700*        OBS! UTSKRIFT AV LASTLISTA STARTAS VIA PF4-TANGENT SOM           
001800*        STARTAR TRANS/PGM 4699.                                          
001900*        PROGRAMMET UPPDATERAR WDE6                                       
002000*        PROGRAMMET UPPDATERAR 4495 (WDR4)                                
002100*                                                                         
002200*        WL018700 PROGRAM IS A REPLICA OF W4066400 PROGRAM                
002300*        AND CUSTOMIZED FOR WEB-LDC REQUIREMENTS                          
002400*                                                                         
002500*    INDATA.                                                              
002600*        TRANSACTION: WL0187T                                             
002700*        REQUEST:     WL0187I1                                            
002800*                                                                         
002900*    OUTDATA.                                                             
003000*        RESPONSE:    WL0187O1                                            
003100                                                                          
003200     SKIP3                                                                
003300 ENVIRONMENT DIVISION.                                                    
003400     SKIP2                                                                
003500 INPUT-OUTPUT SECTION.                                                    
003600                                                                          
003700 FILE-CONTROL.                                                            
003800     EJECT                                                                
003900 DATA DIVISION.                                                           
004000     SKIP3                                                                
004100 FILE SECTION.                                                            
004200     EJECT                                                                
004300 WORKING-STORAGE SECTION.                                                 
004400 77  IDPGM                       PIC X(08)   VALUE 'WL018700'.            
004500                                                                          
004600*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
004700 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
004800 77  KDRC-DISPLAY                PIC Z(5).                                
004900                                                                          
005000*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
005100 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
005200                                                                          
005300 77  JA                          PIC X       VALUE 'J'.                   
005400 77  YES                         PIC X       VALUE 'Y'.                   
005500 77  NEJ                         PIC X       VALUE 'N'.                   
005600 77  MSG-IX                      PIC S9(9)  VALUE +0   COMP SYNC.         
005700                                                                          
005800 77  CURR-SECTION                PIC X(24)  VALUE 'MAIN'.                 
005900 77  CURR-IMS-SECTION            PIC X(24)  VALUE SPACE.                  
006000                                                                          
006100 77  W-SPAR-IDKUNDNR             PIC S9(7)   VALUE ZERO COMP-3.           
006200*    --- INDEX FÖR BLÄDDRINGSRADER                                        
006300 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
006400 77  WS-COUNT                    PIC S9(4)  VALUE +0    COMP SYNC.        
006500 77  MAX-INDX                    PIC S9(4)  VALUE +500  COMP SYNC.        
006600                                                                          
006700*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
006800 77  WS-IDTRPTNR                 PIC X(3)    VALUE SPACE.                 
006900 77  WS-IDLBBET                  PIC X(12)   VALUE SPACE.                 
007000 77  WS-FLFARLIG                 PIC X(1)    VALUE SPACE.                 
007100                                                                          
007200 77  WS-ADRESS-WL0188            PIC X(50)                                
007300       VALUE 'CARPARTS.LDC.TRANSPSUPPL1BG'.                               
007400 77  WS-ADRESS-W40698            PIC X(50)                                
007500       VALUE 'CARPARTS.PULS.BOLLAPRINT'.                                  
007600 77  INDATA-SW                   PIC X       VALUE 'J'.                   
007700     88  INDATA-OK                           VALUE 'J'.                   
007800     88  INDATA-FEL                          VALUE 'N'.                   
007900                                                                          
008000 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
008100     88  NYCKLAR-OK                          VALUE 'J'.                   
008200     88  NYCKLAR-FEL                         VALUE 'N'.                   
008300                                                                          
008400 77  ITALIEN-SW                  PIC X       VALUE ' '.                   
008500     88  ITALIEN-JA                          VALUE 'J'.                   
008600     88  ITALIEN-NEJ                         VALUE 'N'.                   
008700                                                                          
008800 77  DELETE-ARBREG-ROT-SW        PIC X       VALUE 'N'.                   
008900     88  ARBREG-ROT-BORTTAGEN                VALUE 'J'.                   
009000     EJECT                                                                
009100 01  FILLER                  PIC X(16) VALUE 'SWITCHAR'.                  
009200 77  W-FUNKTION              PIC S9(1) COMP-3 VALUE +0.                   
009300     88  AVSLUTA                              VALUE +1.                   
009400     88  BACKA-VALDA-RADER                    VALUE +2.                   
009500     88  PROFORMA                             VALUE +4.                   
009600     88  LASTDOK                              VALUE +5.                   
009700                                                                          
009800 77  SW-RAD-INPUT            PIC  X(1)        VALUE 'N'.                  
009900 77  SW-NOT-WLA187           PIC  X(1)        VALUE 'J'.                  
010000                                                                          
010100 77  SW-FLTRANS              PIC  X(1)        VALUE 'N'.                  
010200     88 FL-XTRANS                             VALUE 'J'.                  
010300                                                                          
010400 77  TRAFF-SW                PIC  X(1)        VALUE 'N'.                  
010500     88  TRAFF                                VALUE 'J'.                  
010600                                                                          
010700 01  FILLER                  PIC X(16) VALUE 'ARBETSFALT'.                
010800 77  W-IDDISTR-NUM           PIC  9(4)   VALUE ZERO.                      
010900 77  W-IDDISTR-ALFA          PIC  X(4)   VALUE SPACE.                     
011000 77  W-IDKUNDNR-NUM          PIC  9(6)   VALUE ZERO.                      
011100 77  W-REG-IDPRODNR              PIC S9(7)   VALUE ZERO COMP-3.           
011200 77  W-SPAR-IDKOLLI-SAMP         PIC S9(5)   VALUE ZERO COMP-3.           
011300 77  W-KVKOLLI                   PIC S9(3)   VALUE ZERO COMP-3.           
011400 77  W-VLORDBTO            PIC S9(4)V9(3)    VALUE ZERO COMP-3.           
011500 77  W-VKORDBTO            PIC S9(6)V9(1)    VALUE ZERO COMP-3.           
011600 77  W-IDPSN-NUM           PIC  9(3)         VALUE ZERO.                  
011700 77  W-SPAR-VLORDBTO       PIC S9(4)V9(3)    VALUE ZERO COMP-3.           
011800 77  W-SPAR-VKORDBTO       PIC S9(6)V9(1)    VALUE ZERO COMP-3.           
011900 77  W-SPAR-SUORDV         PIC S9(9)V9(2)    VALUE ZERO COMP-3.           
012000 77  W-SPAR-SUORDV-LOC     PIC S9(9)V9(2)    VALUE ZERO COMP-3.           
012100 77  W-SPAR-SUORDV-LOCPREL PIC S9(9)V9(2)    VALUE ZERO COMP-3.           
012200 77  WS-VLORDBTO             PIC S9(4)V9(3)  VALUE ZERO COMP-3.           
012300 77  WS-VKORDBTO             PIC S9(6)V9(1)  VALUE ZERO COMP-3.           
012400 77  RKOD-ABEND-MED-DUMP     PIC S9(4)   VALUE +33 COMP SYNC.             
012500                                                                          
012600 77  WS-IDELMT-ERROR             PIC X(16).                               
012700 77  WS-IDMSG-ERROR              PIC X(03).                               
012800 77  WS-IDMSG-INFO               PIC X(03).                               
012900                                                                          
013000 77  WS-REC-LIMIT                PIC X       VALUE 'N'.                   
013100     88  REC-LIMIT                           VALUE 'J'.                   
013200                                                                          
013300 77  WS-INDX-REC                 PIC S9(4)  VALUE +0    COMP SYNC.        
013400                                                                          
013500                                                                          
013600 01  FILLER                  PIC X(16) VALUE 'KONSTANTER'.                
013700 01  KONSTANTER.                                                          
013800     03  KLI-PACK            PIC S9(1) COMP-3 VALUE +1.                   
013900     03  KLI-PACK-FAKT       PIC S9(1) COMP-3 VALUE +6.                   
014000     03  W-AVSLUTA           PIC S9(1) COMP-3 VALUE +1.                   
014100     03  W-BACKA-VALDA-RADER PIC S9(1) COMP-3 VALUE +2.                   
014200     03  W-PROFORMA          PIC S9(1) COMP-3 VALUE +4.                   
014300     03  W-VALD              PIC  X(1)        VALUE 'X'.                  
014400     EJECT                                                                
014500*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
014600 01  GENERAL-SUBPROGRAMS.                                                 
014700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
014800     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
014900     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
015000     03  WZ01AUTH                PIC X(8)    VALUE 'WZ01AUTH'.            
015100     03  WMSGCONV                PIC X(8)    VALUE 'WMSGCONV'.            
015200     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
015300     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
015400     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
015500     SKIP3                                                                
015600*    --- PARAMETERS TO ABEND                                              
015700                                                                          
015800 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
015900 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
016000 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
016100                                                                          
016200*                                                                         
016300 01  FILLER                      PIC X(16)   VALUE 'WMSGCONV'.            
016400*01  -COPY WMSGCONV                                                       
016500 01  FILLER                      PIC X(16)   VALUE 'WZ01AUTH'.            
016600*01  -COPY WZ01AUTH                                                       
016700 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
016800     SKIP3                                                                
016900*01  -COPY WZ01SUB                                                        
017000     EJECT                                                                
017100 01  FILLER                      PIC X(16)   VALUE 'SEND-CONTROL'.        
017200*01  -COPY WZ01SEND                                                       
017300     EJECT                                                                
017400*    - SEND AREA-2 FOR STARTING W40698 PROGRAM                            
017500 01  FILLER                      PIC X(16)   VALUE 'SEND-AREA-2'.         
017600 01  SEND-AREA-2.                                                         
017700*    03  -COPY WZ01REQU -PRE SEND2-                                       
017800*    03  -COPY W4I69801 -PRE SEND2-                                       
017900     EJECT                                                                
018000*    - SEND AREA-1 FOR STARTING WL0188 PROGRAM                            
018100 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
018200     SKIP3                                                                
018300 01  REQU-AREA.                                                           
018400*    03  -COPY WZ01REQ2                                                   
018500*    03  -COPY WL0187I1                                                   
018600     EJECT                                                                
018700 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
018800     SKIP3                                                                
018900 01  RESP-AREA.                                                           
019000*    03  -COPY WZ01RES2                                                   
019100*    03  -COPY WL0187O1                                                   
019200     EJECT                                                                
019300*    - SEND AREA-3 FOR STARTING WL0188 PROGRAM                            
019400 01  FILLER                      PIC X(16)   VALUE 'SEND-AREA-3'.         
019500 01  SEND-AREA-3.                                                         
019600*    03  -COPY WZ01REQU -PRE SEND3-                                       
019700*    03  -COPY WL0188I1 -PRE SEND3-                                       
019800     EJECT                                                                
019900*    --- PARAMETRAR TILL COPYTEXT   WWOMVAND                              
020000*01 -COPY WWOMVAND                                                        
020100     SKIP3                                                                
020200 01  MESSAGE-CODES.                                                       
020300     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '014'.                 
020400     03  ERR-WRONG-KEY           PIC X(3)    VALUE '043'.                 
020500     03  ERR-UNAUTHORIZED        PIC X(3)    VALUE '00A'.                 
020600     03  SUPPLEMENT-NEEDED       PIC X(3)    VALUE '215'.                 
020700     EJECT                                                                
020800 01  TEST-IDDISTR            PIC  9(5) COMP-3 VALUE ZERO.                 
020900*    ----DISTR-DEALER-/BILLIT-FAKT------                                  
021000*01  FILLER  -COPY WWDIST79 -RED TEST-IDDISTR.                            
021100     EJECT                                                                
021200*    ----WWDIST92-----------------------                                  
021300*01  FILLER  -COPY WWDIST92 -RED TEST-IDDISTR.                            
021400     EJECT                                                                
021500*    ----TABELL FÖR X-TRANS--------                                       
021600*01  FILLER  -COPY W476DIST.                                              
021700     EJECT                                                                
021800                                                                          
021900*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
022000 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
022100     SKIP3                                                                
022200*01  -COPY WMSGAREA                                                       
022300     EJECT                                                                
022400 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
022500     SKIP3                                                                
022600*01  -COPY WMFSAREA                                                       
022700     EJECT                                                                
022800*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
022900*                                                                         
023000     EJECT                                                                
023100                                                                          
023200 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
023300     SKIP3                                                                
023400 01  NYCKLAR-TILL-DLI.                                                    
023500     03  W-4495-X.                                                        
023600         05  W-IDHTR             PIC X(4)    VALUE '4495'.                
023700         05  W-IDDC-4495         PIC X(2)    VALUE SPACE.                 
023800         05  W-IDTRPTNR          PIC S9(3)   VALUE ZERO   COMP-3.         
023900         05  W-IDLBBET           PIC X(12)   VALUE SPACE.                 
024000         05  FILLER              PIC X(10)   VALUE LOW-VALUE.             
024100     03  W-4498-X.                                                        
024200         05  W-IDDISTR           PIC S9(5)   VALUE ZERO  COMP-3.          
024300         05  W-IDKUNDNR          PIC S9(7)   VALUE ZERO  COMP-3.          
024400         05  W-KDFAKTYP          PIC  X(1)   VALUE SPACE.                 
024500         05  W-IDKUNDRF          PIC  X(10).                              
024600         05  W-IDKUNDRF-IDORDNR-FILLER REDEFINES W-IDKUNDRF.              
024700           07  W-IDORDNR7        PIC  9(7).                               
024800           07  FILLER            PIC  X(3).                               
024900         05  W-IDPRODNR          PIC S9(7)   VALUE ZERO  COMP-3.          
025000         05  W-IDKOLLI           PIC S9(5)   VALUE ZERO  COMP-3.          
025100     03  W-IDDC-X.                                                        
025200         05  W-IDDC              PIC  X(2)   VALUE SPACE.                 
025300     03  W-IDPRODNR-X.                                                    
025400         05  W-IDPRODNR-KOLLI    PIC S9(7)   VALUE ZERO  COMP-3.          
025500     03  W-IDKOLLI-X.                                                     
025600         05  W-IDKOLLI-KOLLI     PIC S9(5)   VALUE ZERO  COMP-3.          
025610     03  W-IDDCCROSS-X.                                                   
025620         05  W-IDDCCROSS         PIC  X(2).                               
025700     03  W-WDE4ASEQ-X.                                                    
025800      05  W-4A1-IDDISTR          PIC S9(5)   VALUE ZERO  COMP-3.          
025900      05  W-4A1-IDKUNDNR         PIC S9(7)   VALUE ZERO  COMP-3.          
026000      05  W-4A1-IDKUNDRF.                                                 
026100       07  W-4A1-IDORDNR         PIC X(5).                                
026200       07  FILLER                PIC X(5).                                
026300     SKIP2                                                                
026400     03  W-IDGMT-X.                                                       
026500      05  W-IDDISTR-WDB2         PIC S9(5)   VALUE ZERO  COMP-3.          
026600      05  W-IDKUNDNR-WDB2        PIC S9(7)   VALUE ZERO  COMP-3.          
026700     SKIP2                                                                
026800     03  W-IDDC-B6-X.                                                     
026900         05 W-IDDC-B6            PIC X(2)    VALUE SPACE.                 
027000     SKIP2                                                                
027100     03  W-KDKOLSTX-X.                                                    
027200         05 W-KDKOLSTA-CROSS     PIC S9      VALUE 6     COMP-3.          
027300                                                                          
027400     03  W-TIRECXDAT-X.                                                   
027500         05 W-TIRECXDAT          PIC 9(6)    VALUE 0.                     
027600                                                                          
027700                                                                          
027800*    --- STATUS-KOD FRÅN IMS                                              
027900 01  STATUS-WS                   PIC XX.                                  
028000     88  SEGMENT-FINNS                       VALUE '  '.                  
028100     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
028200     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
028300     88  BASEN-SLUT                          VALUE 'GB'.                  
028400     SKIP2                                                                
028500 01  GODK-STATUSKODER.                                                    
028600     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
028700     SKIP3                                                                
028800 01  SSA1                        PIC X(128).                              
028900 01  SSA2                        PIC X(128).                              
028910 01  SSA3                        PIC X(128).                              
029000     EJECT                                                                
029100*    --- IMS FUNKTIONSKODER                                               
029200*01  -COPY W0003                                                          
029300     EJECT                                                                
029400*    ---  DLI INPUT-OUTPUT AREA                                           
029500 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
029600     SKIP3                                                                
029700 01  FILLER         PIC X(16) VALUE 'DLI-IO-E401'.                        
029800 01  DLI-IO-E401.                                                         
029900*    03  -COPY WDE401                                                     
030000     EJECT                                                                
030100 01  FILLER         PIC X(16) VALUE 'DLI-IO-E601'.                        
030200 01  DLI-IO-E601.                                                         
030300*    03  -COPY WDE601                                                     
030400     EJECT                                                                
030500 01  FILLER         PIC X(16) VALUE 'DLI-IO-E611'.                        
030600 01  DLI-IO-E611.                                                         
030700*    03  -COPY WDE611                                                     
030800     EJECT                                                                
030900 01  FILLER         PIC X(16) VALUE 'DLI-IO-E621'.                        
031000 01  DLI-IO-E621.                                                         
031100*    03  -COPY WDE621                                                     
031200     EJECT                                                                
031300 01  FILLER         PIC X(16) VALUE 'DLI-IO-4495'.                        
031400 01  DLI-IO-4495.                                                         
031500*    03  -COPY WDGX4495                                                   
031600     EJECT                                                                
031700 01  FILLER         PIC X(16) VALUE 'DLI-IO-4496'.                        
031800 01  DLI-IO-4496.                                                         
031900*    03  -COPY WDGX4496                                                   
032000     EJECT                                                                
032100 01  FILLER         PIC X(16) VALUE 'DLI-IO-4498'.                        
032200 01  DLI-IO-4498.                                                         
032300*    03  -COPY WDGX4498                                                   
032400     EJECT                                                                
032500 01  FILLER         PIC X(16) VALUE 'DLI-IO-B201'.                        
032600 01  DLI-IO-B201.                                                         
032700*    03  -COPY WDB201                                                     
032800                                                                          
032900 01  FILLER         PIC X(16) VALUE 'WDB601  AREA'.                       
033000 01  DLI-IO-AREA-B601.                                                    
033100*    03  -COPY WDB601                                                     
033200     EJECT                                                                
033300                                                                          
033400     EJECT                                                                
033500 LINKAGE SECTION.                                                         
033600 01  MSG-PCB                     PIC X.                                   
033700     EJECT                                                                
033800*01  -COPY W0009  -PRE ALT-                                               
033900     EJECT                                                                
034000*01  -COPY W0009  -PRE ALT2-                                              
034100     EJECT                                                                
034200*01  -COPY W0009  -PRE BOLL-                                              
034300     EJECT                                                                
034400 01  ATAB-PCB                 PIC X.                                      
034500*01  -COPY W0008  -PRE WDE6-                                              
034600     05  FILLER                  PIC X.                                   
034700     EJECT                                                                
034800*01  -COPY W0008  -PRE WDE4-                                              
034900     05  FILLER                  PIC X.                                   
035000     EJECT                                                                
035100*01  -COPY W0008  -PRE 4495-                                              
035200     05  FILLER                  PIC X.                                   
035300     EJECT                                                                
035400*01  -COPY W0008  -PRE WDB2-                                              
035500     05  FILLER                  PIC X.                                   
035600     EJECT                                                                
035700*01  -COPY W0008  -PRE WDB6-                                              
035800     05  FILLER                  PIC X.                                   
035900     EJECT                                                                
036000 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB ALT2-PCB BOLL-PCB              
036100                          ATAB-PCB WDE6-PCB WDE4-PCB                      
036200                          4495-PCB WDB2-PCB WDB6-PCB.                     
036300 MAIN SECTION.                                                            
036400     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB ALT2-PCB BOLL-PCB              
036500                          ATAB-PCB WDE6-PCB WDE4-PCB                      
036600                          4495-PCB WDB2-PCB WDB6-PCB.                     
036700                                                                          
036800     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
036900     IF SUB-KDRC = 0                                                      
037000       IF REQU-KDPGMACT = 'E' OR 'S' OR 'B'                               
037100         PERFORM A-INIT                                                   
037200         PERFORM B-KOLLA-NYCKLAR                                          
037300         IF NYCKLAR-OK                                                    
037400           IF REQU-KDPGMACT = 'E' OR 'B'                                  
037500             PERFORM G-KOLLA-INPUT                                        
037600             IF INDATA-OK                                                 
037700               PERFORM H-UPPDATERA                                        
037800             END-IF                                                       
037900           ELSE                                                           
038000             PERFORM E-SAMMA-SIDA                                         
038100           END-IF                                                         
038200           IF INDATA-OK AND SW-NOT-WLA187 = 'J'                           
038300             PERFORM F-LAES-VISA-INFO                                     
038400           END-IF                                                         
038500         END-IF                                                           
038600                                                                          
038700         IF (AVSLUTA OR PROFORMA) AND INDATA-OK                           
038800           MOVE '015'       TO RESP-IDMSG-INFO                            
038900           IF (SUB-KDTRANS(1:6) = 'WLA187' AND AVSLUTA)                   
039000             IF RESP-KDTRTYP = 'U'                                        
039100              MOVE '215'       TO RESP-IDMSG-INFO                         
039200             END-IF                                                       
039300           END-IF                                                         
039400           CONTINUE                                                       
039500         END-IF                                                           
039600       ELSE                                                               
039700         MOVE '099'            TO RESP-IDMSG-ERROR                        
039800       END-IF                                                             
039900                                                                          
040000       MOVE RESP-IDMSG-INFO    TO WS-IDMSG-INFO                           
040100       MOVE RESP-IDMSG-ERROR   TO WS-IDMSG-ERROR                          
040200       MOVE RESP-IDELMT-ERROR  TO WS-IDELMT-ERROR                         
040300       IF WS-IDMSG-ERROR NOT = SPACE                                      
040400          MOVE LOW-VALUES        TO RESP-IDDC-KEY                         
040500                                    RESP-IDTRPTNR-KEY(1:)                 
040600                                    RESP-IDLBBET-KEY                      
040700                                    RESP-KDFARLIG-KEY                     
040800          MOVE WS-IDMSG-ERROR   TO RESP-IDMSG-ERROR                       
040900          MOVE WS-IDELMT-ERROR  TO RESP-IDELMT-ERROR                      
041000          MOVE WS-IDMSG-INFO    TO RESP-IDMSG-INFO                        
041100          MOVE 001              TO RESP-IDRESVER                          
041200          IF  REQU-KDPGMACT = 'S'                                         
041300             MOVE ZERO             TO RESP-KVRADER-MAX                    
041400          ELSE                                                            
041500             IF REQU-KVRADER-MAX NUMERIC                                  
041600               MOVE REQU-KVRADER-MAX TO RESP-KVRADER-MAX                  
041700             ELSE                                                         
041800               MOVE ZERO             TO RESP-KVRADER-MAX                  
041900             END-IF                                                       
042000          END-IF                                                          
042100          IF SUB-KDTRANS(1:6) = 'WLA187'                                  
042200            PERFORM S11-MSG-CONV                                          
042300          END-IF                                                          
042400          PERFORM S02-RETURN-RESPONSE                                     
042500       ELSE                                                               
042600          IF REQU-KDPGMACT = 'S'                                          
042700            IF SUB-KDTRANS(1:6) = 'WLA187'                                
042800              PERFORM S11-MSG-CONV                                        
042900            END-IF                                                        
043000            PERFORM S02-RETURN-RESPONSE                                   
043100          ELSE                                                            
043200              IF ITALIEN-NEJ                                              
043300*               -- SEND A RESPONSE TO THE WAITING WEB PGM.                
043400                IF SUB-KDTRANS(1:6) = 'WLA187'                            
043500                  PERFORM S11-MSG-CONV                                    
043600                END-IF                                                    
043700                PERFORM S02-RETURN-RESPONSE                               
043800              ELSE                                                        
043900                IF ITALIEN-JA                                             
044000*                -- A BOLLA DOCUMENT WILL BE CREATED BY PGM 4698.         
044100*                -- DO NOT RETURN A RESPONSE TO THE WEB PGM HERE.         
044200*                -- INSTEAD A REPONSE IS SENT WHEN D&P                    
044300*                -- GETS THE DOCUMENT CREATED BY 4698                     
044400                  PERFORM HDB-STARTA-W4T698X                              
044500                ELSE                                                      
044600                  MOVE '041'                  TO RESP-IDMSG-ERROR         
044700                  MOVE 'IDTRP'                TO RESP-IDELMT-ERROR        
044800                  MOVE SPACE                  TO RESP-IDMSG-INFO          
044900                  IF SUB-KDTRANS(1:6) = 'WLA187'                          
045000                    PERFORM S11-MSG-CONV                                  
045100                  END-IF                                                  
045200                  PERFORM S02-RETURN-RESPONSE                             
045300                END-IF                                                    
045400              END-IF                                                      
045500          END-IF                                                          
045600       END-IF                                                             
045700     END-IF                                                               
045800     MOVE ZERO TO RETURN-CODE                                             
045900     GOBACK                                                               
046000     .                                                                    
046100     EJECT                                                                
046200 A-INIT SECTION.                                                          
046300                                                                          
046400     MOVE 'A-INIT                  ' TO CURR-SECTION                      
046500     MOVE LOW-VALUES    TO RESP-AREA                                      
046600     MOVE SPACE         TO RESP-IDMSG-ERROR                               
046700                           RESP-IDMSG-INFO                                
046800                           RESP-IDELMT-ERROR                              
046900     MOVE 001           TO RESP-IDRESVER                                  
047000     MOVE ZERO          TO RESP-KVRADER-MAX                               
047100     MOVE ZERO          TO WS-COUNT                                       
047200*    -- DEFAULT TRANSACTION TYPE FOR WL0188                               
047300*    -- (BACKGROUND, NO DIALOGUE)                                         
047400     MOVE 'X'           TO RESP-KDTRTYP                                   
047500     IF SUB-KDTRANS(1:6) = 'WLA187'                                       
047600       MOVE 001                  TO AUTH-KDCALL                           
047700       CALL WZ01AUTH          USING AUTH-WZ01AUTH                         
047800                                    REQU-WZ01REQ2                         
047900       IF AUTH-KDRC > 0                                                   
048000         MOVE ERR-UNAUTHORIZED   TO RESP-IDMSG-ERROR                      
048100         MOVE NEJ                TO NYCKLAR-SW                            
048200       END-IF                                                             
048300       MOVE FUNCTION UPPER-CASE (REQU-IDUSER) TO                          
048400                                 REQU-IDUSER                              
048500       MOVE FUNCTION UPPER-CASE (REQU-IDDC-KEY) TO                        
048600                                 REQU-IDDC-KEY                            
048700       IF REQU-KDFARLIG-KEY = LOW-VALUES                                  
048800         CONTINUE                                                         
048900       ELSE                                                               
049000        MOVE FUNCTION UPPER-CASE (REQU-KDFARLIG-KEY) TO                   
049100                                  REQU-KDFARLIG-KEY                       
049200       END-IF                                                             
049300       IF REQU-IDLBBET-KEY  = LOW-VALUES                                  
049400         CONTINUE                                                         
049500       ELSE                                                               
049600        MOVE FUNCTION UPPER-CASE (REQU-IDLBBET-KEY) TO                    
049700                                  REQU-IDLBBET-KEY                        
049800       END-IF                                                             
049900       IF REQU-IDDISTR(1) IS NUMERIC                                      
050000        MOVE REQU-IDDISTR(1) TO REQU-IDDISTR-DOLD                         
050100       END-IF                                                             
050200     END-IF                                                               
050300     .                                                                    
050400     EJECT                                                                
050500 B-KOLLA-NYCKLAR SECTION.                                                 
050600                                                                          
050700     MOVE 'B-KOLLA-NYCKLAR         ' TO CURR-SECTION                      
050800     MOVE JA TO NYCKLAR-SW                                                
050900                                                                          
051000     IF REQU-IDTRPTNR-KEY   = LOW-VALUES                                  
051100       CONTINUE                                                           
051200     ELSE                                                                 
051300       MOVE REQU-IDTRPTNR-KEY TO WS-IDTRPTNR                              
051400     END-IF                                                               
051500     IF REQU-IDTRPTNR-KEY NOT NUMERIC                                     
051600       MOVE '024'        TO RESP-IDMSG-ERROR                              
051700*      NOT NUMERIC ***                                                    
051800       MOVE 'IDTRP'      TO RESP-IDELMT-ERROR                             
051900       MOVE NEJ          TO NYCKLAR-SW                                    
052000     END-IF                                                               
052100                                                                          
052200     IF WS-IDTRPTNR NUMERIC AND WS-IDTRPTNR > ZERO                        
052300       MOVE WS-IDTRPTNR     TO W-IDTRPTNR                                 
052400     ELSE                                                                 
052500       MOVE NEJ             TO NYCKLAR-SW                                 
052600     END-IF                                                               
052700                                                                          
052800     IF REQU-IDLBBET-KEY     = LOW-VALUES                                 
052900       CONTINUE                                                           
053000     ELSE                                                                 
053100       MOVE REQU-IDLBBET-KEY TO WS-IDLBBET                                
053200     END-IF                                                               
053300     IF WS-IDLBBET  NOT = SPACE                                           
053400       MOVE WS-IDLBBET       TO W-IDLBBET                                 
053500     ELSE                                                                 
053600       MOVE NEJ              TO NYCKLAR-SW                                
053700     END-IF                                                               
053800                                                                          
053900     MOVE REQU-IDDC-KEY TO W-IDDC                                         
054000                           W-IDDC-4495                                    
054100                           RESP-IDDC-KEY                                  
054200                                                                          
054300     MOVE REQU-KDFARLIG-KEY TO WS-FLFARLIG                                
054400     IF WS-FLFARLIG = SPACE OR LOW-VALUES                                 
054500       MOVE NEJ TO NYCKLAR-SW                                             
054600     END-IF                                                               
054700     IF REQU-IDDISTR-DOLD NOT NUMERIC                                     
054800       MOVE ZERO              TO REQU-IDDISTR-DOLD                        
054900     ELSE                                                                 
055000       MOVE REQU-IDDISTR-DOLD TO TEST-IDDISTR                             
055100     END-IF                                                               
055200                                                                          
055300     IF NYCKLAR-OK                                                        
055400      MOVE WS-IDLBBET         TO RESP-IDLBBET-KEY                         
055500      MOVE WS-FLFARLIG        TO RESP-KDFARLIG-KEY(1:)                    
055600      MOVE REQU-IDTRPTNR-KEY  TO RESP-IDTRPTNR-KEY                        
055700      INSPECT RESP-IDDC-KEY     REPLACING LEADING ZERO BY SPACE           
055800     END-IF                                                               
055900                                                                          
056000     IF NYCKLAR-FEL                                                       
056100       MOVE ERR-WRONG-KEY    TO RESP-IDMSG-ERROR                          
056200     END-IF                                                               
056300     .                                                                    
056400     EJECT                                                                
056500 E-SAMMA-SIDA SECTION.                                                    
056600                                                                          
056700     MOVE 'E-SAMMA-SIDA            ' TO CURR-SECTION                      
056800     MOVE SPACE  TO W-IDKUNDRF                                            
056900     MOVE NEJ    TO SW-RAD-INPUT                                          
057000     MOVE +1     TO INDX                                                  
057100     PERFORM UNTIL (INDX > MAX-INDX)                                      
057200       IF REQU-KDCMD-RAD (INDX) = 'N' OR LOW-VALUES                       
057300          CONTINUE                                                        
057400       ELSE                                                               
057500          MOVE JA TO SW-RAD-INPUT                                         
057600       END-IF                                                             
057700                                                                          
057800       ADD +1  TO INDX                                                    
057900     END-PERFORM                                                          
058000                                                                          
058100     IF  REQU-FLAVSLUTA = 'N'                                             
058200     AND SW-RAD-INPUT = NEJ                                               
058300       CONTINUE                                                           
058400     ELSE                                                                 
058500       PERFORM EA-MID-INDATA-TILL-MOD                                     
058600     END-IF                                                               
058700     .                                                                    
058800     EJECT                                                                
058900 EA-MID-INDATA-TILL-MOD SECTION.                                          
059000     MOVE 'EA-MID-INDATA-TILL-MOD  ' TO CURR-SECTION                      
059100                                                                          
059200     IF  REQU-FLAVSLUTA = LOW-VALUES                                      
059300       CONTINUE                                                           
059400     ELSE                                                                 
059500       MOVE REQU-FLAVSLUTA TO RESP-FLAVSLUTA                              
059600     END-IF                                                               
059700                                                                          
059800     MOVE REQU-IDDISTR-DOLD TO RESP-IDDISTR-DOLD                          
059900                                                                          
060000     MOVE +1 TO INDX                                                      
060100                                                                          
060200     PERFORM UNTIL (INDX > MAX-INDX)                                      
060300       IF  REQU-KDCMD-RAD (INDX) = LOW-VALUES                             
060400         CONTINUE                                                         
060500       ELSE                                                               
060600         MOVE REQU-KDCMD-RAD (INDX) TO RESP-KDCMD-RAD (INDX)              
060700         MOVE REQU-FLCROSS (INDX)   TO RESP-FLCROSS (INDX)                
060800       END-IF                                                             
060900       ADD +1  TO INDX                                                    
061000     END-PERFORM                                                          
061100     .                                                                    
061200 F-LAES-VISA-INFO SECTION.                                                
061300                                                                          
061400     MOVE 'F-LAES-VISA-INFO        ' TO CURR-SECTION                      
061500                                                                          
061600     PERFORM FA-LAES-LASTAT-HITTILLS                                      
061700                                                                          
061800     IF SEGMENT-FINNS                                                     
061900        MOVE +1 TO INDX                                                   
062000        MOVE ZERO             TO W-IDDISTR                                
062100                                 W-IDKUNDNR                               
062200                                 W-IDPRODNR                               
062300                                 W-IDKOLLI                                
062400        MOVE SPACE            TO W-KDFAKTYP                               
062500                                 W-IDKUNDRF                               
062600        PERFORM IMS-GHNP-WDGX4498-FIRST                                   
062700        IF SEGMENT-FINNS                                                  
062800          MOVE 4498-IDDISTR   TO W-IDDISTR-NUM                            
062900          MOVE W-IDDISTR-NUM  TO RESP-IDDISTR-DOLD                        
063000          MOVE 4498-IDKUNDNR  TO W-IDKUNDNR-NUM                           
063100        END-IF                                                            
063200                                                                          
063300        PERFORM UNTIL INDX > MAX-INDX                                     
063400          IF SEGMENT-FINNS                                                
063500             MOVE 4498-IDDISTR  TO RESP-IDDISTR (INDX)                    
063600                                   TEST-IDDISTR                           
063700             MOVE 4498-IDKUNDNR TO RESP-IDKUNDNR (INDX)                   
063800             MOVE 4498-KDFAKTYP TO RESP-KDFAKTYP (INDX)                   
063900             MOVE 4498-IDORDNR7 TO RESP-IDORDNR7 (INDX)                   
064000             MOVE 4498-IDKOLLI  TO RESP-IDKOLLI (INDX)                    
064100             MOVE 4498-TIRFS    TO RESP-TIRFS (INDX)                      
064200             MOVE 4498-KDKOLLI  TO RESP-KDKOLLI (INDX)                    
064300                                                                          
064400             IF REQU-KDMATT = 'U'                                         
064500                COMPUTE WS-VKORDBTO =                                     
064600                        4498-VKORDBTO * CONV-KG-TO-LB                     
064700                COMPUTE WS-VLORDBTO =                                     
064800                        4498-VLORDBTO * CONV-M3-TO-YD3                    
064900                MOVE WS-VKORDBTO  TO RESP-VKORDBTO (INDX)                 
065000                MOVE WS-VLORDBTO  TO RESP-VLORDBTO (INDX)                 
065100                                                                          
065200             ELSE                                                         
065300                MOVE 4498-VKORDBTO TO RESP-VKORDBTO (INDX)                
065400                MOVE 4498-VLORDBTO TO RESP-VLORDBTO (INDX)                
065500             END-IF                                                       
065600                                                                          
065700             IF 4498-IDPSN(1) > ZERO                                      
065800                MOVE 4498-IDPSN(1) TO W-IDPSN-NUM                         
065900                IF 4498-IDPSN(2) > ZERO                                   
066000                   MOVE '*'         TO RESP-IDPSN(INDX) (1:1)             
066100                   MOVE W-IDPSN-NUM TO RESP-IDPSN(INDX) (2:3)             
066200                ELSE                                                      
066300                   MOVE ' '         TO RESP-IDPSN(INDX) (1:1)             
066400                   MOVE W-IDPSN-NUM TO RESP-IDPSN(INDX) (2:3)             
066500                END-IF                                                    
066600             ELSE                                                         
066700                MOVE SPACE          TO RESP-IDPSN(INDX)(1:)               
066800             END-IF                                                       
066900                                                                          
067000             MOVE 4498-IDPRODNR     TO W-IDPRODNR                         
067100                                       RESP-IDPRODNR(INDX)                
067200             MOVE 4498-FLCROSS      TO RESP-FLCROSS(INDX)                 
067210             IF 4498-FLCROSS = JA                                         
067300                PERFORM FB-GET-CROSSDOCK-RFS                              
067310             END-IF                                                       
067320                                                                          
067400             ADD 1 TO INDX                                                
067500             ADD 1 TO WS-COUNT                                            
067600                                                                          
067700             IF WS-COUNT < 501                                            
067800               CONTINUE                                                   
067900             ELSE                                                         
068000               MOVE '028'   TO RESP-IDMSG-ERROR                           
068100             END-IF                                                       
068200             MOVE WS-COUNT  TO RESP-KVRADER-MAX                           
068300                                                                          
068400                                                                          
068500             PERFORM IMS-GHNP-WDGX4498                                    
068600          ELSE                                                            
068700             ADD 1 TO INDX                                                
068800          END-IF                                                          
068900        END-PERFORM                                                       
069000                                                                          
069100        IF SEGMENT-FINNS                                                  
069200          MOVE 4498-IDDISTR    TO W-IDDISTR-NUM                           
069300          MOVE 4498-IDKUNDNR   TO W-IDKUNDNR-NUM                          
069400        END-IF                                                            
069500     END-IF                                                               
069600     .                                                                    
069700     EJECT                                                                
069800 FA-LAES-LASTAT-HITTILLS SECTION.                                         
069900                                                                          
070000     MOVE 'FA-LAES-LASTAT-HITTILLS ' TO CURR-SECTION                      
070100                                                                          
070200     PERFORM IMS-GHU-WDGX4496                                             
070300     IF SEGMENT-FINNS                                                     
070400       MOVE 4496-KVKOLLI-LAST      TO RESP-KVKOLLI-TOT                    
070500       MOVE 4496-VLORDBTO-LASTB    TO RESP-VLORDBTO-TOT                   
070600       MOVE 4496-VKORDBTO-LASTB    TO RESP-VKORDBTO-TOT                   
070700     ELSE                                                                 
070800       IF ARBREG-ROT-BORTTAGEN                                            
070900          MOVE '001'               TO RESP-IDMSG-INFO                     
071000       ELSE                                                               
071100          MOVE 'IDTRP'             TO RESP-IDELMT-ERROR                   
071200          MOVE '025'               TO RESP-IDMSG-ERROR                    
071300          MOVE ZERO                TO RESP-KVKOLLI-TOT                    
071400                                      RESP-VLORDBTO-TOT                   
071500                                      RESP-VKORDBTO-TOT                   
071600                                      RESP-KVRADER-MAX                    
071700       END-IF                                                             
071800     END-IF                                                               
071900     .                                                                    
072000     EJECT                                                                
072100 FB-GET-CROSSDOCK-RFS SECTION.                                            
072200                                                                          
072300     MOVE 'FB-GET-CROSSDOCK        ' TO CURR-SECTION                      
072400                                                                          
072410     MOVE 4498-IDPRODNR        TO W-IDPRODNR-KOLLI                        
072420     MOVE 4498-IDKOLLI         TO W-IDKOLLI-KOLLI                         
072430     MOVE W-IDDC-4495          TO W-IDDCCROSS                             
072440                                                                          
072450     PERFORM IMS-GU-WDE621-IDDCCROSS                                      
072460     IF SEGMENT-FINNS                                                     
072461        MOVE CROSS-TIRFSDAT   TO RESP-TIRFS (INDX)                        
072470     END-IF                                                               
072500     .                                                                    
072510     EJECT                                                                
072520 G-KOLLA-INPUT SECTION.                                                   
072530                                                                          
072540     MOVE 'G-KOLLA-INPUT           ' TO CURR-SECTION                      
072550                                                                          
072600     MOVE ZERO                 TO W-FUNKTION                              
072700     MOVE JA                   TO INDATA-SW                               
072800     MOVE NEJ                  TO SW-RAD-INPUT                            
072900     IF REQU-KVRADER-MAX NUMERIC AND REQU-KVRADER-MAX > 0                 
073000        MOVE REQU-KVRADER-MAX  TO WS-INDX-REC                             
073100        MOVE NEJ               TO WS-REC-LIMIT                            
073200        MOVE +1 TO INDX                                                   
073300        PERFORM UNTIL (INDX > MAX-INDX) OR  REC-LIMIT                     
073400                                                                          
073500          IF REQU-KDCMD-RAD (INDX) = LOW-VALUES OR 'N'                    
073600            CONTINUE                                                      
073700          ELSE                                                            
073800            MOVE JA   TO SW-RAD-INPUT                                     
073900          END-IF                                                          
074000                                                                          
074100          IF INDX = WS-INDX-REC                                           
074200            MOVE JA TO WS-REC-LIMIT                                       
074300          ELSE                                                            
074400            ADD +1  TO INDX                                               
074500          END-IF                                                          
074600        END-PERFORM                                                       
074700     ELSE                                                                 
074800        MOVE NEJ TO INDATA-SW                                             
074900        IF REQU-KVRADER-MAX = 0                                           
075000           MOVE 'KVRADER' TO RESP-IDELMT-ERROR                            
075100           MOVE '126'     TO RESP-IDMSG-ERROR                             
075200        ELSE                                                              
075300           MOVE 'KVRADER' TO RESP-IDELMT-ERROR                            
075400           MOVE '024'     TO RESP-IDMSG-ERROR                             
075500        END-IF                                                            
075600     END-IF                                                               
075700                                                                          
075800     IF  REQU-FLAVSLUTA = 'N'                                             
075900     AND SW-RAD-INPUT   = NEJ                                             
076000       MOVE ERR-PF11-AND-NO-DATA TO RESP-IDMSG-ERROR                      
076100       MOVE NEJ                  TO INDATA-SW                             
076200     ELSE                                                                 
076300       IF REQU-FLAVSLUTA = 'J' OR 'Y'                                     
076400         MOVE W-AVSLUTA          TO W-FUNKTION                            
076500       ELSE                                                               
076600         IF REQU-FLAVSLUTA = LOW-VALUES OR 'N'                            
076700            CONTINUE                                                      
076800         ELSE                                                             
076900            MOVE NEJ TO INDATA-SW                                         
077000            MOVE '023'                TO RESP-IDMSG-ERROR                 
077100            MOVE 'FLAVSLUTA'          TO RESP-IDELMT-ERROR                
077200         END-IF                                                           
077300       END-IF                                                             
077400                                                                          
077500       MOVE +1    TO  INDX                                                
077600       MOVE NEJ   TO WS-REC-LIMIT                                         
077700       PERFORM UNTIL INDX > MAX-INDX OR REC-LIMIT                         
077800         IF REQU-KDCMD-RAD(INDX) = 'X'                                    
077900           IF AVSLUTA                                                     
078000             MOVE NEJ            TO INDATA-SW                             
078100             MOVE '023'          TO RESP-IDMSG-ERROR                      
078200             MOVE 'KDCMDVAL'     TO RESP-IDELMT-ERROR                     
078300           ELSE                                                           
078400             IF REQU-KDCMD-RAD(INDX) = W-VALD                             
078500               MOVE W-BACKA-VALDA-RADER TO W-FUNKTION                     
078600               PERFORM S01-KOLLA-KOLLI                                    
078700             ELSE                                                         
078800               IF REQU-KDCMD-RAD(INDX) = LOW-VALUES OR 'N'                
078900                  CONTINUE                                                
079000               ELSE                                                       
079100                  MOVE NEJ      TO INDATA-SW                              
079200                  MOVE '023'      TO RESP-IDMSG-ERROR                     
079300                  MOVE 'KDCMDVAL' TO RESP-IDELMT-ERROR                    
079400               END-IF                                                     
079500             END-IF                                                       
079600           END-IF                                                         
079700         END-IF                                                           
079800         IF INDX = WS-INDX-REC                                            
079900           MOVE JA TO WS-REC-LIMIT                                        
080000         ELSE                                                             
080100           ADD +1 TO INDX                                                 
080200         END-IF                                                           
080300       END-PERFORM                                                        
080400     END-IF                                                               
080500     .                                                                    
080600     EJECT                                                                
080700 H-UPPDATERA SECTION.                                                     
080800                                                                          
080900     MOVE 'H-UPPDATERA             ' TO CURR-SECTION                      
081000                                                                          
081100     MOVE ZERO  TO W-KVKOLLI                                              
081200                   W-VKORDBTO                                             
081300                   W-VLORDBTO                                             
081400                                                                          
081500                                                                          
081600     EVALUATE TRUE                                                        
081700                                                                          
081800     WHEN AVSLUTA                                                         
081900         PERFORM HD-SKAPA-SKEPPNING                                       
082000                                                                          
082100     WHEN PROFORMA                                                        
082200         CONTINUE                                                         
082300                                                                          
082400     WHEN BACKA-VALDA-RADER                                               
082500         PERFORM HB-BACKA-VALDA-KOLLIN                                    
082600         PERFORM S05-TA-EV-BORT-ARBREG-ROT                                
082700         MOVE NEJ TO ITALIEN-SW                                           
082800                                                                          
082900     END-EVALUATE                                                         
083000     .                                                                    
083100     EJECT                                                                
083200 HB-BACKA-VALDA-KOLLIN SECTION.                                           
083300                                                                          
083400     MOVE 'HB-BACKA-VALDA-KOLLI    ' TO CURR-SECTION                      
083500                                                                          
083600     MOVE +1       TO INDX                                                
083700     MOVE NEJ      TO WS-REC-LIMIT                                        
083800                                                                          
083900     PERFORM UNTIL INDX > MAX-INDX                                        
084000                OR REC-LIMIT                                              
084100       IF REQU-KDCMD-RAD(INDX)     =  W-VALD                              
084200         MOVE REQU-IDPRODNR(INDX)    TO W-IDPRODNR-KOLLI                  
084300         MOVE REQU-IDKOLLI(INDX)     TO W-IDKOLLI-KOLLI                   
084400         MOVE ZERO                   TO W-SPAR-IDKOLLI-SAMP               
084500         PERFORM S02-UPPDATERA-WDE6                                       
084600                                                                          
084700         MOVE REQU-IDDISTR(INDX)     TO W-IDDISTR                         
084800                                        TEST-IDDISTR                      
084900         MOVE REQU-IDKUNDNR(INDX)    TO W-IDKUNDNR                        
085000                                                                          
085100         MOVE REQU-KDFAKTYP(INDX)    TO W-KDFAKTYP                        
085200         MOVE SPACE                  TO W-IDKUNDRF                        
085300         INSPECT REQU-IDORDNR7(INDX)                                      
085400                 REPLACING LEADING SPACE BY ZERO                          
085500         MOVE REQU-IDORDNR7(INDX)    TO W-IDORDNR7                        
085600         MOVE REQU-IDPRODNR(INDX)    TO W-IDPRODNR                        
085700         MOVE REQU-IDKOLLI(INDX)     TO W-IDKOLLI                         
085800         PERFORM S03-TAG-BORT-ARBREG                                      
085900       END-IF                                                             
086000                                                                          
086100       IF INDX = WS-INDX-REC                                              
086200          MOVE JA TO WS-REC-LIMIT                                         
086300       ELSE                                                               
086400          ADD +1                     TO INDX                              
086500       END-IF                                                             
086600     END-PERFORM                                                          
086700     PERFORM S04-UPPDATERA-ARBREG-TOT                                     
086800     .                                                                    
086900     EJECT                                                                
087000 HD-SKAPA-SKEPPNING SECTION.                                              
087100                                                                          
087200     MOVE 'HD-SKAPA-SKEPPNING      ' TO CURR-SECTION                      
087300*    -- THE SHIPMENT IS CREATED BY PGM WL0188 WHICH IS                    
087400*    -- STARTED BY THE WEB CONTROL LOGIC WHEN THIS                        
087500*    -- PROGRAM RETURNS TO THE WEB.                                       
087600*    -- BUT FOR SHIPMENTS FROM THE WAREHOUSE IN ITALY                     
087700*    -- WITH GOODS TO ITALIAN CUSTOMERS, A "BOLLA" DOCUMENT               
087800*    -- SHOULD FIRST BE CREATED BY STARTING PGM W40698.                   
087900*    -- W40698 SENDS THE DOCUMENT TO D&P WHICH THEN RETURNS A             
088000*    -- RESPONSE TO THE WEB. IN THIS CASE THIS PROGRAM                    
088100*    -- SHOULD JUST TERMINATE WITHOUT SENDING ANY RESPONSE.               
088200                                                                          
088300     MOVE REQU-IDDISTR-DOLD    TO  TEST-IDDISTR                           
088400                                   W-IDDISTR-ALFA                         
088500                                                                          
088600     MOVE W-IDDC               TO W-IDDC-B6                               
088700     PERFORM IMS-GU-WDB601                                                
088800     IF DCS-SDC AND DCS-IDLANDX2 = 'IT'                                   
088900*      -- NO BOLLA FOR TRANSPORT 90                                       
089000       IF WS-IDTRPTNR = '90 ' OR '090'                                    
089100         MOVE NEJ                TO ITALIEN-SW                            
089200       ELSE                                                               
089300*        -- NO BOLLA FOR TRANSPORT TO MALTA                               
089400         IF DIST92-MALTA                                                  
089500           MOVE NEJ              TO ITALIEN-SW                            
089600         ELSE                                                             
089700           MOVE JA               TO ITALIEN-SW                            
089800*          -- PREPARE INPUT TO W40698                                     
089900           MOVE ALL '+'          TO  SEND2-MID-W4I69801                   
090000           MOVE 001              TO  SEND2-REQU-IDMSGVER                  
090100           MOVE 'E'              TO  SEND2-REQU-KDPGMACT                  
090200           MOVE REQU-IDUSER      TO  SEND2-REQU-IDUSER                    
090300           MOVE WS-IDTRPTNR      TO  SEND2-MID-IDTRPTNR                   
090400           MOVE WS-IDLBBET       TO  SEND2-MID-IDLBBET                    
090500           MOVE WS-FLFARLIG      TO  SEND2-MID-FLFARLIG                   
090600           MOVE W-IDDC           TO  SEND2-MID-IDDC                       
090700           MOVE 'N'              TO  SEND2-MID-FLSKRIV-NU                 
090800           MOVE 'J'              TO  SEND2-MID-FLAVSLUTA                  
090900         END-IF                                                           
091000       END-IF                                                             
091100     ELSE                                                                 
091200        MOVE NEJ                 TO ITALIEN-SW                            
091300     END-IF                                                               
091400                                                                          
091500*    -- DECIDE IF WL0188 SHOULD RUN IN "BACKGROUND" (X) MODE              
091600*    -- WITHOUT ANY DIALOGUE, OR WITH A DIALOGE WHICH MAKES               
091700*    -- IT POSSIBLE TO "SUPPLEMENT" THE TRANPORT AND MAKE                 
091800*    -- CHANGES BEFORE IT IS CLOSED (U)                                   
091900     PERFORM S10-VILKEN-TRANS                                             
092000     IF FL-XTRANS                                                         
092100       MOVE 'X'                  TO RESP-KDTRTYP                          
092200     ELSE                                                                 
092300       MOVE 'U'                  TO RESP-KDTRTYP                          
092400     END-IF                                                               
092500     IF (SUB-KDTRANS(1:6) = 'WLA187' AND RESP-KDTRTYP = 'X')              
092600      MOVE ALL '+'               TO  SEND3-REQU-WL0188I1                  
092700      MOVE 001                   TO  SEND3-REQU-IDMSGVER                  
092800      MOVE SPACES                TO  SEND3-REQU-KDPGMACT                  
092900      MOVE REQU-IDUSER           TO  SEND3-REQU-IDUSER                    
093000      MOVE REQU-IDDC-KEY         TO  SEND3-REQU-IDDC-KEY                  
093100      MOVE REQU-IDTRPTNR-KEY     TO SEND3-REQU-IDTRPTNR-KEY               
093200      MOVE REQU-IDLBBET-KEY      TO  SEND3-REQU-IDLBBET-KEY               
093300      MOVE REQU-KDFARLIG-KEY     TO SEND3-REQU-KDFARLIG-KEY               
093400      MOVE 'N'                   TO  SEND3-REQU-FLSKRIV-NU                
093500      MOVE ZERO                  TO  SEND3-REQU-IDSHIPM                   
093600      MOVE REQU-FLAVSLUTA        TO  SEND3-REQU-FLAVSLUTA                 
093700      MOVE 'X'                   TO  SEND3-REQU-KDTRTYP                   
093800      MOVE NEJ                   TO  SW-NOT-WLA187                        
093900      PERFORM S23-SEND-OPEN-WL0188                                        
094000      PERFORM S24-SEND-MESSAGE-WL0188                                     
094100      PERFORM S25-SEND-CLOSE-WL0188                                       
094200     END-IF                                                               
094300                                                                          
094400     .                                                                    
094500     EJECT                                                                
094600 HDB-STARTA-W4T698X  SECTION.                                             
094700                                                                          
094800     MOVE 'HDB-STARTA-W4T698X      ' TO CURR-SECTION                      
094900                                                                          
095000     MOVE JA                         TO ITALIEN-SW                        
095100     PERFORM S20-SEND-OPEN-W40698                                         
095200     PERFORM S21-SEND-MESSAGE-W40698                                      
095300     PERFORM S22-SEND-CLOSE-W40698                                        
095400     .                                                                    
095500     EJECT                                                                
095600                                                                          
095700 S01-KOLLA-KOLLI   SECTION.                                               
095800                                                                          
095900     MOVE 'S01-KOLLA-KOLLI         ' TO CURR-SECTION                      
096000                                                                          
096100     IF REQU-IDDISTR(INDX) NUMERIC                                        
096200                                                                          
096300       IF REQU-FLCROSS(INDX) = JA                                         
096400         CONTINUE                                                         
096500       ELSE                                                               
096600         MOVE REQU-IDPRODNR(INDX) TO W-IDPRODNR-KOLLI                     
096700         MOVE REQU-IDKOLLI(INDX)  TO W-IDKOLLI-KOLLI                      
096800                                                                          
096900         PERFORM IMS-GHU-WDE611                                           
097000         IF SEGMENT-FINNS                                                 
097100           IF KOLLI-KDKOLSTA = KLI-PACK-FAKT                              
097200               CONTINUE                                                   
097300           ELSE                                                           
097400              MOVE NEJ            TO INDATA-SW                            
097500              MOVE '023'          TO RESP-IDMSG-ERROR                     
097600                                     RESP-IDMSG-ERROR-LINE(INDX)          
097700              MOVE 'KDKOLLI'      TO RESP-IDELMT-ERROR                    
097800           END-IF                                                         
097900         ELSE                                                             
098000           MOVE NEJ               TO INDATA-SW                            
098100           MOVE '025'             TO RESP-IDMSG-ERROR                     
098200                                     RESP-IDMSG-ERROR-LINE(INDX)          
098300           MOVE 'IDKOLLI'         TO RESP-IDELMT-ERROR                    
098400         END-IF                                                           
098500       END-IF                                                             
098600     END-IF                                                               
098700     .                                                                    
098800     EJECT                                                                
098900 S02-UPPDATERA-WDE6    SECTION.                                           
099000                                                                          
099100     MOVE 'S02-UPPDATERA-WDE6      ' TO CURR-SECTION                      
099200                                                                          
099300     PERFORM IMS-GHU-WDE611                                               
099400     IF SEGMENT-FINNS                                                     
099500       ADD 1                         TO W-KVKOLLI                         
099600       ADD KOLLI-VLORDBTO-KOLLI      TO W-VLORDBTO                        
099700       ADD KOLLI-VKORDBTO-KOLLI      TO W-VKORDBTO                        
099800       IF REQU-FLCROSS(INDX) = JA                                         
099900         PERFORM IMS-GHNP-WDE621                                          
100000         IF SEGMENT-FINNS                                                 
100100           MOVE 1                    TO CROSS-KDKOLSTA-CROSS              
100200           PERFORM IMS-REPL-WDE621                                        
100300         END-IF                                                           
100400                                                                          
100500       ELSE                                                               
100600         MOVE W-SPAR-IDKOLLI-SAMP    TO KOLLI-IDKOLLI-SAMP                
100700         MOVE KOLLI-IDKUNDNR         TO W-SPAR-IDKUNDNR                   
100800         IF KOLLI-IDKOLLI-SAMP  >  ZERO                                   
100900            MOVE NEJ                 TO KOLLI-FLUTLAST                    
101000         ELSE                                                             
101100            MOVE JA                  TO KOLLI-FLUTLAST                    
101200         END-IF                                                           
101300         MOVE SPACE                  TO KOLLI-IDLBBET                     
101400         MOVE KLI-PACK               TO KOLLI-KDKOLSTA                    
101500         PERFORM IMS-REPL-WDE611                                          
101600                                                                          
101700         MOVE KOLLI-VLORDBTO-KOLLI   TO W-SPAR-VLORDBTO                   
101800         MOVE KOLLI-VKORDBTO-KOLLI   TO W-SPAR-VKORDBTO                   
101900         IF DIST79-DEALER-PRICE                                           
102000           MOVE KOLLI-SUORDV-LOC  TO W-SPAR-SUORDV-LOC                    
102100           MOVE KOLLI-SUORDV-LOCPREL TO W-SPAR-SUORDV-LOCPREL             
102200         ELSE                                                             
102300           IF KOLLI-SUORDV-KLI-EXP > ZERO                                 
102400             MOVE KOLLI-SUORDV-KLI-EXP TO W-SPAR-SUORDV                   
102500           ELSE                                                           
102600             MOVE KOLLI-SUORDV-KOLLI TO W-SPAR-SUORDV                     
102700           END-IF                                                         
102800         END-IF                                                           
102900       END-IF                                                             
103000                                                                          
103100       PERFORM IMS-GHU-WDE601                                             
103200       SUBTRACT +1             FROM VORD-KVKOLLI-FL                       
103300       MOVE REQU-IDDISTR-DOLD        TO  TEST-IDDISTR                     
103400       IF DIST79-DEALER-PRICE                                             
103500         SUBTRACT W-SPAR-SUORDV-LOC  FROM VORD-SUORDV-FL-LOC              
103600        SUBTRACT W-SPAR-SUORDV-LOCPREL FROM VORD-SUORDV-FL-LOCPREL        
103700       ELSE                                                               
103800         SUBTRACT W-SPAR-SUORDV  FROM VORD-SUORDV-FL                      
103900       END-IF                                                             
104000       SUBTRACT W-SPAR-VKORDBTO FROM VORD-VKORDBTO-FL                     
104100       SUBTRACT W-SPAR-VLORDBTO FROM VORD-VLORDBTO-FL                     
104200                                                                          
104300       PERFORM IMS-REPL-WDE601                                            
104400     END-IF                                                               
104500     .                                                                    
104600     EJECT                                                                
104700 S03-TAG-BORT-ARBREG       SECTION.                                       
104800                                                                          
104900     MOVE 'S03-TAG-BORT-ARBREG     ' TO CURR-SECTION                      
105000                                                                          
105100     PERFORM IMS-GHU-WDGX4498                                             
105200     IF SEGMENT-FINNS                                                     
105300        PERFORM IMS-DLET-WDGX4498                                         
105400     END-IF                                                               
105500                                                                          
105600     .                                                                    
105700     EJECT                                                                
105800 S04-UPPDATERA-ARBREG-TOT  SECTION.                                       
105900                                                                          
106000     MOVE 'S04-UPPDATERA-ARBREG-TOT' TO CURR-SECTION                      
106100                                                                          
106200     PERFORM IMS-GHU-WDGX4496                                             
106300     IF SEGMENT-FINNS                                                     
106400       SUBTRACT W-KVKOLLI      FROM 4496-KVKOLLI-LAST                     
106500       SUBTRACT W-VKORDBTO     FROM 4496-VKORDBTO-LASTB                   
106600       SUBTRACT W-VLORDBTO     FROM 4496-VLORDBTO-LASTB                   
106700                                                                          
106800       PERFORM IMS-REPL-WDGX4496                                          
106900     END-IF                                                               
107000     .                                                                    
107100     EJECT                                                                
107200                                                                          
107300 S05-TA-EV-BORT-ARBREG-ROT SECTION.                                       
107400                                                                          
107500     MOVE 'S05-TA-EV-BORT-ARBREG-RO' TO CURR-SECTION                      
107600                                                                          
107700     PERFORM IMS-GU-WDGX4498-OKVAL                                        
107800     IF SEGMENT-SAKNAS                                                    
107900        MOVE JA TO DELETE-ARBREG-ROT-SW                                   
108000        PERFORM IMS-GHU-4495                                              
108100        IF SEGMENT-FINNS                                                  
108200           PERFORM IMS-DLET-4495                                          
108300        END-IF                                                            
108400     ELSE                                                                 
108500        MOVE NEJ TO DELETE-ARBREG-ROT-SW                                  
108600     END-IF                                                               
108700     .                                                                    
108800     EJECT                                                                
108900                                                                          
109000 S10-VILKEN-TRANS   SECTION.                                              
109100                                                                          
109200     MOVE NEJ                            TO SW-FLTRANS                    
109300     SET IDDC-IX  TO  1                                                   
109400     SEARCH TRANS-TABELL                                                  
109500              AT END                                                      
109600                     MOVE NEJ            TO SW-FLTRANS                    
109700            WHEN TRA-IDDC (IDDC-IX) = W-IDDC                              
109800       AND                                                                
109900            TRA-IDDISTR-FOM (IDDC-IX) NOT > W-IDDISTR-ALFA                
110000       AND                                                                
110100            TRA-IDDISTR-TOM (IDDC-IX) NOT < W-IDDISTR-ALFA                
110200                                                                          
110300                   MOVE JA               TO SW-FLTRANS                    
110400     END-SEARCH                                                           
110500     .                                                                    
110600     EJECT                                                                
110700                                                                          
110800 S11-MSG-CONV SECTION.                                                    
110900     MOVE LOW-VALUES              TO RESP-MESSAGES (1)                    
111000                                     RESP-MESSAGES (2)                    
111100     MOVE 1                       TO MSG-IX                               
111200*    REQUEST OK                                                           
111300     MOVE 200                     TO RESP-KDSTATUS-API                    
111400     IF RESP-IDMSG-INFO > SPACE                                           
111500       MOVE SPACES                TO MSG-CONV-AREA                        
111600       MOVE RESP-IDMSG-INFO       TO MSG-CONV-IDMSG-IN                    
111700       CALL WMSGCONV           USING MSG-CONV-AREA                        
111800       MOVE MSG-CONV-IDMSG-OUT    TO RESP-IDMSG   (MSG-IX)                
111900       MOVE MSG-CONV-MESSAGE      TO RESP-MESSAGE (MSG-IX)                
112000       ADD 1                      TO MSG-IX                               
112100     END-IF                                                               
112200     IF RESP-IDMSG-ERROR > SPACE                                          
112300*     BAD REQUEST                                                         
112400       MOVE 400                   TO RESP-KDSTATUS-API                    
112500       MOVE SPACES                TO MSG-CONV-AREA                        
112600       MOVE RESP-IDMSG-ERROR      TO MSG-CONV-IDMSG-IN                    
112700       MOVE RESP-IDELMT-ERROR     TO MSG-CONV-IDELMT                      
112800       CALL WMSGCONV           USING MSG-CONV-AREA                        
112900       MOVE MSG-CONV-IDMSG-OUT    TO RESP-IDMSG   (MSG-IX)                
113000       MOVE MSG-CONV-MESSAGE      TO RESP-MESSAGE (MSG-IX)                
113100     END-IF                                                               
113200     .                                                                    
113300*    --- DISPATCHER SECTIONS                                              
113400 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
113500                                                                          
113600     MOVE 'S01-FETCH-REQUEST-ARGUME0 ' TO CURR-SECTION                    
113700                                                                          
113800     MOVE 'GETARG'               TO SUB-KDFUNC                            
113900     MOVE 'CARPARTS.LDC.CLOSETRANSPORT'    TO SUB-ADDISPABS               
114000                                                                          
114100*    MOVE MAX-INDX(500) TO REQU-KVRADER-MAX SO THAT THE                   
114200*    LENGTH IS CALCULATED CORRECTLY TO BE ABLE TO FETCH ALL               
114300*    POSSIBLE INPUT                                                       
114400     MOVE MAX-INDX               TO REQU-KVRADER-MAX                      
114500     MOVE LENGTH OF REQU-AREA    TO SUB-KVDLEN                            
114600                                                                          
114700     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
114800                                                                          
114900     IF SUB-KDRC > 0                                                      
115000       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
115100       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
115200       DELIMITED BY SIZE INTO ERROR-TEXT                                  
115300       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
115400     END-IF                                                               
115500     .                                                                    
115600     SKIP3                                                                
115700 S02-RETURN-RESPONSE SECTION.                                             
115800                                                                          
115900     MOVE 'S02-RETURN-RESPONSE       ' TO CURR-SECTION                    
116000                                                                          
116100     MOVE 'RETURN'                   TO SUB-KDFUNC                        
116200     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
116300                                                                          
116400     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
116500                                                                          
116600     IF SUB-KDRC > 0                                                      
116700       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
116800       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
116900       DELIMITED BY SIZE INTO ERROR-TEXT                                  
117000       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
117100     END-IF                                                               
117200     .                                                                    
117300     EJECT                                                                
117400 S20-SEND-OPEN-W40698 SECTION.                                            
117500     MOVE 'S20-SEND-OPEN-W40698      ' TO CURR-SECTION                    
117600                                                                          
117700     MOVE 'OPEN'                        TO SEND-KDFUNC                    
117800     MOVE WS-ADRESS-W40698              TO SEND-ADDISPABS                 
117900     CALL WZ01SEND USING SEND-CONTROL-AREA                                
118000                         SEND-OPEN-AREA                                   
118100     IF SEND-KDRC > 0                                                     
118200       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
118300       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
118400       DELIMITED BY SIZE INTO FELTEXT                                     
118500       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
118600     END-IF                                                               
118700     .                                                                    
118800     EJECT                                                                
118900 S21-SEND-MESSAGE-W40698 SECTION.                                         
119000     MOVE 'S21-SEND-MESSAGE-W40698 ' TO CURR-SECTION                      
119100                                                                          
119200     MOVE 'PUT'                           TO SEND-KDFUNC                  
119300     MOVE LENGTH OF SEND-AREA-2           TO SEND-KVDLEN                  
119400     CALL WZ01SEND USING SEND-CONTROL-AREA                                
119500                         SEND-KVDLEN                                      
119600                         SEND-AREA-2                                      
119700     IF SEND-KDRC > 0                                                     
119800       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
119900       STRING 'WZ01SEND GET ERROR RC=' KDRC-DISPLAY                       
120000       DELIMITED BY SIZE INTO FELTEXT                                     
120100       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
120200     END-IF                                                               
120300     .                                                                    
120400     EJECT                                                                
120500 S22-SEND-CLOSE-W40698 SECTION.                                           
120600     MOVE 'S22-SEND-CLOSE-W40698   ' TO CURR-SECTION                      
120700                                                                          
120800     MOVE 'CLOSE'                    TO SEND-KDFUNC                       
120900     CALL WZ01SEND USING SEND-CONTROL-AREA                                
121000                                                                          
121100     IF SEND-KDRC > 0                                                     
121200       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
121300       STRING 'WZ01SEND CLOSE ERROR RC=' KDRC-DISPLAY                     
121400       DELIMITED BY SIZE INTO FELTEXT                                     
121500       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
121600     END-IF                                                               
121700     .                                                                    
121800     EJECT                                                                
121900 S23-SEND-OPEN-WL0188 SECTION.                                            
122000     MOVE 'S23-SEND-OPEN-WL0188      ' TO CURR-SECTION                    
122100                                                                          
122200     MOVE 'OPEN'                        TO SEND-KDFUNC                    
122300     MOVE WS-ADRESS-WL0188              TO SEND-ADDISPABS                 
122400     CALL WZ01SEND USING SEND-CONTROL-AREA                                
122500                         SEND-OPEN-AREA                                   
122600     IF SEND-KDRC > 0                                                     
122700       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
122800       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
122900       DELIMITED BY SIZE INTO FELTEXT                                     
123000       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
123100     END-IF                                                               
123200     .                                                                    
123300     EJECT                                                                
123400 S24-SEND-MESSAGE-WL0188 SECTION.                                         
123500     MOVE 'S24-SEND-MESSAGE-WL0188 ' TO CURR-SECTION                      
123600                                                                          
123700     MOVE 'PUT'                           TO SEND-KDFUNC                  
123800     MOVE LENGTH OF SEND-AREA-3           TO SEND-KVDLEN                  
123900     CALL WZ01SEND USING SEND-CONTROL-AREA                                
124000                         SEND-KVDLEN                                      
124100                         SEND-AREA-3                                      
124200     IF SEND-KDRC > 0                                                     
124300       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
124400       STRING 'WZ01SEND GET ERROR RC=' KDRC-DISPLAY                       
124500       DELIMITED BY SIZE INTO FELTEXT                                     
124600       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
124700     END-IF                                                               
124800     .                                                                    
124900     EJECT                                                                
125000 S25-SEND-CLOSE-WL0188 SECTION.                                           
125100     MOVE 'S25-SEND-CLOSE-WL0188   ' TO CURR-SECTION                      
125200                                                                          
125300     MOVE 'CLOSE'                    TO SEND-KDFUNC                       
125400     CALL WZ01SEND USING SEND-CONTROL-AREA                                
125500                                                                          
125600     IF SEND-KDRC > 0                                                     
125700       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
125800       STRING 'WZ01SEND CLOSE ERROR RC=' KDRC-DISPLAY                     
125900       DELIMITED BY SIZE INTO FELTEXT                                     
126000       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
126100     END-IF                                                               
126200     .                                                                    
126300     EJECT                                                                
126400 IMS-GHU-4495  SECTION.                                                   
126500     STRING 'WDR401  (WDGXKEY  =' W-4495-X ')'                            
126600                      DELIMITED BY SIZE INTO SSA1                         
126700     MOVE '  GE' TO GODK-STATUSKODER                                      
126800     CALL CBLTDLI USING GHU 4495-PCB DLI-IO-4495 SSA1                     
126900     MOVE 4495-STATUS-CODE TO STATUS-WS                                   
127000     PERFORM IMS-STATUSKONTROLL                                           
127100     .                                                                    
127200     SKIP2                                                                
127300 IMS-DLET-4495  SECTION.                                                  
127400     MOVE '    ' TO GODK-STATUSKODER                                      
127500     CALL CBLTDLI USING DLET 4495-PCB DLI-IO-4495                         
127600     MOVE 4495-STATUS-CODE TO STATUS-WS                                   
127700     PERFORM IMS-STATUSKONTROLL                                           
127800     .                                                                    
127900     SKIP2                                                                
128000 IMS-GHU-WDGX4496 SECTION.                                                
128100     STRING 'WDR401  *P(WDGXKEY  =' W-4495-X ')'                          
128200                      DELIMITED BY SIZE INTO SSA1                         
128300     MOVE  'WDGX4496 ' TO SSA2                                            
128400     MOVE '  GE' TO GODK-STATUSKODER                                      
128500     CALL CBLTDLI USING GHU 4495-PCB DLI-IO-4496 SSA1 SSA2                
128600     MOVE 4495-STATUS-CODE TO STATUS-WS                                   
128700     PERFORM IMS-STATUSKONTROLL                                           
128800     .                                                                    
128900     SKIP2                                                                
129000 IMS-REPL-WDGX4496 SECTION.                                               
129100     MOVE '  ' TO GODK-STATUSKODER                                        
129200     CALL CBLTDLI USING REPL 4495-PCB DLI-IO-4496                         
129300     MOVE 4495-STATUS-CODE TO STATUS-WS                                   
129400     PERFORM IMS-STATUSKONTROLL                                           
129500     .                                                                    
129600     SKIP2                                                                
129700 IMS-GHNP-WDGX4498-FIRST SECTION.                                         
129800     STRING 'WDGX4498(WDGXKEY >=' W-4498-X ')'                            
129900                      DELIMITED BY SIZE INTO SSA1                         
130000     MOVE '  GEGB' TO GODK-STATUSKODER                                    
130100     CALL CBLTDLI USING GHNP 4495-PCB DLI-IO-4498 SSA1                    
130200     MOVE 4495-STATUS-CODE TO STATUS-WS                                   
130300     PERFORM IMS-STATUSKONTROLL                                           
130400     .                                                                    
130500     SKIP2                                                                
130600 IMS-GHNP-WDGX4498      SECTION.                                          
130700     MOVE 'WDGX4498 ' TO SSA1                                             
130800     MOVE '  GEGB' TO GODK-STATUSKODER                                    
130900     CALL CBLTDLI USING GHNP 4495-PCB DLI-IO-4498 SSA1                    
131000     MOVE 4495-STATUS-CODE TO STATUS-WS                                   
131100     PERFORM IMS-STATUSKONTROLL                                           
131200     .                                                                    
131300     SKIP2                                                                
131400 IMS-GHU-WDGX4498 SECTION.                                                
131500     STRING 'WDR401  (WDGXKEY  =' W-4495-X ')'                            
131600                      DELIMITED BY SIZE INTO SSA1                         
131700     STRING 'WDGX4498(WDGXKEY  =' W-4498-X ')'                            
131800                      DELIMITED BY SIZE INTO SSA2                         
131900     MOVE '  GE' TO GODK-STATUSKODER                                      
132000     CALL CBLTDLI USING GHU 4495-PCB DLI-IO-4498 SSA1 SSA2                
132100     MOVE 4495-STATUS-CODE TO STATUS-WS                                   
132200     PERFORM IMS-STATUSKONTROLL                                           
132300     .                                                                    
132400     SKIP2                                                                
132500 IMS-GU-WDGX4498-OKVAL SECTION.                                           
132600     STRING 'WDR401  (WDGXKEY  =' W-4495-X ')'                            
132700                      DELIMITED BY SIZE INTO SSA1                         
132800     MOVE   'WDGX4498'       TO SSA2                                      
132900     MOVE '  GE' TO GODK-STATUSKODER                                      
133000     CALL CBLTDLI USING GU 4495-PCB DLI-IO-4498 SSA1 SSA2                 
133100     MOVE 4495-STATUS-CODE TO STATUS-WS                                   
133200     PERFORM IMS-STATUSKONTROLL                                           
133300     .                                                                    
133400     SKIP2                                                                
133500 IMS-DLET-WDGX4498 SECTION.                                               
133600                                                                          
133700     MOVE '  ' TO GODK-STATUSKODER                                        
133800     CALL CBLTDLI USING DLET 4495-PCB DLI-IO-4498                         
133900     MOVE 4495-STATUS-CODE TO STATUS-WS                                   
134000     PERFORM IMS-STATUSKONTROLL                                           
134100     .                                                                    
134200     SKIP2                                                                
134300 IMS-GHU-WDE601 SECTION.                                                  
134400                                                                          
134500     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-X ')'                        
134600            DELIMITED BY SIZE INTO SSA1                                   
134700     MOVE '  ' TO GODK-STATUSKODER                                        
134800     CALL CBLTDLI USING GHU WDE6-PCB DLI-IO-E601 SSA1                     
134900     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
135000     PERFORM IMS-STATUSKONTROLL                                           
135100     .                                                                    
135200                                                                          
135300 IMS-GHU-WDE611 SECTION.                                                  
135400                                                                          
135500     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-X ')'                        
135600            DELIMITED BY SIZE INTO SSA1                                   
135700     STRING 'WDE611  (IDKOLLI  =' W-IDKOLLI-X ')'                         
135800            DELIMITED BY SIZE INTO SSA2                                   
135900     MOVE '  ' TO GODK-STATUSKODER                                        
136000     CALL CBLTDLI USING GHU WDE6-PCB DLI-IO-E611 SSA1 SSA2                
136100     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
136200     PERFORM IMS-STATUSKONTROLL                                           
136300     .                                                                    
136400                                                                          
136500 IMS-GHNP-WDE621 SECTION.                                                 
136600                                                                          
136700     STRING 'WDE621  (KDKOLSTX =' W-KDKOLSTX-X                            
136800                    '&IDDCCROS =' REQU-IDDC-KEY                           
136900                    '&TIRECDAT >' W-TIRECXDAT-X ')'                       
137000     DELIMITED BY SIZE INTO SSA1                                          
137100     MOVE '  GE'      TO GODK-STATUSKODER                                 
137200     CALL CBLTDLI USING GHNP WDE6-PCB DLI-IO-E621 SSA1                    
137300     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
137400     PERFORM IMS-STATUSKONTROLL                                           
137500     .                                                                    
137600                                                                          
137700 IMS-REPL-WDE621 SECTION.                                                 
137800                                                                          
137900     MOVE '  ' TO GODK-STATUSKODER                                        
138000     CALL CBLTDLI USING REPL WDE6-PCB DLI-IO-E621                         
138100     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
138200     PERFORM IMS-STATUSKONTROLL                                           
138300     .                                                                    
138400                                                                          
138500 IMS-REPL-WDE601 SECTION.                                                 
138600                                                                          
138700     MOVE '  ' TO GODK-STATUSKODER                                        
138800     CALL CBLTDLI USING REPL WDE6-PCB DLI-IO-E601                         
138900     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
139000     PERFORM IMS-STATUSKONTROLL                                           
139100     .                                                                    
139200                                                                          
139300 IMS-REPL-WDE611 SECTION.                                                 
139400                                                                          
139500     MOVE '  ' TO GODK-STATUSKODER                                        
139600     CALL CBLTDLI USING REPL WDE6-PCB DLI-IO-E611                         
139700     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
139800     PERFORM IMS-STATUSKONTROLL                                           
139900     .                                                                    
140000                                                                          
140010 IMS-GU-WDE621-IDDCCROSS SECTION.                                         
140020                                                                          
140030     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-X ')'                        
140040          DELIMITED BY SIZE INTO SSA1                                     
140050     STRING 'WDE611  (IDKOLLI  =' W-IDKOLLI-X ')'                         
140060          DELIMITED BY SIZE INTO SSA2                                     
140070     STRING 'WDE621  (IDDCCROS =' W-IDDCCROSS-X')'                        
140080          DELIMITED BY SIZE INTO SSA3                                     
140090                                                                          
140091     MOVE '  GE' TO GODK-STATUSKODER                                      
140092     CALL CBLTDLI USING GU WDE6-PCB DLI-IO-E621 SSA1 SSA2 SSA3            
140093     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
140094     PERFORM IMS-STATUSKONTROLL                                           
140095     .                                                                    
140096                                                                          
140100 IMS-GU-WDE401-ASEK       SECTION.                                        
140200                                                                          
140300     STRING 'WDE401  (WDE4ASEQ =' W-WDE4ASEQ-X ')'                        
140400     DELIMITED BY SIZE INTO SSA1                                          
140500     MOVE '  GE' TO GODK-STATUSKODER                                      
140600     CALL CBLTDLI USING GU WDE4-PCB DLI-IO-E401 SSA1                      
140700     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
140800     PERFORM IMS-STATUSKONTROLL                                           
140900     .                                                                    
141000                                                                          
141100 IMS-GN-WDE401-ASEK       SECTION.                                        
141200                                                                          
141300     STRING 'WDE401  (WDE4ASEQ =' W-WDE4ASEQ-X ')'                        
141400     DELIMITED BY SIZE INTO SSA1                                          
141500     MOVE '  GEGB' TO GODK-STATUSKODER                                    
141600     CALL CBLTDLI USING GN WDE4-PCB DLI-IO-E401 SSA1                      
141700     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
141800     PERFORM IMS-STATUSKONTROLL                                           
141900     .                                                                    
142000                                                                          
142100 IMS-GU-WDB201      SECTION.                                              
142200                                                                          
142300     STRING 'WDB201  (IDGMT   >=' W-IDGMT-X ')'                           
142400                      DELIMITED BY SIZE INTO SSA1                         
142500     MOVE '    ' TO GODK-STATUSKODER                                      
142600     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-B201 SSA1                      
142700     MOVE WDB2-STATUS-CODE TO STATUS-WS                                   
142800     PERFORM IMS-STATUSKONTROLL                                           
142900     .                                                                    
143000                                                                          
143100 IMS-GU-WDB601    SECTION.                                                
143200     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
143300          DELIMITED BY SIZE INTO SSA1                                     
143400     MOVE '  GE' TO GODK-STATUSKODER                                      
143500     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
143600     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
143700     PERFORM IMS-STATUSKONTROLL                                           
143800     IF SEGMENT-SAKNAS                                                    
143900        MOVE SPACE TO DCS-KDDC                                            
144000     END-IF                                                               
144100     .                                                                    
144200     EJECT                                                                
144300 IMS-STATUSKONTROLL SECTION.                                              
144400                                                                          
144500     SET STATUS-IX TO 1                                                   
144600     SEARCH GODK-STATUS                                                   
144700       AT END                                                             
144800         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
144900         DELIMITED BY SIZE INTO FELTEXT                                   
145000         CALL FELLOG                                                      
145100       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
145200         CONTINUE                                                         
145300     END-SEARCH                                                           
145400     .                                                                    
