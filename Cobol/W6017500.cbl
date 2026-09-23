000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W6017500.                                                
000300 AUTHOR.         BO HAMMARIN.                                             
000400 DATE-WRITTEN.   MARS-99.                                                 
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        PGM HANTERAR MOTTAGNING AV DIREKTLEVERANSER SOM SKALL            
000900*        VIDARE UT TILL KUNDEN VIA VCP'S CENTRALLAGER.                    
001000*                                                                         
001100*        HANTERINGEN ÄR PÅ KOLLINIVÅ OCH MAN KAN SÅVÄL KLARMARKERA        
001200*        ETT KOLLI FÖR LASTNING/FAKTURERING SOM BACKA DETSAMMA.           
001300*                                                                         
001400*        FLER ÄN EN RAD KAN BEARBETAS I SAMMA TRANSAKTION (PF11).         
001500*                                                                         
001600*        KOLLIN GÖRS KLARA FÖR LASTNING/FAKTURERING GENOM ATT;            
001700*        >>> WDE601                                                       
001800*        1 ANTAL KOLLIN UPPDATERAS                                        
001900*        2 ORDERSTATUS UPPDATERAS                                         
002000*          OM VISSA VILLKOR UPPFYLLS                                      
002100*                                                                         
002200*        >>> WDE610                                                       
002300*        1 FLAGGA FÖR AUT.FAKTURERING SÄTTS TILL JA                       
002400*        2 KOLLISTATUS SÄTTS TILL 1                                       
002500*        3 PACKNINGSTID/PACKNINGSDATUM SÄTTS TILL MASKINTID               
002600*        4 PGM W403PLAT ANROPAS FÖR PLATSBESTÄMNING                       
002700*                                                                         
002800*        >>> WDG7XX                                                       
002900*        1 HÄNDELSETRANSAR FÖR AUTOMATFAKTURERING-SVERIGE SKAPAS          
003000*          OM VISSA VILLKOR UPPFYLLS                                      
003100*                                                                         
003200*        >>> WDR601                                                       
003300*        1 LOGG-POST SKAPAS                                               
003400*                                                                         
003500*        VID BACKNING;                                                    
003600*        >>> WDE601                                                       
003700*        1 ANTAL KOLLIN UPPDATERAS                                        
003800*        2 ORDERSTATUS UPPDATERAS                                         
003900*          OM VISSA VILLKOR UPPFYLLS                                      
004000*                                                                         
004100*        >>> WDE610                                                       
004200*        1 FLAGGA FÖR AUT.FAKTURERING SÄTTS TILL NEJ                      
004300*        2 KOLLISTATUS SÄTTS TILL 0                                       
004400*        3 PACKNINGSTID/PACKNINGSDATUM SÄTTS TILL 0                       
004500*                                                                         
004600*        >>> WDG7XX                                                       
004700*        1 HÄNDELSETRANSAR FÖR AUTOMATFAKTURERING-SVERIGE TAS BORT        
004800*          OM SÅDANA FINNS                                                
004900*                                                                         
005000*        >>> WDR601                                                       
005100*        1 LOGG-POST SKAPAS                                               
005200*                                                                         
005300*        PROGRAMMET UPPDATERAR WDE6                                       
005400*        PROGRAMMET UPPDATERAR WLXXDV (WDG7)                              
005500*        PROGRAMMET UPPDATERAR WLFILA (WDR6)                              
005600*        PROGRAMMET LÄSER      WDE6F                                      
005700*                                                                         
005800*    E-TRACKER: 7845458 DAT. 2010-02   ADD DISTR. AS KEY ON SCREEN        
005900*                                                                         
006000*    INDATA.                                                              
006100*        TRANSAKTION: W6T175                                              
006200*                     W6T175U                                             
006300*                                                                         
006400*        MID:         W6I17501                                            
006500*                                                                         
006600*    UTDATA.                                                              
006700*        MOD:         W6O17501                                            
006800                                                                          
006900 ENVIRONMENT DIVISION.                                                    
007000     EJECT                                                                
007100 DATA DIVISION.                                                           
007200 WORKING-STORAGE SECTION.                                                 
007300                                                                          
007400*    CHECKED BY WY2000                                                    
007500     SKIP3                                                                
007600 77  IDPGM                       PIC X(08)   VALUE 'W6017500'.            
007700                                                                          
007800*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
007900 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
008000                                                                          
008100 77  JA                          PIC X       VALUE 'J'.                   
008200 77  NEJ                         PIC X       VALUE 'N'.                   
008300 77  WS-CDC-11                   PIC X(2)    VALUE '11'.                  
008400 77  WS-IDLEVNR                  PIC X(5)    VALUE SPACE.                 
008500 77  WS-IDDISTR-IN               PIC X(4)    VALUE ZERO.                  
008600 77  WS-SLASK                    PIC  9(12).                              
008700                                                                          
008800 77  CURRENT-SECTION             PIC X(30)   VALUE SPACE.                 
008900 77  CURRENT-IMS-SECTION         PIC X(30)   VALUE SPACE.                 
009000                                                                          
009100*    --- INDEX FÖR BLÄDDRINGSRADER                                        
009200 77  INDX                        PIC S9(4)   VALUE +0   COMP SYNC.        
009300 77  MAX-INDX                    PIC S9(4)   VALUE +15  COMP SYNC.        
009400                                                                          
009500*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
009600                                                                          
009700 77  INDATA-SW                   PIC X       VALUE 'J'.                   
009800     88  INDATA-OK                           VALUE 'J'.                   
009900     88  INDATA-FEL                          VALUE 'N'.                   
010000                                                                          
010100 77  TRAEFF-SW                   PIC X       VALUE 'N'.                   
010200     88  TRAEFF                              VALUE 'J'.                   
010300                                                                          
010400 77  KOLLI-FINNS-SW              PIC X       VALUE 'N'.                   
010500     88  KOLLI-FINNS                         VALUE 'J'.                   
010600                                                                          
010700 77  KDCMD-FINNS-SW              PIC X       VALUE 'N'.                   
010800     88  KDCMD-FINNS                         VALUE 'J'.                   
010900                                                                          
011000 77  UPPD-SW                     PIC X       VALUE 'N'.                   
011100     88  UPPDATERING-GJORD                   VALUE 'J'.                   
011200                                                                          
011300 77  U-SW                        PIC X       VALUE 'N'.                   
011400     88  UTSKRIVEN                           VALUE 'J'.                   
011500                                                                          
011600 77  P-SW                        PIC X       VALUE 'N'.                   
011700     88  PACKAD                              VALUE 'J'.                   
011800                                                                          
011900 77  MARKERING-SW                PIC X       VALUE 'N'.                   
012000     88  INGEN-MARKERING-GJORD               VALUE 'N'.                   
012100                                                                          
012200 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
012300     88  NYCKLAR-OK                          VALUE 'J'.                   
012400     88  NYCKLAR-FEL                         VALUE 'N'.                   
012500                                                                          
012600 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
012700     88  EGEN-MID                            VALUE '6175'.                
012800     88  GODK-MID                            VALUE '6174' '6175'.         
012900     88  TVILLING-MID                        VALUE '6174'.                
013000     88  HELP-MID                            VALUE '0551'.                
013100     EJECT                                                                
013200 01     TEST-IDDISTR             PIC 9(5)    COMP-3.                      
013300 01     FILLER REDEFINES TEST-IDDISTR.                                    
013400*  03   -COPY WWDIST03.                                                   
013500 01     FILLER REDEFINES TEST-IDDISTR.                                    
013600*  03   -COPY WWDIST21.                                                   
013700 01  FILLER    REDEFINES TEST-IDDISTR.                                    
013800*  03   -COPY WWDIST85.                                                   
013900     EJECT                                                                
014000*    --- DIV DATUM-/TIDFÄLT                                               
014100 01  WS-DAGENS-KLOCKA            PIC 9(9).                                
014200 01  WS-DAGENS-DATUM.                                                     
014300     03 WS-DAGENS-DATUM-SS       PIC 9(2).                                
014400     03 WS-DAGENS-DATUM-AAMMDD   PIC 9(6).                                
014500 01  WS-DAGDAT-AAVVD             PIC 9(5).                                
014600 01  WS-DASUPREF.                                                         
014700     03  WS-SEKEL                PIC 9(2)    VALUE ZERO.                  
014800     03  WS-AAMMDD               PIC 9(6)    VALUE ZERO.                  
014900 01  WS-TITID                    PIC 9(7).                                
015000 01  WS-TITID-ALFA.                                                       
015100     03  WS-NOLL                 PIC X(1).                                
015200     03  WS-HHMM                 PIC 9(4).                                
015300     03  WS-SS                   PIC X(2).                                
015400 01  WS-TITID2                   PIC 9(5).                                
015500 01  WS-TITID2-ALFA.                                                      
015600     03  WS-NOLL                 PIC X(1).                                
015700     03  WS-HHMM2                PIC 9(4).                                
015800 01  WS-HHMM-DEC                 PIC 9(2)V9(2).                           
015900     EJECT                                                                
016000*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
016100 01  GENERELLA-SUBPROGRAM.                                                
016200     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
016300     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
016400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
016500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
016600     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
016700     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
016800     03  W403PLAT                PIC X(8)    VALUE 'W403PLAT'.            
016900     EJECT                                                                
017000*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
017100*01 -COPY WMEDAREA                                                        
017200                                                                          
017300 01  MESSAGE-CODES.                                                       
017400     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
017500     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
017600     03  INF-LAST-PAGE           PIC X(3)    VALUE '106'.                 
017700     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
017800     03  INF-UPDATE-NOT-DONE     PIC X(3)    VALUE '034'.                 
017900     03  INF-UPDATE-NOT-ALLOWED  PIC X(3)    VALUE '007'.                 
018000     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
018100     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
018200     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
018300     03  ERR-RECORD-MISSING      PIC X(3)    VALUE '029'.                 
018400     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
018500     03  ERR-ADDRESS-HANDLING    PIC X(3)    VALUE '730'.                 
018600     EJECT                                                                
018700 01  FILLER                      PIC X(16)   VALUE 'WDARAREA'.            
018800*01  -COPY WDATAREA                                                       
018900     EJECT                                                                
019000 01  FILLER                      PIC X(16)   VALUE 'WDECAREA'.            
019100*01  -COPY WDECAREA                                                       
019200     EJECT                                                                
019300 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
019400*01 -COPY WMSGINIT                                                        
019500     EJECT                                                                
019600 01  FILLER                      PIC X(16)   VALUE 'W403PLAT '.           
019700*01 -COPY W403PLAT                                                        
019800     EJECT                                                                
019900*    --- AREA MED DATA SOM SKA SPARAS MELLAN DIALOGSTEGEN                 
020000*                                                                         
020100 01  FILLER                      PIC X(8)   VALUE 'SPARAREA'.             
020200 01  SPAR-AREA.                                                           
020300     03  SPAR-IDTRANS            PIC X(4)   VALUE '6175'.                 
020400     03  SPAR-MID-IDLEVNR        PIC X(5)   VALUE SPACE.                  
020500     03  SPAR-MID-DASUPREF       PIC X(8)   VALUE SPACE.                  
020600     03  SPAR-MID-IDSUPREF       PIC X(10)  VALUE SPACE.                  
020700     03  SPAR-MID-IDDISTR        PIC X(4)   VALUE SPACE.                  
020800     03  SPAR-IDLEVNR-ENTER      PIC X(5)   VALUE SPACE.                  
020900     03  SPAR-IDLEVNR-NEXT       PIC X(5)   VALUE SPACE.                  
021000     03  SPAR-DASUPREF-ENTER     PIC 9(8)   VALUE ZERO.                   
021100     03  SPAR-DASUPREF-NEXT      PIC 9(8)   VALUE ZERO.                   
021200     03  SPAR-IDSUPREF-ENTER     PIC X(10)  VALUE SPACE.                  
021300     03  SPAR-IDSUPREF-NEXT      PIC X(10)  VALUE SPACE.                  
021400     03  SPAR-IDPRODNR-ENTER     PIC 9(7)   VALUE ZERO.                   
021500     03  SPAR-IDPRODNR-NEXT      PIC 9(7)   VALUE ZERO.                   
021600     03  SPAR-IDKOLLI-ENTER      PIC 9(5)   VALUE ZERO.                   
021700     03  SPAR-IDKOLLI-NEXT       PIC 9(5)   VALUE ZERO.                   
021800     03  SPAR-UPPD-NYCKLAR OCCURS 15.                                     
021900         05  SPAR-UPPD-IDPRODNR  PIC 9(7)   VALUE ZERO.                   
022000         05  SPAR-UPPD-IDKOLLI   PIC 9(5)   VALUE ZERO.                   
022100     EJECT                                                                
022200*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
022300*                                                                         
022400 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
022500                                                                          
022600*01  MID -COPY W6I17501                                                   
022700     EJECT                                                                
022800 01  FILLER                      PIC X(16)   VALUE 'MSG/MOD-AREA'.        
022900                                                                          
023000*01  -COPY WMSGAREA                                                       
023100     EJECT                                                                
023200     03  MOD REDEFINES MSG-AREA.                                          
023300*      05  -COPY W6O17501                                                 
023400     EJECT                                                                
023500 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
023600                                                                          
023700*01  -COPY WMFSAREA                                                       
023800     EJECT                                                                
023900*                                                                         
024000 01  FILLER                      PIC X(16)  VALUE '4322-ARB-AREA'.        
024100* ARBETSAREA XXJK-WDGX4322                                                
024200 01  -COPY WDGX4322    -PRE XXJK-                                         
024300*                                                                         
024400*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
024500*                                                                         
024600     EJECT                                                                
024700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
024800                                                                          
024900 01  NYCKLAR-TILL-DLI.                                                    
025000*    --- VÄRDE PÅ BLÄDDRINGSNYCKEL FÖR FÖRSTA RADEN PÅ SKÄRMEN            
025100                                                                          
025200     03  W-WDE6F1KY-MIN-X.                                                
025300         05  W-IDLEVNR-MIN       PIC X(5)    VALUE LOW-VALUE.             
025400         05  W-DASUPREF-MIN      PIC 9(8)    VALUE ZERO.                  
025500         05  W-IDSUPREF-MIN      PIC X(10)   VALUE LOW-VALUE.             
025600         05  W-IDPRODNR-MIN      PIC S9(7) VALUE ZERO COMP-3.             
025700         05  W-IDKOLLI-MIN       PIC S9(5) VALUE ZERO COMP-3.             
025800                                                                          
025900     03  W-WDE6F1KY-MAX-X.                                                
026000         05  W-IDLEVNR-MAX       PIC X(5)    VALUE HIGH-VALUE.            
026100         05  W-DASUPREF-MAX      PIC 9(8)    VALUE 99999999.              
026200         05  W-IDSUPREF-MAX      PIC X(10)   VALUE HIGH-VALUE.            
026300         05  FILLER              PIC X(7)    VALUE HIGH-VALUE.            
026400                                                                          
026500     03  W-IDPRODNR-ESEQ-X.                                               
026600         05  W-IDPRODNR-ESEQ     PIC S9(7)   VALUE ZERO COMP-3.           
026700                                                                          
026800     03  W-IDPRODNR-X.                                                    
026900         05  W-IDPRODNR          PIC S9(7)   VALUE ZERO COMP-3.           
027000                                                                          
027100     03  W-IDKOLLI-X.                                                     
027200         05  W-IDKOLLI           PIC S9(5)   VALUE ZERO COMP-3.           
027300                                                                          
027400     03  W-IDDISTR-F1-X.                                                  
027500         05  W-IDDISTR-F1        PIC S9(5)   VALUE ZERO COMP-3.           
027600                                                                          
027700     03  W-IDSUPREF-F1-X.                                                 
027800         05  W-IDSUPREF-F1       PIC X(10)   VALUE SPACE.                 
027900                                                                          
028000     03  W-KDSEGKEY-X.                                                    
028010         05 W-KDSEGKEY           PIC X(1)    VALUE '1'.                   
028100*                                                                         
028200     03  W-4726-WDGXKEY-ROT-X.                                            
028300         05    W-4726-IDHTYP     PIC X(4)    VALUE '4726'.                
028400         05    W-4726-FLBATCH    PIC X(1)    VALUE SPACE.                 
028500         05    W-4726-LOWVALUE   PIC X(25)   VALUE LOW-VALUE.             
028600*                                                                         
028700     03  W-4726-WDGXKEY-UNDSEG-X.                                         
028800         05    W-4726-IDDISTR    PIC S9(5)   COMP-3.                      
028900         05    W-4726-IDKUNDNR   PIC S9(7)   COMP-3.                      
029000         05    W-4726-IDDC       PIC X(2).                                
029100         05    W-4726-KDFAKTYP   PIC X.                                   
029200*                                                                         
029300   03    W-4321-IDHTYP-X.                                                 
029400         05  W-4321-IDHTYP         PIC X(4)  VALUE '4321'.                
029500         05  W-4321-NYCKEL-VALFRI  PIC X(26) VALUE LOW-VALUE.             
029600*                                                                         
029700     EJECT                                                                
029800*    --- STATUS-KOD FRÅN IMS                                              
029900 01  STATUS-WS                   PIC XX.                                  
030000     88  SEGMENT-FINNS                       VALUE '  '.                  
030100     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
030200     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
030300     88  SEGMENT-SLUT                        VALUE 'GB'.                  
030400                                                                          
030500 01  GODK-STATUSKODER.                                                    
030600     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
030700                                                                          
030800 01  SSA1                        PIC X(160).                              
030900 01  SSA2                        PIC X(64).                               
031000 01  SSA3                        PIC X(64).                               
031100     EJECT                                                                
031200*    --- IMS FUNKTIONSKODER                                               
031300*01  -COPY W0003                                                          
031400     EJECT                                                                
031500*    ---  DLI INPUT-OUTPUT AREA                                           
031600                                                                          
031700 01  FILLER          PIC X(16) VALUE 'DLI-IO-E401'.                       
031800 01  DLI-IO-E401.                                                         
031900*    03  -COPY WDE401                                                     
032000     EJECT                                                                
032100 01  FILLER          PIC X(16) VALUE 'DLI-IO-E601'.                       
032200 01  DLI-IO-E601.                                                         
032300*    03  -COPY WDE601                                                     
032400     EJECT                                                                
032500 01  FILLER          PIC X(16) VALUE 'DLI-IO-E611'.                       
032600 01  DLI-IO-E611.                                                         
032700*    03  -COPY WDE611                                                     
032800     EJECT                                                                
032810 01  FILLER               PIC X(16)   VALUE 'WDE621  '.                   
032820 01  DLI-IO-WDE621.                                                       
032830*    03  -COPY WDE621                                                     
032840                                                                          
032900 01  FILLER          PIC X(16) VALUE 'DLI-IO-WDE6F1'.                     
033000 01  DLI-IO-WDE6F1.                                                       
033100*    03  -COPY WDE6F1                                                     
033200     EJECT                                                                
033300 01  FILLER          PIC X(16) VALUE 'DLI-IO-WLXXDV01'.                   
033400 01  DLI-IO-WLXXDV01 PIC X(500).                                          
033500     EJECT                                                                
033600 01  FILLER          PIC X(16) VALUE 'DLI-IO-WLXXDV11'.                   
033700 01  DLI-IO-WLXXDV11.                                                     
033800*    03  -COPY WDGX4726                                                   
033900     EJECT                                                                
034000 01  FILLER          PIC X(16) VALUE 'DLI-IO-WLXXDV21'.                   
034100 01  DLI-IO-WLXXDV21.                                                     
034200*    03  -COPY WDGX4727                                                   
034300     EJECT                                                                
034400 01  FILLER          PIC X(16) VALUE 'DLI-IO-WLFILA01'.                   
034500*01  WLFILA01    -COPY WDR601                                             
034600*    05 -COPY W46341 -PRE LOGG- -RED FIL-WDR601-DATA                      
034700     EJECT                                                                
034800 01  FILLER                      PIC X(16)   VALUE '4322-AREA'.           
034900 01  -COPY WDGX01      -PRE 4321-                                         
035000 01  -COPY WDGX4322                                                       
035100     EJECT                                                                
035200 LINKAGE SECTION.                                                         
035300*01  -COPY W0009  -PRE MSG-                                               
035400                                                                          
035500*01  -COPY W0008  -PRE USEA-                                              
035600     05  FILLER                  PIC X.                                   
035700                                                                          
035800*01  -COPY W0008  -PRE WDE6F-                                             
035900     05  FILLER                  PIC X.                                   
036000                                                                          
036100*01  -COPY W0008  -PRE WDE4-                                              
036200     05  FILLER                  PIC X.                                   
036300                                                                          
036400*01  -COPY W0008  -PRE WDE6-                                              
036500     05  FILLER                  PIC X.                                   
036600                                                                          
036700*01  -COPY W0008  -PRE XXDV-                                              
036800     05  FILLER                  PIC X.                                   
036900     EJECT                                                                
037000*01  -COPY W0008  -PRE PLATS-DM-                                          
037100     05  FILLER                  PIC X.                                   
037200*01  -COPY W0008  -PRE PLATS-DN-                                          
037300     05  FILLER                  PIC X.                                   
037400*01  -COPY W0008  -PRE PLATS-DP-                                          
037500     05  FILLER                  PIC X.                                   
037600*01  -COPY W0008  -PRE PLATS-DO-                                          
037700     05  FILLER                  PIC X.                                   
037800*01  -COPY W0008  -PRE PLATS-WDE6C-                                       
037900     05  FILLER                  PIC X.                                   
038000*01  -COPY W0008  -PRE PLATS-GMTC-                                        
038100     05  FILLER                  PIC X.                                   
038200     EJECT                                                                
038300*01  -COPY W0008  -PRE PLATS-WDB6-                                        
038400     05  FILLER                  PIC X.                                   
038500     EJECT                                                                
038600*01  -COPY W0008  -PRE FILA-                                              
038700     05  FILLER                  PIC X.                                   
038800     EJECT                                                                
038900*01  -COPY W0008  -PRE 4322-                                              
039000     05  FILLER                  PIC X.                                   
039100     EJECT                                                                
039200 PROCEDURE DIVISION  USING MSG-PCB       USEA-PCB                         
039300                           WDE6F-PCB     WDE4-PCB WDE6-PCB                
039400                           XXDV-PCB                                       
039500                           PLATS-DM-PCB  PLATS-DN-PCB                     
039600                           PLATS-DP-PCB  PLATS-DO-PCB                     
039700                           PLATS-WDE6C-PCB PLATS-GMTC-PCB                 
039800                           PLATS-WDB6-PCB  FILA-PCB 4322-PCB.             
039900 MAIN SECTION.                                                            
040000     ENTRY 'DLITCBL' USING MSG-PCB       USEA-PCB                         
040100                           WDE6F-PCB     WDE4-PCB WDE6-PCB                
040200                           XXDV-PCB                                       
040300                           PLATS-DM-PCB  PLATS-DN-PCB                     
040400                           PLATS-DP-PCB  PLATS-DO-PCB                     
040500                           PLATS-WDE6C-PCB PLATS-GMTC-PCB                 
040600                           PLATS-WDB6-PCB  FILA-PCB 4322-PCB.             
040700                                                                          
040800     PERFORM IMS-GET-MSG                                                  
040900     IF SEGMENT-FINNS                                                     
041000       PERFORM A-INIT                                                     
041100                                                                          
041200       PERFORM B-KOLLA-NYCKLAR                                            
041300       IF NYCKLAR-OK                                                      
041400         IF MFS-UPDATE                                                    
041500           PERFORM G-KOLLA-INPUT                                          
041600           IF INDATA-OK                                                   
041700             PERFORM H-UPPDATERA                                          
041800           END-IF                                                         
041900         ELSE                                                             
042000           IF MFS-FIRST                                                   
042100             PERFORM C-FOERSTA-SIDA                                       
042200           ELSE                                                           
042300             IF MFS-NEXT                                                  
042400               PERFORM D-NAESTA-SIDA                                      
042500             ELSE                                                         
042600               PERFORM E-SAMMA-SIDA                                       
042700             END-IF                                                       
042800           END-IF                                                         
042900         END-IF                                                           
043000         IF INDATA-OK                                                     
043100           PERFORM F-LAES-VISA-INFO                                       
043200         END-IF                                                           
043300       END-IF                                                             
043400       COMPUTE MSG-KVLL = LENGTH OF MOD-W6O17501 + 4                      
043500       PERFORM IMS-INSERT-MSG                                             
043600     END-IF                                                               
043700                                                                          
043800     MOVE ZERO TO RETURN-CODE                                             
043900     GOBACK                                                               
044000     .                                                                    
044100     EJECT                                                                
044200 A-INIT SECTION.                                                          
044300                                                                          
044400     IF MSG-DUBBLA-TRANSKODER                                             
044500       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W6I17501                 
044600       MOVE MSG-IDTRANS-2                 TO MFS-IDTRANS                  
044700       MOVE MSG-KDMFSFOR-2                TO MFS-KDMFSFOR                 
044800     ELSE                                                                 
044900       MOVE MSG-INDATA-MINUS-1-TRANSKOD   TO MID-W6I17501                 
045000       MOVE MSG-IDTRANS-1                 TO MFS-IDTRANS                  
045100       MOVE MSG-KDMFSFOR-1                TO MFS-KDMFSFOR                 
045200     END-IF                                                               
045300                                                                          
045400     MOVE MSG-KDTRTYP                     TO MFS-KDTRTYP                  
045500     MOVE MSG-IDPFK                       TO MFS-IDPFK                    
045600     MOVE MFS-IDTRANS                     TO W-IDTRANS                    
045700                                                                          
045800     MOVE LOW-VALUE                       TO MSG-AREA                     
045900     MOVE 'W6O175N1'                      TO MFS-IDMOD                    
046000     MOVE '6175'                          TO MOD-IDTRANS                  
046100     MOVE MFS-RENSA-FAELT                 TO MOD-TEMFSFEL                 
046200                                             MOD-TEMFSINF                 
046300                                                                          
046400     MOVE SPACE                           TO MED-IDMFSFEL                 
046500                                             MED-IDMFSINF                 
046600                                                                          
046700     IF EGEN-MID OR HELP-MID                                              
046800       CONTINUE                                                           
046900     ELSE                                                                 
047000       MOVE SPACE                         TO MFS-KDTRTYP                  
047100       MOVE '7'                           TO MFS-IDPFK                    
047200     END-IF                                                               
047300     MOVE 'GB'                            TO MED-IDSKYLT                  
047400                                                                          
047500     MOVE LOW-VALUE                       TO W-WDE6F1KY-MIN-X             
047600                                                                          
047700     MOVE HIGH-VALUE                      TO W-WDE6F1KY-MAX-X             
047800                                                                          
047900     MOVE FUNCTION CURRENT-DATE(1:8)      TO WS-DAGENS-DATUM              
048000     ACCEPT WS-DAGENS-KLOCKA              FROM TIME                       
048100                                                                          
048200     MOVE 'IDAG'      TO DAT-KDDATFORM                                    
048300     CALL WDATKONV USING DAT-KDDATFORM                                    
048400                         DAT-I-TIDATUM                                    
048500                         DAT-O-TIDATUM                                    
048600                         DAT-KDSVAR                                       
048700     MOVE DAT-TIAAVVD TO WS-DAGDAT-AAVVD                                  
048800     .                                                                    
048900     EJECT                                                                
049000 B-KOLLA-NYCKLAR SECTION.                                                 
049100                                                                          
049200     MOVE ALL '+'               TO MSGI-WMSGINIT                          
049300     MOVE '001'                 TO MSGI-KDCALL                            
049400     MOVE MSG-LTERM-NAME        TO MSGI-IDLTERM-USER                      
049500     MOVE MSG-SIGNON-USERID     TO MSGI-IDUSER                            
049600     MOVE '6175'                TO MSGI-IDTRANS                           
049700                                                                          
049800     IF EGEN-MID                                                          
049900       MOVE MID-IDLEVNR-IN      TO MSGI-IDLEVNR                           
050000       MOVE MID-IDDISTR-IN      TO MSGI-IDDISTR                           
050100     END-IF                                                               
050200     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
050300     MOVE MSGI-SPAR-AREA        TO SPAR-AREA                              
050400                                                                          
050500     MOVE JA                    TO NYCKLAR-SW                             
050600                                                                          
050700     MOVE MSGI-IDLEVNR          TO WS-IDLEVNR                             
050800     MOVE MSGI-IDDISTR          TO WS-IDDISTR-IN                          
050900                                                                          
051000*    -- KONTROLL AV MID-INPUT                                             
051100                                                                          
051200     IF TVILLING-MID                                                      
051300       MOVE SPAR-MID-IDLEVNR    TO MID-IDLEVNR-IN                         
051400       MOVE SPAR-MID-DASUPREF   TO MID-TISUPREF-IN                        
051500       MOVE SPACE               TO MID-TISUPREF-UT                        
051600       MOVE SPAR-MID-IDSUPREF   TO MID-IDSUPREF-IN                        
051700       MOVE SPACE               TO MID-IDSUPREF-UT                        
051800       MOVE ALL '+'             TO MID-IDDISTR-IN                         
051900       MOVE SPACE               TO MID-IDDISTR-UT                         
052000       MOVE SPACE               TO SPAR-MID-IDLEVNR                       
052100       MOVE ALL '+'             TO SPAR-MID-DASUPREF                      
052200                                   SPAR-MID-IDSUPREF                      
052300                                   SPAR-MID-IDDISTR                       
052400     END-IF                                                               
052500                                                                          
052600     IF EGEN-MID                                                          
052700       MOVE SPACE               TO SPAR-MID-IDLEVNR                       
052800       MOVE ALL '+'             TO SPAR-MID-DASUPREF                      
052900                                   SPAR-MID-IDSUPREF                      
053000                                   SPAR-MID-IDDISTR                       
053100     END-IF                                                               
053200     IF NOT GODK-MID                                                      
053300       MOVE ALL '+'             TO MID-TISUPREF-IN                        
053400                                   MID-IDSUPREF-IN                        
053500                                   MID-IDDISTR-IN                         
053600       MOVE SPACE               TO MID-TISUPREF-UT                        
053700                                   MID-IDSUPREF-UT                        
053800                                   MID-IDDISTR-UT                         
053900     END-IF                                                               
054000                                                                          
054100     IF MID-IDLEVNR-IN  NOT = ALL '+' OR                                  
054200        MID-TISUPREF-IN NOT = ALL '+' OR                                  
054300        MID-IDSUPREF-IN NOT = ALL '+' OR                                  
054400        MID-IDDISTR-IN  NOT = ALL '+'                                     
054500       MOVE '7'                 TO MFS-IDPFK                              
054600       MOVE SPACE               TO MFS-KDTRTYP                            
054700     END-IF                                                               
054800                                                                          
054900*    -- KONTROLL AV IDLEVNR                                               
055000     MOVE MFS-RENSA-FAELT       TO MOD-IDLEVNR-IN                         
055100                                                                          
055200       MOVE WS-IDLEVNR          TO W-IDLEVNR-MIN                          
055300                                   W-IDLEVNR-MAX                          
055400                                   LOGG-IDLEVNR                           
055500       MOVE W-IDLEVNR-MIN       TO SPAR-MID-IDLEVNR                       
055600       MOVE WS-IDLEVNR          TO MOD-IDLEVNR-UT                         
055700                                                                          
055800                                                                          
055900*    -- KONTROLL AV TISUPREF                                              
056000     MOVE MFS-RENSA-FAELT       TO MOD-TISUPREF-IN                        
056100                                                                          
056200     IF MID-TISUPREF-IN = ALL '+' AND                                     
056300        MID-TISUPREF-UT NOT = SPACE                                       
056400       MOVE MID-TISUPREF-UT     TO MID-TISUPREF-IN                        
056500     END-IF                                                               
056600                                                                          
056700     IF MID-TISUPREF-IN NOT = ALL '+'                                     
056800       INSPECT MID-TISUPREF-IN REPLACING ALL SPACE BY ZERO                
056900                                                                          
057000       IF MID-TISUPREF-IN NUMERIC AND MID-TISUPREF-IN > ZERO              
057100         MOVE 'AAMMDD'            TO DAT-KDDATFORM                        
057200         MOVE MID-TISUPREF-IN     TO DAT-I-TIDATUM                        
057300         CALL WDATKONV USING DAT-KDDATFORM,                               
057400                             DAT-I-TIDATUM,                               
057500                             DAT-O-TIDATUM,                               
057600                             DAT-KDSVAR                                   
057700         IF DAT-KDSVAR = 'F'                                              
057800           MOVE NEJ TO NYCKLAR-SW                                         
057900           MOVE MID-TISUPREF-IN   TO MOD-TISUPREF-UT                      
058000         ELSE                                                             
058100           IF MID-TISUPREF-IN < '500000'                                  
058200             MOVE 20              TO WS-SEKEL                             
058300           ELSE                                                           
058400             MOVE 19              TO WS-SEKEL                             
058500           END-IF                                                         
058600           MOVE MID-TISUPREF-IN   TO WS-AAMMDD                            
058700                                     MOD-TISUPREF-UT                      
058800                                     SPAR-MID-DASUPREF                    
058900           MOVE WS-DASUPREF       TO W-DASUPREF-MIN                       
059000                                     W-DASUPREF-MAX                       
059100         END-IF                                                           
059200       ELSE                                                               
059300         MOVE ZERO                TO WS-SEKEL                             
059400                                     WS-AAMMDD                            
059500         MOVE SPACE               TO MOD-TISUPREF-UT                      
059600       END-IF                                                             
059700     ELSE                                                                 
059800       MOVE ZERO                TO WS-SEKEL                               
059900                                   WS-AAMMDD                              
060000       MOVE SPACE               TO MOD-TISUPREF-UT                        
060100     END-IF                                                               
060200                                                                          
060300*    -- KONTROLL AV IDSUPREF                                              
060400     MOVE MFS-RENSA-FAELT       TO MOD-IDSUPREF-IN                        
060500                                                                          
060600     IF MID-IDSUPREF-IN = ALL '+' AND                                     
060700        MID-IDSUPREF-UT NOT = SPACE                                       
060800       MOVE MID-IDSUPREF-UT     TO MID-IDSUPREF-IN                        
060900     END-IF                                                               
061000     MOVE MID-IDSUPREF-IN       TO SPAR-MID-IDSUPREF                      
061100     IF MID-IDSUPREF-IN NOT = ALL '+' AND ALL ' '                         
061200         MOVE MID-IDSUPREF-IN   TO MOD-IDSUPREF-UT                        
061300                                   W-IDSUPREF-F1                          
061400     ELSE                                                                 
061500       MOVE SPACE               TO MOD-IDSUPREF-UT                        
061600                                   W-IDSUPREF-F1                          
061700     END-IF                                                               
061800                                                                          
061900*    -- KONTROLL AV IDDISTR                                               
062000     MOVE MFS-RENSA-FAELT       TO MOD-IDDISTR-IN                         
062100                                                                          
062200     IF MID-IDDISTR-IN = ALL '+' AND                                      
062300        MID-IDDISTR-UT NOT = SPACE                                        
062400       MOVE MID-IDDISTR-UT     TO MID-IDDISTR-IN                          
062500     END-IF                                                               
062600     MOVE MID-IDDISTR-IN       TO SPAR-MID-IDDISTR                        
062700     IF MID-IDDISTR-IN NOT = ALL '+'                                      
062800       INSPECT MID-IDDISTR-IN REPLACING LEADING SPACE BY ZERO             
062900       IF MID-IDDISTR-IN NUMERIC AND MID-IDDISTR-IN > ZERO                
063000         MOVE MID-IDDISTR-IN   TO MOD-IDDISTR-UT                          
063100                                  W-IDDISTR-F1                            
063200         INSPECT MOD-IDDISTR-UT REPLACING LEADING ZERO BY SPACE           
063300       ELSE                                                               
063400         MOVE NEJ TO NYCKLAR-SW                                           
063500       END-IF                                                             
063600     ELSE                                                                 
063700       MOVE SPACE               TO MOD-IDDISTR-UT                         
063800       MOVE ZERO                TO W-IDDISTR-F1                           
063900     END-IF                                                               
064000                                                                          
064100     IF NYCKLAR-FEL                                                       
064200       MOVE ERR-WRONG-KEY       TO MED-IDMFSFEL                           
064300       CALL WMEDKONV USING MED-WMEDAREA                                   
064400       MOVE MED-MFSFEL          TO MOD-TEMFSFEL                           
064500       PERFORM MFS-RENSA-FAELT-IN                                         
064600       PERFORM MFS-RENSA-FAELT-UT                                         
064700     END-IF                                                               
064800     .                                                                    
064900     EJECT                                                                
065000 C-FOERSTA-SIDA SECTION.                                                  
065100                                                                          
065200     MOVE INF-FIRST-PAGE TO MED-IDMFSINF                                  
065300     CALL WMEDKONV USING MED-WMEDAREA                                     
065400     MOVE MED-MFSINF     TO MOD-TEMFSFEL                                  
065500                                                                          
065600     PERFORM MFS-RENSA-FAELT-IN                                           
065700     .                                                                    
065800     EJECT                                                                
065900 D-NAESTA-SIDA SECTION.                                                   
066000                                                                          
066100     IF SPAR-IDTRANS = '6175'                                             
066200       MOVE SPAR-IDLEVNR-NEXT  TO W-IDLEVNR-MIN                           
066300       MOVE SPAR-DASUPREF-NEXT TO W-DASUPREF-MIN                          
066400       MOVE SPAR-IDSUPREF-NEXT TO W-IDSUPREF-MIN                          
066500       MOVE SPAR-IDPRODNR-NEXT TO W-IDPRODNR-MIN                          
066600       MOVE SPAR-IDKOLLI-NEXT  TO W-IDKOLLI-MIN                           
066700     ELSE                                                                 
066800       PERFORM MFS-RENSA-FAELT-IN                                         
066900     END-IF                                                               
067000     .                                                                    
067100     EJECT                                                                
067200 E-SAMMA-SIDA SECTION.                                                    
067300                                                                          
067400     MOVE NEJ         TO KDCMD-FINNS-SW                                   
067500                                                                          
067600     IF SPAR-IDTRANS = '6175' OR '0551'                                   
067700       MOVE SPAR-IDLEVNR-ENTER  TO W-IDLEVNR-MIN                          
067800       MOVE SPAR-DASUPREF-ENTER TO W-DASUPREF-MIN                         
067900       MOVE SPAR-IDSUPREF-ENTER TO W-IDSUPREF-MIN                         
068000       MOVE SPAR-IDPRODNR-ENTER TO W-IDPRODNR-MIN                         
068100       MOVE SPAR-IDKOLLI-ENTER  TO W-IDKOLLI-MIN                          
068200                                                                          
068300       MOVE +1 TO INDX                                                    
068400       PERFORM UNTIL INDX > MAX-INDX                                      
068500                                                                          
068600         IF MID-CMD(INDX) = '+'                                           
068700           MOVE MFS-RENSA-FAELT TO MOD-CMD(INDX)                          
068800         ELSE                                                             
068900           MOVE MFS-ADD-LAES-IN-FAELT TO MOD-CMD-ATTR(INDX)               
069000           MOVE MFS-ROER-EJ-FAELT TO MOD-CMD(INDX)                        
069100           MOVE JA   TO KDCMD-FINNS-SW                                    
069200         END-IF                                                           
069300         ADD +1 TO INDX                                                   
069400       END-PERFORM                                                        
069500                                                                          
069600       IF KDCMD-FINNS                                                     
069700         MOVE INF-PRESS-PF11      TO MED-IDMFSINF                         
069800         CALL WMEDKONV USING MED-WMEDAREA                                 
069900         MOVE MED-MFSINF          TO MOD-TEMFSFEL                         
070000       END-IF                                                             
070100     ELSE                                                                 
070200       PERFORM MFS-RENSA-FAELT-IN                                         
070300     END-IF                                                               
070400     .                                                                    
070500     EJECT                                                                
070600 F-LAES-VISA-INFO SECTION.                                                
070700     MOVE 'F-LAES-VISA-INFO'       TO CURRENT-SECTION                     
070800                                                                          
070900     MOVE +1 TO INDX                                                      
071000     PERFORM UNTIL INDX > MAX-INDX                                        
071100       MOVE ZERO                  TO SPAR-UPPD-IDPRODNR (INDX)            
071200                                     SPAR-UPPD-IDKOLLI  (INDX)            
071300       ADD +1 TO INDX                                                     
071400     END-PERFORM                                                          
071500                                                                          
071600     MOVE +1 TO INDX                                                      
071700     PERFORM FA-LAES-RADDATA-GU                                           
071800     IF KOLLI-FINNS                                                       
071900       MOVE SEQF-IDLEVNR          TO SPAR-IDLEVNR-ENTER                   
072000       MOVE SEQF-DASUPREF         TO SPAR-DASUPREF-ENTER                  
072100       MOVE SEQF-IDSUPREF         TO SPAR-IDSUPREF-ENTER                  
072200       MOVE SEQF-IDPRODNR         TO SPAR-IDPRODNR-ENTER                  
072300       MOVE SEQF-IDKOLLI          TO SPAR-IDKOLLI-ENTER                   
072400     ELSE                                                                 
072500       MOVE W-IDLEVNR-MIN         TO SPAR-IDLEVNR-ENTER                   
072600       MOVE W-DASUPREF-MIN        TO SPAR-DASUPREF-ENTER                  
072700       MOVE W-IDSUPREF-MIN        TO SPAR-IDSUPREF-ENTER                  
072800       MOVE W-IDPRODNR-MIN        TO SPAR-IDPRODNR-ENTER                  
072900       MOVE W-IDKOLLI-MIN         TO SPAR-IDKOLLI-ENTER                   
073000       MOVE ZERO                  TO SPAR-UPPD-IDPRODNR (1)               
073100                                     SPAR-UPPD-IDKOLLI  (1)               
073200       MOVE +20 TO INDX                                                   
073300     END-IF                                                               
073400                                                                          
073500     PERFORM UNTIL INDX > MAX-INDX                                        
073600       IF KOLLI-FINNS                                                     
073700         MOVE KOLLI-DASUPREF       TO WS-DASUPREF                         
073800         MOVE WS-AAMMDD            TO MOD-TISUPREF       (INDX)           
073900         MOVE KOLLI-TISUPTID       TO WS-TITID2                           
074000         MOVE WS-TITID2            TO WS-TITID2-ALFA                      
074100         COMPUTE WS-HHMM-DEC = WS-HHMM2 / 100                             
074200         MOVE WS-HHMM-DEC          TO MOD-TISUPTID       (INDX)           
074300         MOVE KOLLI-IDSUPREF       TO MOD-IDSUPREF       (INDX)           
074400         MOVE KOLLI-TIPACKN        TO MOD-TIPACKN        (INDX)           
074500         MOVE KOLLI-TIPACTID       TO WS-TITID                            
074600         MOVE WS-TITID             TO WS-TITID-ALFA                       
074700         COMPUTE WS-HHMM-DEC = WS-HHMM / 100                              
074800         MOVE WS-HHMM-DEC          TO MOD-TIPACTID       (INDX)           
074900         MOVE KOLLI-IDDISTR        TO MOD-IDDISTR        (INDX)           
075000         MOVE KOLLI-IDKUNDNR       TO MOD-IDKUNDNR       (INDX)           
075100         MOVE KORD-IDORDNR5        TO MOD-IDORDNR5       (INDX)           
075200         MOVE KOLLI-IDKOLLI        TO MOD-IDKOLLI        (INDX)           
075300         MOVE KOLLI-VKORDBTO-KOLLI TO MOD-VKORDBTO-KOLLI (INDX)           
075400         MOVE KOLLI-VLORDBTO-KOLLI TO MOD-VLORDBTO-KOLLI (INDX)           
075500         ADD +1 TO INDX                                                   
075600         PERFORM FB-LAES-RADDATA-GN                                       
075700       ELSE                                                               
075800         MOVE MFS-RENSA-FAELT      TO MOD-TISUPREF       (INDX)           
075900                                      MOD-TISUPTID       (INDX)           
076000                                      MOD-IDSUPREF       (INDX)           
076100                                      MOD-TIPACKN        (INDX)           
076200                                      MOD-TIPACTID       (INDX)           
076300                                      MOD-IDDISTR        (INDX)           
076400                                      MOD-IDKUNDNR       (INDX)           
076500                                      MOD-IDORDNR5       (INDX)           
076600                                      MOD-IDKOLLI        (INDX)           
076700                                      MOD-VKORDBTO-KOLLI (INDX)           
076800                                      MOD-VLORDBTO-KOLLI (INDX)           
076900         ADD +1 TO INDX                                                   
077000       END-IF                                                             
077100     END-PERFORM                                                          
077200                                                                          
077300     IF KOLLI-FINNS                                                       
077400       MOVE SEQF-IDLEVNR           TO SPAR-IDLEVNR-NEXT                   
077500       MOVE SEQF-DASUPREF          TO SPAR-DASUPREF-NEXT                  
077600       MOVE SEQF-IDSUPREF          TO SPAR-IDSUPREF-NEXT                  
077700       MOVE SEQF-IDPRODNR          TO SPAR-IDPRODNR-NEXT                  
077800       MOVE SEQF-IDKOLLI           TO SPAR-IDKOLLI-NEXT                   
077900                                                                          
078000       IF MED-IDMFSINF = SPACE OR '006'                                   
078100         MOVE INF-MORE-INFO-EXISTS   TO MED-IDMFSINF                      
078200         CALL WMEDKONV USING MED-WMEDAREA                                 
078300         MOVE MED-MFSINF             TO MOD-TEMFSINF                      
078400       END-IF                                                             
078500     ELSE                                                                 
078600       IF INDX = 20                                                       
078700         MOVE ERR-RECORD-MISSING   TO MED-IDMFSFEL                        
078800         CALL WMEDKONV USING MED-WMEDAREA                                 
078900         MOVE MED-MFSFEL           TO MOD-TEMFSFEL                        
079000         PERFORM MFS-RENSA-FAELT-UT                                       
079100         PERFORM MFS-RENSA-FAELT-IN                                       
079200       ELSE                                                               
079300         MOVE SEQF-IDLEVNR         TO SPAR-IDLEVNR-NEXT                   
079400         MOVE SEQF-DASUPREF        TO SPAR-DASUPREF-NEXT                  
079500         MOVE SEQF-IDSUPREF        TO SPAR-IDSUPREF-NEXT                  
079600         MOVE SEQF-IDPRODNR        TO SPAR-IDPRODNR-NEXT                  
079700         MOVE SEQF-IDKOLLI         TO SPAR-IDKOLLI-NEXT                   
079800                                                                          
079900         IF MED-IDMFSINF = SPACE                                          
080000           MOVE INF-LAST-PAGE  TO MED-IDMFSINF                            
080100           CALL WMEDKONV USING MED-WMEDAREA                               
080200           MOVE MED-MFSINF TO MOD-TEMFSINF                                
080300         END-IF                                                           
080400       END-IF                                                             
080500     END-IF                                                               
080600                                                                          
080700     MOVE '002'                    TO MSGI-KDCALL                         
080800     MOVE '6175'                   TO SPAR-IDTRANS                        
080900     MOVE SPAR-AREA                TO MSGI-SPAR-AREA                      
081000     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
081100     .                                                                    
081200     EJECT                                                                
081300 FA-LAES-RADDATA-GU SECTION.                                              
081400     MOVE 'FA-LAES-RADDATA-GU'     TO CURRENT-SECTION                     
081500                                                                          
081600     MOVE NEJ                      TO KOLLI-FINNS-SW                      
081700                                                                          
081800     IF W-IDDISTR-F1 > ZERO                                               
081900       IF W-IDSUPREF-F1 NOT = SPACE                                       
082000         PERFORM IMS-GU-DISTR-2-WDE6F1                                    
082100       ELSE                                                               
082200         PERFORM IMS-GU-DISTR-WDE6F1                                      
082300       END-IF                                                             
082400     ELSE                                                                 
082500       IF W-IDSUPREF-F1 NOT = SPACE                                       
082600         PERFORM IMS-GU-SUPREF-WDE6F1                                     
082700       ELSE                                                               
082800         PERFORM IMS-GU-KOLLI-WDE6F1                                      
082900       END-IF                                                             
083000     END-IF                                                               
083100                                                                          
083200     IF SEGMENT-FINNS                                                     
083300       MOVE JA                 TO KOLLI-FINNS-SW                          
083400       MOVE SEQF-IDPRODNR      TO W-IDPRODNR                              
083500       MOVE SEQF-IDKOLLI       TO W-IDKOLLI                               
083600       PERFORM IMS-GU-WDE611                                              
083700       MOVE '60175-1'          TO IDPGM                                   
083800       MOVE SEQF-IDPRODNR      TO W-IDPRODNR-ESEQ                         
083900       PERFORM IMS-GU-WDE401-ESEQ                                         
084000       IF SEGMENT-SAKNAS                                                  
084100         MOVE ZERO             TO KORD-IDORDNR5                           
084200       END-IF                                                             
084300* SPARA NYCKLAR TILL EV. KOMMANDE UPPDATERING AV WDE601/WDE610            
084400       MOVE SEQF-IDPRODNR      TO SPAR-UPPD-IDPRODNR (INDX)               
084500       MOVE KOLLI-IDKOLLI      TO SPAR-UPPD-IDKOLLI  (INDX)               
084600     END-IF                                                               
084700                                                                          
084800     .                                                                    
084900     EJECT                                                                
085000 FB-LAES-RADDATA-GN SECTION.                                              
085100     MOVE 'FB-LAES-RADDATA-GN '    TO CURRENT-SECTION                     
085200                                                                          
085300     MOVE NEJ                      TO KOLLI-FINNS-SW                      
085400                                                                          
085500     IF W-IDDISTR-F1 > ZERO                                               
085600       IF W-IDSUPREF-F1 NOT = SPACE                                       
085700         PERFORM IMS-GN-DISTR-2-WDE6F1                                    
085800       ELSE                                                               
085900         PERFORM IMS-GN-DISTR-WDE6F1                                      
086000       END-IF                                                             
086100     ELSE                                                                 
086200       IF W-IDSUPREF-F1 NOT = SPACE                                       
086300         PERFORM IMS-GN-SUPREF-WDE6F1                                     
086400       ELSE                                                               
086500         PERFORM IMS-GN-KOLLI-WDE6F1                                      
086600       END-IF                                                             
086700     END-IF                                                               
086800                                                                          
086900     IF SEGMENT-FINNS                                                     
087000       MOVE JA                 TO KOLLI-FINNS-SW                          
087100       MOVE SEQF-IDPRODNR      TO W-IDPRODNR                              
087200       MOVE SEQF-IDKOLLI       TO W-IDKOLLI                               
087300       PERFORM IMS-GU-WDE611                                              
087400       MOVE '60175-2'          TO IDPGM                                   
087500       MOVE SEQF-IDPRODNR      TO W-IDPRODNR-ESEQ                         
087600       PERFORM IMS-GU-WDE401-ESEQ                                         
087700       IF SEGMENT-SAKNAS                                                  
087800         MOVE ZERO             TO KORD-IDORDNR5                           
087900       END-IF                                                             
088000* SPARA NYCKLAR TILL EV. KOMMANDE UPPDATERING AV WDE601/WDE610            
088100       IF INDX <= MAX-INDX                                                
088200         MOVE SEQF-IDPRODNR    TO SPAR-UPPD-IDPRODNR (INDX)               
088300         MOVE KOLLI-IDKOLLI    TO SPAR-UPPD-IDKOLLI  (INDX)               
088400       END-IF                                                             
088500     END-IF                                                               
088600                                                                          
088700     .                                                                    
088800     EJECT                                                                
088900 G-KOLLA-INPUT SECTION.                                                   
089000                                                                          
089100     MOVE JA                       TO INDATA-SW                           
089200     MOVE NEJ                      TO MARKERING-SW                        
089300                                                                          
089400*    -- KONTROLL AV SELECT-RAD                                            
089500     MOVE +1 TO INDX                                                      
089600     PERFORM UNTIL INDX > MAX-INDX                                        
089700       IF MID-CMD (INDX) NOT = '+' AND ' '                                
089800         IF MID-CMD (INDX) NOT = 'Y' AND 'J' AND 'N'                      
089900           MOVE NEJ                TO INDATA-SW                           
090000           MOVE MFS-ALFA-FAELT-FEL TO MOD-CMD-ATTR (INDX)                 
090100           MOVE 15                 TO INDX                                
090200           MOVE JA                 TO MARKERING-SW                        
090300         ELSE                                                             
090400           MOVE MFS-ALFA-FAELT-RAETT                                      
090500                                   TO MOD-CMD-ATTR (INDX)                 
090600           MOVE JA                 TO MARKERING-SW                        
090700           PERFORM GA-CHECK-DATA                                          
090800         END-IF                                                           
090900       END-IF                                                             
091000       ADD +1 TO INDX                                                     
091100     END-PERFORM                                                          
091200                                                                          
091300     IF INGEN-MARKERING-GJORD                                             
091400       MOVE ERR-PF11-AND-NO-DATA   TO MED-IDMFSFEL                        
091500       CALL WMEDKONV USING MED-WMEDAREA                                   
091600       MOVE MED-MFSFEL             TO MOD-TEMFSFEL                        
091700       PERFORM MFS-ROER-EJ-FAELT-IN                                       
091800       PERFORM MFS-ROER-EJ-FAELT-UT                                       
091900       MOVE NEJ                    TO INDATA-SW                           
092000     ELSE                                                                 
092100       IF INDATA-FEL                                                      
092200         MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                        
092300         CALL WMEDKONV USING MED-WMEDAREA                                 
092400         MOVE MED-MFSFEL           TO MOD-TEMFSFEL                        
092500         PERFORM MFS-ROER-EJ-FAELT-UT                                     
092600         PERFORM MFS-ROER-EJ-FAELT-IN                                     
092700       END-IF                                                             
092800     END-IF                                                               
092900     .                                                                    
093000     EJECT                                                                
093100                                                                          
093200 GA-CHECK-DATA SECTION.                                                   
093300                                                                          
093400     IF SPAR-UPPD-IDPRODNR (INDX) > ZERO                                  
093500       MOVE SPAR-UPPD-IDPRODNR (INDX)  TO W-IDPRODNR                      
093600       MOVE SPAR-UPPD-IDKOLLI  (INDX)  TO W-IDKOLLI                       
093700       PERFORM IMS-GU-VORD                                                
093800       PERFORM IMS-GU-WDE611                                              
093900       IF MID-CMD (INDX) = 'J' OR 'Y'                                     
094000         IF KOLLI-KDKOLSTA = 0                                            
094100           MOVE VORD-IDPRODNR      TO W-IDPRODNR-ESEQ                     
094200           PERFORM IMS-GU-WDE401-ESEQ                                     
094300           PERFORM HA-DEFINIERA-PLATS                                     
094400           IF PLATS-KDSVAR NOT = SPACE                                    
094500             MOVE NEJ                    TO INDATA-SW                     
094600             MOVE MFS-ALFA-FAELT-FEL     TO MOD-CMD-ATTR (INDX)           
094700             MOVE ERR-ADDRESS-HANDLING   TO MED-IDMFSINF                  
094800             CALL WMEDKONV USING MED-WMEDAREA                             
094900             MOVE MED-MFSINF             TO MOD-TEMFSINF                  
095000           END-IF                                                         
095100         ELSE                                                             
095200           MOVE NEJ                      TO INDATA-SW                     
095300           MOVE MFS-ALFA-FAELT-FEL       TO MOD-CMD-ATTR (INDX)           
095400           MOVE INF-UPDATE-NOT-ALLOWED   TO MED-IDMFSINF                  
095500           CALL WMEDKONV              USING MED-WMEDAREA                  
095600           MOVE MED-MFSINF               TO MOD-TEMFSINF                  
095700         END-IF                                                           
095800       ELSE                                                               
095900         IF MID-CMD (INDX) = 'N'                                          
096000           IF KOLLI-KDKOLSTA = 1                                          
096100             CONTINUE                                                     
096200           ELSE                                                           
096300             MOVE NEJ                    TO INDATA-SW                     
096400             MOVE MFS-ALFA-FAELT-FEL     TO MOD-CMD-ATTR (INDX)           
096500             MOVE INF-UPDATE-NOT-ALLOWED TO MED-IDMFSINF                  
096600             CALL WMEDKONV            USING MED-WMEDAREA                  
096700             MOVE MED-MFSINF             TO MOD-TEMFSINF                  
096800           END-IF                                                         
096900         END-IF                                                           
097000       END-IF                                                             
097100     END-IF                                                               
097200                                                                          
097300     .                                                                    
097400     EJECT                                                                
097500 H-UPPDATERA SECTION.                                                     
097600                                                                          
097700     MOVE NEJ                          TO UPPD-SW                         
097800                                                                          
097900* HÄR SÄTTS NYA VÄRDEN FÖR DIV. FÄLT I WDE610                             
098000     MOVE +1 TO INDX                                                      
098100     PERFORM UNTIL INDX > MAX-INDX OR                                     
098200                   SPAR-UPPD-IDPRODNR (INDX) = ZERO                       
098300       MOVE SPAR-UPPD-IDPRODNR (INDX)  TO W-IDPRODNR                      
098400                                          LOGG-IDPRODNR                   
098500       MOVE SPAR-UPPD-IDKOLLI  (INDX)  TO W-IDKOLLI                       
098600                                          LOGG-IDKOLLI                    
098700       PERFORM IMS-GHU-VORD                                               
098800       PERFORM IMS-GHU-KOLLI                                              
098900       IF MID-CMD (INDX) = 'J' OR 'Y'                                     
099000         IF KOLLI-KDKOLSTA = 0                                            
099100           MOVE '60175-3'          TO IDPGM                               
099200           MOVE VORD-IDPRODNR      TO W-IDPRODNR-ESEQ                     
099300           PERFORM IMS-GU-WDE401-ESEQ                                     
099400           PERFORM HA-DEFINIERA-PLATS                                     
099500           PERFORM IMS-GHU-KOLLI                                          
099600           MOVE PLATS-IDTRPTNR           TO KOLLI-IDTRPTNR                
099700           MOVE PLATS-ADFLGEO            TO KOLLI-ADFLGEO                 
099800           MOVE PLATS-ADFLOMR            TO KOLLI-ADFLOMR                 
099900           MOVE PLATS-ADRUTNIV           TO KOLLI-ADRUTNIV                
100000           MOVE PLATS-ADVMODUL           TO KOLLI-ADVMODUL                
100100           MOVE PLATS-DIDMODUL           TO KOLLI-DIDMODUL                
100200           MOVE PLATS-DIHMODUL           TO KOLLI-DIHMODUL                
100300           MOVE PLATS-ADHMODUL           TO KOLLI-ADHMODUL                
100400           MOVE PLATS-FLUTLAST           TO KOLLI-FLUTLAST                
100500*LK        MOVE PLATS-IDDC-CROSS         TO KOLLI-IDDC-CROSS              
100600           MOVE VORD-IDDISTR             TO TEST-IDDISTR                  
100700           IF VORD-FLAUTFAK = JA AND DIST03-SVERIGE-2                     
100800             MOVE NEJ                    TO KOLLI-FLUTLAST                
100900           END-IF                                                         
101000           MOVE 1                        TO KOLLI-KDKOLSTA                
101100           MOVE WS-DAGENS-DATUM-AAMMDD   TO KOLLI-TIPACKN                 
101200           COMPUTE KOLLI-TIPACTID = WS-DAGENS-KLOCKA / 100                
101300           END-COMPUTE                                                    
101400           IF KOLLI-TIAAVVD-PATR = ZERO                                   
101500              PERFORM S01-SKAPA-TRANS-TILL-SV-AF                          
101600           END-IF                                                         
101700                                                                          
101800           PERFORM IMS-REPL-KOLLI                                         
101900           PERFORM HD-SKAPA-LOGG                                          
101910                                                                          
101920           IF PLATS-IDDC-CROSS > SPACES                                   
101930            MOVE W-KDSEGKEY-X     TO CROSS-KDSEGKEY                       
101940            MOVE PLATS-IDDC       TO CROSS-IDDC-SEND                      
101950            MOVE PLATS-IDDC-CROSS TO CROSS-IDDC-CROSS                     
101960            MOVE KOLLI-IDDISTR    TO CROSS-IDDISTR                        
101970            MOVE KOLLI-IDKUNDNR   TO CROSS-IDKUNDNR                       
101980            MOVE VORD-IDPRODNR    TO CROSS-IDPRODNR                       
101990            MOVE KOLLI-IDKOLLI    TO CROSS-IDKOLLI                        
101991            MOVE KOLLI-IDLEVNR    TO CROSS-IDLEVNR                        
101992            MOVE KOLLI-IDSUPREF   TO CROSS-IDSUPREF                       
101993            MOVE KOLLI-DARFS(3:6) TO CROSS-TIRFSDAT                       
101994            MOVE ZERO             TO CROSS-IDTRPTNR-CROSS                 
101995            MOVE ZERO             TO CROSS-TIRECXDAT                      
101996            MOVE ZERO             TO CROSS-TIRECXTID                      
101997            MOVE ZERO             TO CROSS-TISKEPPN                       
101998            MOVE ZERO             TO CROSS-IDSHIPM-CROSS                  
101999            MOVE SPACE            TO CROSS-IDLBBET-CROSS                  
102000            MOVE 1                TO CROSS-KDKOLSTA-CROSS                 
102001            PERFORM IMS-ISRT-WDE621                                       
102002           END-IF                                                         
102003                                                                          
102010           MOVE JA                       TO UPPD-SW                       
102100         END-IF                                                           
102200       ELSE                                                               
102300         IF MID-CMD (INDX) = 'N'                                          
102400           IF KOLLI-KDKOLSTA = 1                                          
102500             MOVE NEJ                  TO KOLLI-FLAUTFAK                  
102600             MOVE 0                    TO KOLLI-KDKOLSTA                  
102700                                          KOLLI-TIPACKN                   
102800                                          KOLLI-TIPACTID                  
102900             MOVE JA                   TO UPPD-SW                         
103000             PERFORM IMS-REPL-KOLLI                                       
103100             PERFORM HE-SKAPA-LOGG-BACKNING                               
103200           END-IF                                                         
103300         END-IF                                                           
103400       END-IF                                                             
103500       ADD +1 TO INDX                                                     
103600     END-PERFORM                                                          
103700                                                                          
103800     IF UPPDATERING-GJORD                                                 
103900       MOVE +1 TO INDX                                                    
104000       PERFORM UNTIL INDX > MAX-INDX OR                                   
104100                     SPAR-UPPD-IDPRODNR (INDX) = ZERO                     
104200         IF MID-CMD (INDX) = 'Y' OR 'J' OR 'N'                            
104300           MOVE SPAR-UPPD-IDPRODNR (INDX) TO W-IDPRODNR                   
104400           MOVE NEJ                       TO P-SW                         
104500                                           U-SW                           
104600           PERFORM IMS-GU-VORD                                            
104700           PERFORM UNTIL SEGMENT-SAKNAS                                   
104800             PERFORM IMS-GNP-KOLLI                                        
104900             IF KOLLI-KDKOLSTA = 0                                        
105000               MOVE JA                    TO U-SW                         
105100             ELSE                                                         
105200               IF KOLLI-KDKOLSTA = 1                                      
105300                 MOVE JA                  TO P-SW                         
105400               END-IF                                                     
105500             END-IF                                                       
105600           END-PERFORM                                                    
105700                                                                          
105800* HÄR SÄTTS NYA VÄRDEN FÖR DIV. FÄLT I WDE601 OCH                         
105900* HÄR AVGÖRS OM BEFINTLIGA WDGX4726/27-SEGMENT SKALL TAS BORT             
106000* ELLER OM NYA SÅDANA SKALL LÄGGAS UPP                                    
106100           PERFORM IMS-GHU-VORD                                           
106200           MOVE VORD-IDDISTR              TO TEST-IDDISTR                 
106300           IF UTSKRIVEN AND                                               
106400              PACKAD                                                      
106500             IF VORD-KDORDSTA NOT = 2                                     
106600               IF VORD-KDORDSTA = 3                                       
106700                 IF VORD-FLAUTFAK = JA AND                                
106800                    DIST03-SVERIGE-2                                      
106900                   PERFORM HB-BORTTAG-WDGX4726-27                         
107000                 END-IF                                                   
107100               END-IF                                                     
107200* UTSKRIVEN, PACKNING PÅBÖRJAD                                            
107300               MOVE 2                     TO VORD-KDORDSTA                
107400             END-IF                                                       
107500           ELSE                                                           
107600             IF UTSKRIVEN                                                 
107700               IF VORD-KDORDSTA NOT = 1                                   
107800                 IF VORD-KDORDSTA = 3                                     
107900                   IF VORD-FLAUTFAK = JA AND                              
108000                      DIST03-SVERIGE-2                                    
108100                     PERFORM HB-BORTTAG-WDGX4726-27                       
108200                   END-IF                                                 
108300                 END-IF                                                   
108400* UTSKRIVEN, PACKNING EJ PÅBÖRJAD                                         
108500                 MOVE 1                   TO VORD-KDORDSTA                
108600               END-IF                                                     
108700             ELSE                                                         
108800               IF PACKAD AND                                              
108900                  VORD-KVORDRAD = VORD-KVORDRAD-PACK                      
109000                 IF VORD-KDORDSTA NOT = 3                                 
109100                   IF VORD-FLAUTFAK = JA AND                              
109200                      DIST03-SVERIGE-2                                    
109300                     PERFORM HC-NYUPPL-WDGX4726-27                        
109400                   END-IF                                                 
109500* PACKNING KLAR                                                           
109600                   MOVE 3                 TO VORD-KDORDSTA                
109700                 END-IF                                                   
109800               END-IF                                                     
109900             END-IF                                                       
110000           END-IF                                                         
110100           IF MID-CMD(INDX) = 'Y' OR 'J'                                  
110200             ADD +1                       TO VORD-KVKOLLI                 
110300           ELSE                                                           
110400             IF MID-CMD(INDX) = 'N'                                       
110500               SUBTRACT +1                FROM VORD-KVKOLLI               
110600             END-IF                                                       
110700           END-IF                                                         
110800           PERFORM IMS-REPL-VORD                                          
110900         END-IF                                                           
111000         ADD +1 TO INDX                                                   
111100       END-PERFORM                                                        
111200     END-IF                                                               
111300                                                                          
111400     IF UPPDATERING-GJORD                                                 
111500       MOVE INF-UPDATE-DONE             TO MED-IDMFSINF                   
111600       CALL WMEDKONV USING MED-WMEDAREA                                   
111700       MOVE MED-MFSINF                  TO MOD-TEMFSINF                   
111800       PERFORM MFS-FORM-ATTR                                              
111900       PERFORM MFS-RENSA-FAELT-IN                                         
112000     ELSE                                                                 
112100       MOVE INF-UPDATE-NOT-DONE         TO MED-IDMFSINF                   
112200       CALL WMEDKONV USING MED-WMEDAREA                                   
112300       MOVE MED-MFSINF                  TO MOD-TEMFSINF                   
112400       PERFORM MFS-FORM-ATTR                                              
112500       PERFORM MFS-RENSA-FAELT-IN                                         
112600     END-IF                                                               
112700                                                                          
112800     MOVE SPAR-IDLEVNR-ENTER    TO W-IDLEVNR-MIN                          
112900     MOVE SPAR-DASUPREF-ENTER   TO W-DASUPREF-MIN                         
113000     MOVE SPAR-IDSUPREF-ENTER   TO W-IDSUPREF-MIN                         
113100     MOVE SPAR-IDPRODNR-ENTER   TO W-IDPRODNR-MIN                         
113200     MOVE SPAR-IDKOLLI-ENTER    TO W-IDKOLLI-MIN                          
113300     .                                                                    
113400     EJECT                                                                
113500 HA-DEFINIERA-PLATS SECTION.                                              
113600                                                                          
113700     IF KOLLI-KDORDKL = 4                                                 
113800        MOVE +1               TO PLATS-KDCALL                             
113900     ELSE                                                                 
114000        MOVE +2               TO PLATS-KDCALL                             
114100     END-IF                                                               
114200     MOVE WS-CDC-11           TO PLATS-IDDC                               
114300     MOVE KOLLI-IDDISTR       TO PLATS-IDDISTR                            
114400     MOVE KOLLI-IDKUNDNR      TO PLATS-IDKUNDNR                           
114500     MOVE VORD-KDFRAKT        TO PLATS-KDFRAKT                            
114600     MOVE KORD-IDORDNR5       TO PLATS-IDORDNR                            
114700     MOVE KOLLI-KDORDKL       TO PLATS-KDORDKLX                           
114800     MOVE ZERO                TO PLATS-DIKOLLIH                           
114900                                 PLATS-DIKOLLIL                           
115000                                 PLATS-DIKOLLIB                           
115100                                 PLATS-VKORDNTO-KOLLI                     
115200     MOVE SPACE               TO PLATS-KDKOLLID                           
115300     MOVE KOLLI-ADFLGEO       TO PLATS-ADFLGEO                            
115400     MOVE KOLLI-ADFLOMR       TO PLATS-ADFLOMR                            
115500     MOVE KOLLI-ADRUTNIV      TO PLATS-ADRUTNIV                           
115600     MOVE ZERO                TO PLATS-IDTRPTNR                           
115700                                 PLATS-DIHMODUL                           
115800                                 PLATS-DIDMODUL                           
115900                                 PLATS-ADVMODUL                           
116000                                 PLATS-ADHMODUL                           
116100     MOVE SPACE               TO PLATS-FLUTLAST                           
116200                                 PLATS-IDDC-CROSS                         
116300                                                                          
116400     CALL W403PLAT USING PLATS-W403PLAT                                   
116500                         PLATS-DM-PCB                                     
116600                         PLATS-DN-PCB                                     
116700                         PLATS-DP-PCB                                     
116800                         PLATS-DO-PCB                                     
116900                         PLATS-WDE6C-PCB                                  
117000                         PLATS-GMTC-PCB                                   
117100                         PLATS-WDB6-PCB                                   
117200     .                                                                    
117300     EJECT                                                                
117400 HB-BORTTAG-WDGX4726-27 SECTION.                                          
117500                                                                          
117600     MOVE '4726'                  TO W-4726-IDHTYP                        
117700*    IF  DIST03-SVERIGE                                                   
117800*        MOVE JA                  TO W-4726-FLBATCH                       
117900*    ELSE                                                                 
118000         MOVE NEJ                 TO W-4726-FLBATCH                       
118100*    END-IF                                                               
118200     MOVE LOW-VALUE               TO W-4726-LOWVALUE                      
118300     PERFORM IMS-GHU-4726-ROT                                             
118400                                                                          
118500     MOVE VORD-IDDISTR            TO W-4726-IDDISTR                       
118600     MOVE VORD-IDKUNDNR           TO W-4726-IDKUNDNR                      
118700     MOVE VORD-IDDC               TO W-4726-IDDC                          
118800     MOVE VORD-KDFAKTYP           TO W-4726-KDFAKTYP                      
118900     PERFORM IMS-GHNP-4726-UNDERSEG                                       
119000                                                                          
119100     IF SEGMENT-FINNS                                                     
119200       MOVE VORD-IDDISTR          TO AUTFAKT-IDDISTR                      
119300       MOVE VORD-IDKUNDNR         TO AUTFAKT-IDKUNDNR                     
119400       MOVE VORD-IDDC             TO AUTFAKT-IDDC                         
119500       MOVE VORD-KDFAKTYP         TO AUTFAKT-KDFAKTYP                     
119600       PERFORM IMS-DLET-4726-UNDERSEG                                     
119700     END-IF                                                               
119800     .                                                                    
119900     EJECT                                                                
120000 HC-NYUPPL-WDGX4726-27 SECTION.                                           
120100                                                                          
120200     MOVE '4726'                  TO W-4726-IDHTYP                        
120300*    IF  DIST03-SVERIGE                                                   
120400*        MOVE JA                  TO W-4726-FLBATCH                       
120500*    ELSE                                                                 
120600         MOVE NEJ                 TO W-4726-FLBATCH                       
120700*    END-IF                                                               
120800     MOVE LOW-VALUE               TO W-4726-LOWVALUE                      
120900     PERFORM IMS-GHU-4726-ROT                                             
121000                                                                          
121100     MOVE VORD-IDDISTR            TO W-4726-IDDISTR                       
121200     MOVE VORD-IDKUNDNR           TO W-4726-IDKUNDNR                      
121300     MOVE VORD-IDDC               TO W-4726-IDDC                          
121400     MOVE VORD-KDFAKTYP           TO W-4726-KDFAKTYP                      
121500     PERFORM IMS-GHNP-4726-UNDERSEG                                       
121600                                                                          
121700     IF SEGMENT-SAKNAS                                                    
121800       MOVE VORD-IDDISTR          TO AUTFAKT-IDDISTR                      
121900       MOVE VORD-IDKUNDNR         TO AUTFAKT-IDKUNDNR                     
122000       MOVE VORD-IDDC             TO AUTFAKT-IDDC                         
122100       MOVE VORD-KDFAKTYP         TO AUTFAKT-KDFAKTYP                     
122200       PERFORM IMS-INSERT-4726-UNDERSEG                                   
122300     END-IF                                                               
122400                                                                          
122500     MOVE VORD-IDPRODNR           TO AUTFAKT-IDPRODNR                     
122600     MOVE ZERO                    TO AUTFAKT-IDSKEPPN                     
122700                                     AUTFAKT-PRFRAKT                      
122800     IF  DIST03-SVERIGE                                                   
122900         MOVE NEJ                 TO AUTFAKT-FLLASTA                      
123000     ELSE                                                                 
123100         MOVE JA                  TO AUTFAKT-FLLASTA                      
123200     END-IF                                                               
123300                                                                          
123400     PERFORM IMS-INSERT-4727                                              
123500     .                                                                    
123600     EJECT                                                                
123700 HD-SKAPA-LOGG SECTION.                                                   
123800                                                                          
123900     MOVE 'W463'                      TO FIL-CT-IDSYSTEM                  
124000     MOVE 'VIA'                       TO FIL-CT-IDPTYP                    
124100     MOVE ' '                         TO FIL-CT-IDVTYP                    
124200                                                                          
124300     MOVE 'W6017500'                  TO FIL-IDPGM                        
124400     MOVE FUNCTION CURRENT-DATE (3:6) TO FIL-TIREGDAT                     
124500     MOVE FUNCTION CURRENT-DATE (1:8) TO LOGG-DAREGDAT                    
124600     ACCEPT FIL-TIKLOCK               FROM TIME                           
124700     MOVE FIL-TIKLOCK                 TO LOGG-TIREGTID                    
124800     MOVE 1                           TO FIL-IDSEKVNR                     
124900     MOVE 'VIA'                       TO LOGG-IDPTYP                      
125000     MOVE VORD-IDDC                   TO LOGG-IDDC                        
125100     MOVE VORD-IDDISTR                TO LOGG-IDDISTR                     
125200     MOVE VORD-IDKUNDNR               TO LOGG-IDKUNDNR                    
125300     MOVE ZERO                        TO LOGG-IDRADNR                     
125400                                         LOGG-IDARTNR                     
125500                                         LOGG-IDORDNR7                    
125600                                         LOGG-KVANTAL                     
125700     IF KOLLI-TIPACKN > 501231                                            
125800       COMPUTE LOGG-DAPACKN = 19000000 + KOLLI-TIPACKN                    
125900     ELSE                                                                 
126000       COMPUTE LOGG-DAPACKN = 20000000 + KOLLI-TIPACKN                    
126100     END-IF                                                               
126200     MOVE KOLLI-IDSUPREF              TO LOGG-IDSUPREF                    
126300     MOVE KOLLI-DASUPREF              TO LOGG-DASUPREF                    
126400     MOVE KOLLI-TIPACTID              TO LOGG-TIPACTID                    
126500     COMPUTE LOGG-TISUPTID = KOLLI-TISUPTID / 10                          
126600     MOVE KOLLI-KDORDKL               TO LOGG-KDORDKL                     
126700     MOVE KOLLI-KDVIA                 TO LOGG-KDVIA                       
126800     MOVE ZERO                        TO LOGG-DABEKDAT                    
126900                                         LOGG-DAFAKT                      
127000                                         LOGG-DALEVDAT                    
127100                                         LOGG-DASKEPPN                    
127200                                         LOGG-DASNDDAT                    
127300                                         LOGG-TIBEKR                      
127400                                         LOGG-TISNDTID                    
127500                                         LOGG-KDORDBEK                    
127600     MOVE SPACE                       TO LOGG-BERADREF                    
127700                                                                          
127800     PERFORM IMS-ISRT-LOGG                                                
127900     PERFORM UNTIL SEGMENT-FINNS                                          
128000       ADD +1  TO FIL-IDSEKVNR                                            
128100       PERFORM IMS-ISRT-LOGG                                              
128200     END-PERFORM                                                          
128300     .                                                                    
128400     EJECT                                                                
128500 HE-SKAPA-LOGG-BACKNING SECTION.                                          
128600                                                                          
128700     MOVE 'W6017500'                  TO FIL-IDPGM                        
128800     MOVE FUNCTION CURRENT-DATE (3:6) TO FIL-TIREGDAT                     
128900     MOVE FUNCTION CURRENT-DATE (1:8) TO LOGG-DAREGDAT                    
129000     ACCEPT FIL-TIKLOCK               FROM TIME                           
129100     MOVE FIL-TIKLOCK                 TO LOGG-TIREGTID                    
129200     MOVE 1                           TO FIL-IDSEKVNR                     
129300     MOVE 'VIA'                       TO LOGG-IDPTYP                      
129400     MOVE VORD-IDDC                   TO LOGG-IDDC                        
129500     MOVE VORD-IDDISTR                TO LOGG-IDDISTR                     
129600     MOVE VORD-IDKUNDNR               TO LOGG-IDKUNDNR                    
129700     MOVE ZERO                        TO LOGG-IDRADNR                     
129800                                         LOGG-IDARTNR                     
129900                                         LOGG-IDORDNR7                    
130000                                         LOGG-DAPACKN                     
130100                                         LOGG-TIPACTID                    
130200                                         LOGG-KVANTAL                     
130300     COMPUTE LOGG-TISUPTID = KOLLI-TISUPTID / 10                          
130400     MOVE KOLLI-KDORDKL               TO LOGG-KDORDKL                     
130500     MOVE KOLLI-KDVIA                 TO LOGG-KDVIA                       
130600     MOVE ZERO                        TO LOGG-DABEKDAT                    
130700                                         LOGG-DAFAKT                      
130800                                         LOGG-DALEVDAT                    
130900                                         LOGG-DASKEPPN                    
131000                                         LOGG-TIBEKR                      
131100                                         LOGG-KDORDBEK                    
131200     MOVE SPACE                       TO LOGG-BERADREF                    
131300                                                                          
131400     PERFORM IMS-ISRT-LOGG                                                
131500     PERFORM UNTIL SEGMENT-FINNS                                          
131600       ADD +1  TO FIL-IDSEKVNR                                            
131700       PERFORM IMS-ISRT-LOGG                                              
131800     END-PERFORM                                                          
131900     .                                                                    
132000     EJECT                                                                
132100 S01-SKAPA-TRANS-TILL-SV-AF SECTION.                                      
132200                                                                          
132300**   SKAPA INFO TILL SVENSKA ÅF, SÄNDS VIA VR.                            
132400     IF DIST03-SVERIGE-100-799                                            
132500        OR DIST03-NORGE                                                   
132600        OR DIST03-DANMARK-900                                             
132700        OR DIST85-PU-VIA-VR                                               
132800        OR DIST21-TYRE                                                    
132900       MOVE W-IDPRODNR         TO XXJK-4322-IDPRODNR                      
133000       MOVE KOLLI-IDKOLLI      TO XXJK-4322-IDKOLLI                       
133100       MOVE XXJK-4322-WDGX4322 TO 4322-WDGX4322                           
133200       PERFORM IMS-ISRT-4322-SEGM                                         
133300                                                                          
133400* EN LITEN UPPDATERING AV KOLLIT FÖR ATT UNDVIKA DUBBLA TRANSAR           
133500       MOVE WS-DAGDAT-AAVVD    TO KOLLI-TIAAVVD-PATR                      
133600     END-IF                                                               
133700     .                                                                    
133800     EJECT                                                                
133900 MFS-RENSA-FAELT-UT SECTION.                                              
134000                                                                          
134100     MOVE +1 TO INDX                                                      
134200     PERFORM UNTIL INDX > MAX-INDX                                        
134300       MOVE MFS-RENSA-FAELT TO MOD-CMD            (INDX)                  
134400                               MOD-TISUPREF       (INDX)                  
134500                               MOD-TISUPTID       (INDX)                  
134600                               MOD-IDSUPREF       (INDX)                  
134700                               MOD-TIPACKN        (INDX)                  
134800                               MOD-TIPACTID       (INDX)                  
134900                               MOD-IDDISTR        (INDX)                  
135000                               MOD-IDKUNDNR       (INDX)                  
135100                               MOD-IDORDNR5       (INDX)                  
135200                               MOD-IDKOLLI        (INDX)                  
135300                               MOD-VKORDBTO-KOLLI (INDX)                  
135400                               MOD-VLORDBTO-KOLLI (INDX)                  
135500       ADD +1 TO INDX                                                     
135600     END-PERFORM                                                          
135700     .                                                                    
135800 MFS-RENSA-FAELT-IN SECTION.                                              
135900                                                                          
136000     MOVE MFS-RENSA-FAELT TO MOD-IDLEVNR-IN                               
136100                             MOD-TISUPREF-IN                              
136200                             MOD-IDSUPREF-IN                              
136300                             MOD-IDDISTR-IN                               
136400     .                                                                    
136500     EJECT                                                                
136600 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
136700                                                                          
136800     MOVE +1 TO INDX                                                      
136900     PERFORM UNTIL INDX > MAX-INDX                                        
137000       MOVE MFS-ROER-EJ-FAELT TO MOD-CMD            (INDX)                
137100                                 MOD-TISUPREF       (INDX)                
137200                                 MOD-TISUPTID       (INDX)                
137300                                 MOD-IDSUPREF       (INDX)                
137400                                 MOD-TIPACKN        (INDX)                
137500                                 MOD-TIPACTID       (INDX)                
137600                                 MOD-IDDISTR        (INDX)                
137700                                 MOD-IDKUNDNR       (INDX)                
137800                                 MOD-IDORDNR5       (INDX)                
137900                                 MOD-IDKOLLI        (INDX)                
138000                                 MOD-VKORDBTO-KOLLI (INDX)                
138100                                 MOD-VLORDBTO-KOLLI (INDX)                
138200       ADD +1 TO INDX                                                     
138300     END-PERFORM                                                          
138400     .                                                                    
138500 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
138600                                                                          
138700     MOVE MFS-ROER-EJ-FAELT TO MOD-IDLEVNR-IN                             
138800                               MOD-TISUPREF-IN                            
138900                               MOD-IDSUPREF-IN                            
139000                               MOD-IDDISTR-IN                             
139100     .                                                                    
139200     EJECT                                                                
139300 MFS-FORM-ATTR SECTION.                                                   
139400                                                                          
139500*    --- ALLA INDATA-FÄLT                                                 
139600     MOVE +1 TO INDX                                                      
139700     PERFORM UNTIL INDX > MAX-INDX                                        
139800       MOVE MFS-FORMATETS-ATTR TO MOD-CMD-ATTR (INDX)                     
139900       ADD +1 TO INDX                                                     
140000     END-PERFORM                                                          
140100     .                                                                    
140200     EJECT                                                                
140300* --- IMS SEKTIONER ---                                                   
140400                                                                          
140500 IMS-GET-MSG SECTION.                                                     
140600     MOVE '  QC'          TO GODK-STATUSKODER                             
140700     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
140800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
140900     PERFORM IMS-STATUSKONTROLL                                           
141000     .                                                                    
141100                                                                          
141200 IMS-INSERT-MSG SECTION.                                                  
141300     IF ENGLISH-TEXT                                                      
141400       MOVE 'N' TO MFS-KDHUVOMR                                           
141500     END-IF                                                               
141600     MOVE LOW-VALUE       TO MSG-KDZ1 MSG-KDZ2                            
141700     MOVE SPACE TO GODK-STATUSKODER                                       
141800     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
141900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
142000     PERFORM IMS-STATUSKONTROLL                                           
142100     .                                                                    
142200     EJECT                                                                
142300 IMS-GN-DISTR-WDE6F1 SECTION.                                             
142400     MOVE 'IMS-GN-DISTR-WDE6F1 '   TO CURRENT-IMS-SECTION                 
142500                                                                          
142600     STRING 'WDE6F1  (WDE6F1KY>=' W-WDE6F1KY-MIN-X                        
142700                    '&WDE6F1KY<=' W-WDE6F1KY-MAX-X                        
142800                    '&IDDISTR  =' W-IDDISTR-F1-X ')'                      
142900            DELIMITED BY SIZE INTO SSA1                                   
143000     MOVE '  GEGB'            TO GODK-STATUSKODER                         
143100     CALL CBLTDLI USING GN  WDE6F-PCB DLI-IO-WDE6F1 SSA1                  
143200     MOVE WDE6F-STATUS-CODE TO STATUS-WS                                  
143300     PERFORM IMS-STATUSKONTROLL                                           
143400     .                                                                    
143500     EJECT                                                                
143600 IMS-GU-DISTR-WDE6F1 SECTION.                                             
143700     MOVE 'IMS-GU-DISTR-WDE6F1 '   TO CURRENT-IMS-SECTION                 
143800                                                                          
143900     STRING 'WDE6F1  (WDE6F1KY>=' W-WDE6F1KY-MIN-X                        
144000                    '&WDE6F1KY<=' W-WDE6F1KY-MAX-X                        
144100                    '&IDDISTR  =' W-IDDISTR-F1-X ')'                      
144200            DELIMITED BY SIZE INTO SSA1                                   
144300     MOVE '  GE'            TO GODK-STATUSKODER                           
144400     CALL CBLTDLI USING GU  WDE6F-PCB DLI-IO-WDE6F1 SSA1                  
144500     MOVE WDE6F-STATUS-CODE TO STATUS-WS                                  
144600     PERFORM IMS-STATUSKONTROLL                                           
144700     .                                                                    
144800     EJECT                                                                
144900 IMS-GN-DISTR-2-WDE6F1 SECTION.                                           
145000     MOVE 'IMS-GN-DISTR-2-WDE6F1 '  TO CURRENT-IMS-SECTION                
145100                                                                          
145200     STRING 'WDE6F1  (WDE6F1KY>=' W-WDE6F1KY-MIN-X                        
145300                    '&WDE6F1KY<=' W-WDE6F1KY-MAX-X                        
145400                    '&IDSUPREF =' W-IDSUPREF-F1-X                         
145500                    '&IDDISTR  =' W-IDDISTR-F1-X ')'                      
145600            DELIMITED BY SIZE INTO SSA1                                   
145700     MOVE '  GEGB'            TO GODK-STATUSKODER                         
145800     CALL CBLTDLI USING GN  WDE6F-PCB DLI-IO-WDE6F1 SSA1                  
145900     MOVE WDE6F-STATUS-CODE TO STATUS-WS                                  
146000     PERFORM IMS-STATUSKONTROLL                                           
146100     .                                                                    
146200     EJECT                                                                
146300 IMS-GU-DISTR-2-WDE6F1 SECTION.                                           
146400     MOVE 'IMS-GU-DISTR-2-WDE6F1 '  TO CURRENT-IMS-SECTION                
146500                                                                          
146600     STRING 'WDE6F1  (WDE6F1KY>=' W-WDE6F1KY-MIN-X                        
146700                    '&WDE6F1KY<=' W-WDE6F1KY-MAX-X                        
146800                    '&IDSUPREF =' W-IDSUPREF-F1-X                         
146900                    '&IDDISTR  =' W-IDDISTR-F1-X ')'                      
147000            DELIMITED BY SIZE INTO SSA1                                   
147100     MOVE '  GE'            TO GODK-STATUSKODER                           
147200     CALL CBLTDLI USING GU  WDE6F-PCB DLI-IO-WDE6F1 SSA1                  
147300     MOVE WDE6F-STATUS-CODE TO STATUS-WS                                  
147400     PERFORM IMS-STATUSKONTROLL                                           
147500     .                                                                    
147600     EJECT                                                                
147700 IMS-GN-KOLLI-WDE6F1 SECTION.                                             
147800     MOVE 'IMS-GN-KOLLI-WDE6F1 '  TO CURRENT-IMS-SECTION                  
147900                                                                          
148000     STRING 'WDE6F1  (WDE6F1KY>=' W-WDE6F1KY-MIN-X                        
148100                    '&WDE6F1KY<=' W-WDE6F1KY-MAX-X ')'                    
148200            DELIMITED BY SIZE INTO SSA1                                   
148300     MOVE '  GEGB'          TO GODK-STATUSKODER                           
148400     CALL CBLTDLI USING GN  WDE6F-PCB DLI-IO-WDE6F1 SSA1                  
148500     MOVE WDE6F-STATUS-CODE TO STATUS-WS                                  
148600     PERFORM IMS-STATUSKONTROLL                                           
148700     .                                                                    
148800     EJECT                                                                
148900 IMS-GU-KOLLI-WDE6F1 SECTION.                                             
149000     MOVE 'IMS-GU-KOLLI-WDE6F1 '  TO CURRENT-IMS-SECTION                  
149100                                                                          
149200     STRING 'WDE6F1  (WDE6F1KY>=' W-WDE6F1KY-MIN-X                        
149300                    '&WDE6F1KY<=' W-WDE6F1KY-MAX-X ')'                    
149400            DELIMITED BY SIZE INTO SSA1                                   
149500     MOVE '  GE'          TO GODK-STATUSKODER                             
149600     CALL CBLTDLI USING GU  WDE6F-PCB DLI-IO-WDE6F1 SSA1                  
149700     MOVE WDE6F-STATUS-CODE TO STATUS-WS                                  
149800     PERFORM IMS-STATUSKONTROLL                                           
149900     .                                                                    
150000     EJECT                                                                
150100 IMS-GU-SUPREF-WDE6F1 SECTION.                                            
150200     MOVE 'IMS-GU-SUPREF-WDE6F1 '  TO CURRENT-IMS-SECTION                 
150300                                                                          
150400     STRING 'WDE6F1  (WDE6F1KY>=' W-WDE6F1KY-MIN-X                        
150500                    '&WDE6F1KY<=' W-WDE6F1KY-MAX-X                        
150600                    '&IDSUPREF =' W-IDSUPREF-F1-X ')'                     
150700            DELIMITED BY SIZE INTO SSA1                                   
150800     MOVE '  GE'          TO GODK-STATUSKODER                             
150900     CALL CBLTDLI USING GU  WDE6F-PCB DLI-IO-WDE6F1 SSA1                  
151000     MOVE WDE6F-STATUS-CODE TO STATUS-WS                                  
151100     PERFORM IMS-STATUSKONTROLL                                           
151200     .                                                                    
151300     EJECT                                                                
151400 IMS-GN-SUPREF-WDE6F1 SECTION.                                            
151500     MOVE 'IMS-GN-SUPREF-WDE6F1 '  TO CURRENT-IMS-SECTION                 
151600                                                                          
151700     STRING 'WDE6F1  (WDE6F1KY>=' W-WDE6F1KY-MIN-X                        
151800                    '&WDE6F1KY<=' W-WDE6F1KY-MAX-X                        
151900                    '&IDSUPREF =' W-IDSUPREF-F1-X ')'                     
152000            DELIMITED BY SIZE INTO SSA1                                   
152100     MOVE '  GEGB'          TO GODK-STATUSKODER                           
152200     CALL CBLTDLI USING GN  WDE6F-PCB DLI-IO-WDE6F1 SSA1                  
152300     MOVE WDE6F-STATUS-CODE TO STATUS-WS                                  
152400     PERFORM IMS-STATUSKONTROLL                                           
152500     .                                                                    
152600     EJECT                                                                
152700 IMS-GNP-VORD  SECTION.                                                   
152800     MOVE 'WDE601   '       TO SSA1                                       
152900     MOVE '  GE'            TO GODK-STATUSKODER                           
153000     CALL CBLTDLI USING GNP WDE6F-PCB DLI-IO-E601 SSA1                    
153100     MOVE WDE6F-STATUS-CODE TO STATUS-WS                                  
153200     PERFORM IMS-STATUSKONTROLL                                           
153300     .                                                                    
153400                                                                          
153500 IMS-GU-WDE401-ESEQ SECTION.                                              
153600     STRING 'WDE401  (WDE4ESEQ =' W-IDPRODNR-ESEQ-X ')'                   
153700            DELIMITED BY SIZE INTO SSA1                                   
153800     MOVE '  GE'            TO GODK-STATUSKODER                           
153900     CALL CBLTDLI USING GU  WDE4-PCB DLI-IO-E401 SSA1                     
154000     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
154100     PERFORM IMS-STATUSKONTROLL                                           
154200     .                                                                    
154300     EJECT                                                                
154400 IMS-GU-VORD  SECTION.                                                    
154500     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-X ')'                        
154600            DELIMITED BY SIZE INTO SSA1                                   
154700     MOVE '  '              TO GODK-STATUSKODER                           
154800     CALL CBLTDLI USING GU  WDE6-PCB DLI-IO-E601 SSA1                     
154900     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
155000     PERFORM IMS-STATUSKONTROLL                                           
155100     .                                                                    
155200                                                                          
155300 IMS-GHU-VORD  SECTION.                                                   
155400     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-X ')'                        
155500            DELIMITED BY SIZE INTO SSA1                                   
155600     MOVE '  '              TO GODK-STATUSKODER                           
155700     CALL CBLTDLI USING GHU WDE6-PCB DLI-IO-E601 SSA1                     
155800     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
155900     PERFORM IMS-STATUSKONTROLL                                           
156000     .                                                                    
156100                                                                          
156200 IMS-REPL-VORD  SECTION.                                                  
156300     MOVE '  '              TO GODK-STATUSKODER                           
156400     CALL CBLTDLI USING REPL WDE6-PCB DLI-IO-E601                         
156500     MOVE WDE6-STATUS-CODE  TO STATUS-WS                                  
156600     PERFORM IMS-STATUSKONTROLL                                           
156700     .                                                                    
156800                                                                          
156900 IMS-GNP-KOLLI SECTION.                                                   
157000     MOVE 'WDE611   '       TO SSA1                                       
157100     MOVE '  GE'            TO GODK-STATUSKODER                           
157200     CALL CBLTDLI USING GNP WDE6-PCB DLI-IO-E611 SSA1                     
157300     MOVE WDE6-STATUS-CODE  TO STATUS-WS                                  
157400     PERFORM IMS-STATUSKONTROLL                                           
157500     .                                                                    
157600                                                                          
157700 IMS-GHU-KOLLI SECTION.                                                   
157800     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-X ')'                        
157900            DELIMITED BY SIZE INTO SSA1                                   
158000     STRING 'WDE611  (IDKOLLI  =' W-IDKOLLI-X ')'                         
158100            DELIMITED BY SIZE INTO SSA2                                   
158200     MOVE '  '              TO GODK-STATUSKODER                           
158300     CALL CBLTDLI USING GHU WDE6-PCB DLI-IO-E611 SSA1 SSA2                
158400     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
158500     PERFORM IMS-STATUSKONTROLL                                           
158600     .                                                                    
158700                                                                          
158800 IMS-GU-WDE611 SECTION.                                                   
158900     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-X ')'                        
159000            DELIMITED BY SIZE INTO SSA1                                   
159100     STRING 'WDE611  (IDKOLLI  =' W-IDKOLLI-X ')'                         
159200            DELIMITED BY SIZE INTO SSA2                                   
159300     MOVE '  '              TO GODK-STATUSKODER                           
159400     CALL CBLTDLI USING GU WDE6-PCB DLI-IO-E611 SSA1 SSA2                 
159500     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
159600     PERFORM IMS-STATUSKONTROLL                                           
159700     .                                                                    
159800                                                                          
159900 IMS-REPL-KOLLI SECTION.                                                  
160000     MOVE '  '              TO GODK-STATUSKODER                           
160100     CALL CBLTDLI USING REPL WDE6-PCB DLI-IO-E611                         
160200     MOVE WDE6-STATUS-CODE  TO STATUS-WS                                  
160300     PERFORM IMS-STATUSKONTROLL                                           
160400     .                                                                    
160500     EJECT                                                                
160510 IMS-ISRT-WDE621 SECTION.                                                 
160520                                                                          
160530     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-X ')'                        
160540          DELIMITED BY SIZE INTO SSA1                                     
160550     STRING 'WDE611  (IDKOLLI  =' W-IDKOLLI-X ')'                         
160560          DELIMITED BY SIZE INTO SSA2                                     
160570     MOVE 'WDE621 ' TO SSA3                                               
160580     MOVE '  II' TO GODK-STATUSKODER                                      
160590     CALL CBLTDLI USING ISRT WDE6-PCB DLI-IO-WDE621 SSA1 SSA2 SSA3        
160591     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
160592     PERFORM IMS-STATUSKONTROLL                                           
160593     .                                                                    
160594     EJECT                                                                
160600 IMS-GHU-4726-ROT SECTION.                                                
160700     STRING 'WLXXDV01(WDGXKEY  =' W-4726-WDGXKEY-ROT-X ')'                
160800            DELIMITED BY SIZE INTO SSA1                                   
160900     MOVE '  ' TO GODK-STATUSKODER                                        
161000     CALL CBLTDLI USING GHU    XXDV-PCB DLI-IO-WLXXDV01 SSA1              
161100     MOVE XXDV-STATUS-CODE TO STATUS-WS                                   
161200     PERFORM IMS-STATUSKONTROLL                                           
161300     .                                                                    
161400                                                                          
161500 IMS-GHNP-4726-UNDERSEG SECTION.                                          
161600     STRING 'WLXXDV11(WDGXKEY  =' W-4726-WDGXKEY-UNDSEG-X ')'             
161700            DELIMITED BY SIZE INTO SSA1                                   
161800     MOVE '  GE'           TO GODK-STATUSKODER                            
161900     CALL CBLTDLI USING GHNP   XXDV-PCB DLI-IO-WLXXDV11 SSA1              
162000     MOVE XXDV-STATUS-CODE TO STATUS-WS                                   
162100     PERFORM IMS-STATUSKONTROLL                                           
162200     .                                                                    
162300                                                                          
162400 IMS-INSERT-4726-UNDERSEG SECTION.                                        
162500     STRING 'WLXXDV01(WDGXKEY  =' W-4726-WDGXKEY-ROT-X ')'                
162600            DELIMITED BY SIZE INTO SSA1                                   
162700     MOVE 'WLXXDV11 '      TO SSA2                                        
162800     MOVE '  ' TO GODK-STATUSKODER                                        
162900     CALL CBLTDLI USING ISRT XXDV-PCB DLI-IO-WLXXDV11 SSA1 SSA2           
163000     MOVE XXDV-STATUS-CODE TO STATUS-WS                                   
163100     PERFORM IMS-STATUSKONTROLL                                           
163200     .                                                                    
163300                                                                          
163400 IMS-INSERT-4727 SECTION.                                                 
163500     STRING 'WLXXDV01(WDGXKEY  =' W-4726-WDGXKEY-ROT-X ')'                
163600            DELIMITED BY SIZE INTO SSA1                                   
163700     STRING 'WLXXDV11(WDGXKEY  =' W-4726-WDGXKEY-UNDSEG-X ')'             
163800            DELIMITED BY SIZE INTO SSA2                                   
163900     MOVE 'WLXXDV21 '      TO SSA3                                        
164000     MOVE '  II'           TO GODK-STATUSKODER                            
164100     CALL CBLTDLI USING ISRT XXDV-PCB DLI-IO-WLXXDV21                     
164200                               SSA1 SSA2 SSA3                             
164300     MOVE XXDV-STATUS-CODE TO STATUS-WS                                   
164400     PERFORM IMS-STATUSKONTROLL                                           
164500     .                                                                    
164600                                                                          
164700 IMS-DLET-4726-UNDERSEG SECTION.                                          
164800     MOVE '  '             TO GODK-STATUSKODER                            
164900     CALL CBLTDLI USING DLET XXDV-PCB DLI-IO-WLXXDV11                     
165000     MOVE XXDV-STATUS-CODE TO STATUS-WS                                   
165100     PERFORM IMS-STATUSKONTROLL                                           
165200     .                                                                    
165300     EJECT                                                                
165400 IMS-ISRT-LOGG SECTION.                                                   
165500     SKIP2                                                                
165600     MOVE 'WLFILA01 ' TO SSA1                                             
165700     MOVE '  II' TO GODK-STATUSKODER                                      
165800     CALL CBLTDLI USING ISRT FILA-PCB WLFILA01 SSA1                       
165900     MOVE FILA-STATUS-CODE TO STATUS-WS                                   
166000     PERFORM IMS-STATUSKONTROLL                                           
166100     .                                                                    
166200     EJECT                                                                
166300 IMS-ISRT-4322-SEGM SECTION.                                              
166400     STRING 'WDG201  (WDGXKEY  =' W-4321-IDHTYP-X ')'                     
166500            DELIMITED BY SIZE INTO SSA1                                   
166600     MOVE 'WDG202  *L' TO SSA2                                            
166700     MOVE '  ' TO GODK-STATUSKODER                                        
166800     CALL CBLTDLI USING ISRT 4322-PCB 4322-WDGX4322 SSA1 SSA2             
166900     MOVE 4322-STATUS-CODE TO STATUS-WS                                   
167000     PERFORM IMS-STATUSKONTROLL                                           
167100     .                                                                    
167200     SKIP2                                                                
167300 IMS-STATUSKONTROLL SECTION.                                              
167400     SET STATUS-IX TO 1                                                   
167500     SEARCH GODK-STATUS                                                   
167600       AT END                                                             
167700         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
167800         DELIMITED BY SIZE INTO FELTEXT                                   
167900         CALL FELLOG                                                      
168000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
168100         CONTINUE                                                         
168200     END-SEARCH                                                           
168300     .                                                                    
168400     EJECT                                                                
