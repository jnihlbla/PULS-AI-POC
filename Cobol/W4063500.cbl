001200**********************************************************                
001400 ID DIVISION.                                                             
001500 PROGRAM-ID.     W4063500.                                                
001600 AUTHOR.         MOGREN STINA.                                            
001700 DATE-WRITTEN.   06/12/07.                                                
001800 DATE-COMPILED.                                                           
002000*    FUNKTION:                                                            
002100*        PROGRAMMET ÄR EN BAKGRUNDS-MPP I PASS-IT MODULEN                 
002101*                                                                         
002110*        PROGRAMMET SKAPAR SAMMA RADER SOM BILL-IT SKULLE                 
002120*        HA GJORT OM SATSRADERNA HADE FAKTURERATS                         
002130*        PRIS=0 PÅ RADERNA GÖR ATT BILLIT INTE TAR EMOT DESSA             
002140*        DÄRFÖR SKAPAS LIKADANA RADER UTANFÖR BILLIT OCH                  
002150*        ANROP GÖRS TILL 4634                                             
002210*                                                                         
002300*        PROGRAMMET STARTAS AV WZ01 FRÅN BUILD-IT  4637                   
002310*        FÖR SATS-DISTRIKT 98                                             
002320*                                                                         
002400*        RADERNA SOM SKA 'FAKTURERAS' SÄNDES MED WZ01                     
002500*                                                                         
002610*        PROGRAMMET LÄSER      WDE4                                       
002660*                              WDB6  DC-REGISTER                          
002700*                                                                         
002800*    INDATA.                                                              
002900*        TRANSAKTION: W40635X                                             
003000*        MID:         WF2104I1                                            
003100*                                                                         
003200*    UTDATA.                                                              
003300*        WZ01-RADER   TILL SAVE-IT                                        
003400*                                                                         
003500     SKIP3                                                                
003600 ENVIRONMENT DIVISION.                                                    
003700                                                                          
003800 DATA DIVISION.                                                           
003900     EJECT                                                                
004000 WORKING-STORAGE SECTION.                                                 
004100 77  IDPGM                       PIC X(08)   VALUE 'W4063500'.            
004200                                                                          
004300*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
004400 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004410                                                                          
004500                                                                          
004600 77  JA                          PIC X       VALUE 'J'.                   
004610 77  YES                         PIC X       VALUE 'Y'.                   
004700 77  NEJ                         PIC X       VALUE 'N'.                   
004710*------                                                                   
004720 77  ANTAL-SEND                  PIC S9(4)   BINARY VALUE ZERO.           
004800                                                                          
005000*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
005200                                                                          
005400                                                                          
005500 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
005600     88  NYCKLAR-OK                          VALUE 'J'.                   
005700     88  NYCKLAR-FEL                         VALUE 'N'.                   
005710                                                                          
006711 77  W-RAD-RAKNARE               PIC 9(5)    VALUE ZERO.                  
006714 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   VALUE +33  COMP-3.           
006715 77  KDRC-DISP                   PIC 9(4)    VALUE ZERO.                  
006718                                                                          
006730 77  WS-IDDC                     PIC X(2)   VALUE SPACE.                  
006731 77  WS-IDDISTR                  PIC S9(5)  VALUE ZERO COMP-3.            
006732 77  WS-IDPRODNR                 PIC S9(7)  VALUE ZERO COMP-3.            
006736                                                                          
006737 01  WS-IDDISTR-F                PIC 9(5)    VALUE ZERO.                  
006739 01  WS-IDPRODNR-F               PIC 9(7)    VALUE ZERO.                  
006740 01  W-ANT                       PIC S9(3)   VALUE ZERO COMP-3.           
006741                                                                          
006742 01  FILLER                      PIC X(16)   VALUE 'WS-SEKTION'.          
006743 01  WS-SEKTION                  PIC X(30)   VALUE SPACE.                 
006744                                                                          
006745 01  WS-REDUIN                   PIC X(30)   VALUE SPACE.                 
006746 01  WS-REDUUT                   PIC X(30)   VALUE SPACE.                 
006747                                                                          
006748 01  WS-NUM11                    PIC 9(11).                               
006749 01  WS-NUM9                     PIC 9(9).                                
006750 01  WS-NUM7                     PIC 9(7).                                
006751 01  WS-NUM5                     PIC 9(5).                                
006752                                                                          
006755 01  WS-IDARTNR-CNTRL            PIC X(2).                                
006756 01  FILLER                      REDEFINES WS-IDARTNR-CNTRL.              
006757     03  WS-REKSIFFR1            PIC X(1).                                
006758     03  WS-REKSIFFR2            PIC X(1).                                
006761                                                                          
006762 01  WS-IDCOM                    PIC S9(9)   VALUE ZERO COMP-3.           
006763 01  FILLER                      PIC X(16)   VALUE 'WS-IDKOLLI'.          
006764 01  WS-IDKOLLI                  PIC S9(5)   VALUE ZERO COMP-3.           
006765                                                                          
006766 01  WS-IDPARTNR                 PIC X(9)    VALUE SPACE.                 
006767 01  FILLER                      REDEFINES WS-IDPARTNR.                   
006768     03  WS-INT                  PIC 9(2).                                
006769     03  FILLER                  PIC 9(7).                                
006770                                                                          
006771 01  WS-IDSKYLT                  PIC X(3)    VALUE SPACE.                 
006772 01  WS-KDSPRAK                  PIC S9      VALUE ZERO COMP-3.           
006773                                                                          
006774 01  WS-TIKLOCK                  PIC S9(9)   VALUE ZERO COMP-3.           
006775 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
006778 01  WS-TTMMSS                   PIC 9(6)    VALUE ZERO.                  
006779 01  WS-DATUM                    PIC 9(8)    VALUE ZERO.                  
006780 01  FILLER                      REDEFINES WS-DATUM.                      
006781     03  WS-SEKEL                PIC 9(2).                                
006782     03  WS-AAMMDD               PIC 9(6).                                
006783                                                                          
006797                                                                          
006800*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
006900 01  GENERELLA-SUBPROGRAM.                                                
007000     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
007100     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
007200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007300     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007400     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
007410     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
007420     03  W009REDU                PIC X(8)    VALUE 'W009REDU'.            
007430     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
007440     03  WZ01RECV                PIC X(8)    VALUE 'WZ01RECV'.            
007500     EJECT                                                                
007510                                                                          
007600*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
007700*01 -COPY WMEDAREA                                                        
007800     SKIP3                                                                
007900 01  MESSAGE-CODES.                                                       
008200     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
008301     EJECT                                                                
008302*                                                                         
008308                                                                          
008309 01  TEST-IDDISTR                PIC 9(5)    COMP-3 VALUE ZERO.           
008316*01  FILLER   -COPY WWDIST19    -RED TEST-IDDISTR.                        
008317     EJECT                                                                
008340                                                                          
008350*01  -COPY WDATAREA                                                       
008400     EJECT                                                                
008500*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
008600*                                                                         
008700 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
008800     SKIP3                                                                
008900*01 -COPY WMSGINIT                                                        
009000     EJECT                                                                
009001                                                                          
009601*                                                                         
009602*    --- AREOR FÖR ANROP TILL WZ01  ------                                
009603 01  FILLER                      PIC X(16)   VALUE 'WZ01-SEND'.           
009604*01  -COPY WZ01SEND                                                       
009605                                                                          
009610                                                                          
009611 01  UT-AREA.                                                             
009612*    03  FILLER -COPY WZ01REQU  -PRE UT-                                  
009613*    03  FILLER -COPY WF2104I1  -PRE WU-                                  
009614     EJECT                                                                
009615                                                                          
009620*    --- AREOR FÖR ANROP FRÅN WZ01                                        
009630 01  FILLER                      PIC X(16)   VALUE 'WZ01-RECV '.          
009640*01  -COPY WZ01RECV                                                       
009650                                                                          
009700*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
009800*                                                                         
009900 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
009910 01  RECV-AREA.                                                           
010000*    03  -COPY WZ01REQU -PRE IN-                                          
010100*    03  MID -COPY WF0201I1   -PRE WF-                                    
010200     EJECT                                                                
011340                                                                          
011400*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
011500*                                                                         
011600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
011700     SKIP3                                                                
011800 01  NYCKLAR-TILL-DLI.                                                    
011909     03  W-IDPURAD-X.                                                     
011910         05  W-IDPURAD           PIC S9(5)   VALUE ZERO COMP-3.           
011911                                                                          
011912     03  W-WDE4ESEQ-X.                                                    
011913         05  W-IDPRODNR-ESEQ     PIC S9(7)   VALUE ZERO COMP-3.           
011914                                                                          
012000* TILL WDB601                                                             
012001     03  W-IDDC-B6-X.                                                     
012002         05 W-IDDC-B6            PIC X(2).                                
012003                                                                          
012012     SKIP2                                                                
012020                                                                          
012100*    --- STATUS-KOD FRÅN IMS                                              
012200 01  STATUS-WS                   PIC XX.                                  
012300     88  SEGMENT-FINNS                       VALUE '  '.                  
012400     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
012500     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
012510     88  SEGMENT-SLUT                        VALUE 'GB'.                  
012600     SKIP2                                                                
012700 01  GODK-STATUSKODER.                                                    
012800     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012900     SKIP3                                                                
013000 01  SSA1                        PIC X(320).                              
013100 01  SSA2                        PIC X(384).                              
013110 01  SSA3                        PIC X(64).                               
013200     EJECT                                                                
013300*    --- IMS FUNKTIONSKODER                                               
013400*01  -COPY W0003                                                          
013600     EJECT                                                                
013610                                                                          
013700*    ---  DLI INPUT-OUTPUT AREA                                           
013800                                                                          
013810 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDE401'.           
013820 01  DLI-IO-WDE401.                                                       
013830*    03  -COPY WDE401                                                     
013840     EJECT                                                                
013850 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDE411'.           
013860 01  DLI-IO-WDE411.                                                       
013870*    03  -COPY WDE411                                                     
013880     EJECT                                                                
014299                                                                          
014300 01  FILLER                    PIC X(16) VALUE 'WDB601 AREA'.             
014310 01   DLI-IO-AREA-B601.                                                   
014320*     03  -COPY WDB601                                                    
014324     EJECT                                                                
014330                                                                          
014400 LINKAGE SECTION.                                                         
014701                                                                          
014702 01  IO-PCB        PIC X.                                                 
014703                                                                          
014704*01  -COPY W0009  -PRE  SAVE-                                             
014705     EJECT                                                                
014711*01  -COPY W0008  -PRE WDE4-                                              
014712     05  FILLER                  PIC X.                                   
014900*01  -COPY W0008  -PRE WDB6-                                              
014901     05  FILLER                  PIC X.                                   
014902     EJECT                                                                
014906 PROCEDURE DIVISION  USING          IO-PCB  SAVE-PCB                      
014908                                            WDE4-PCB                      
014914                                            WDB6-PCB.                     
014916 MAIN SECTION.                                                            
014917     ENTRY 'DLITCBL' USING          IO-PCB  SAVE-PCB                      
014919                                            WDE4-PCB                      
014925                                            WDB6-PCB.                     
015000                                                                          
015400     PERFORM A-INIT                                                       
015401     PERFORM S01-OPEN-WZ01                                                
015410     PERFORM UNTIL  RECV-KDRC > 0                                         
015500       PERFORM B-KOLLA-NYCKLAR                                            
015600       IF NYCKLAR-OK                                                      
016170                                                                          
016218         PERFORM G-SKAPA-SEND-DATA                                        
016250                                                                          
016255       END-IF                                                             
016256       PERFORM S12-RECV-MESSAGE                                           
016257     END-PERFORM                                                          
016258     IF RECV-KDRC > 1                                                     
016259       MOVE 'WZ01-RECV AVSLUTAS FEL..'  TO FELTEXT                        
016260       CALL FELLOG                                                        
016261     END-IF                                                               
016262     MOVE HIGH-VALUE                  TO WU-WF2104I1                      
016263*    AVSLUTSPOST                                                          
016264     PERFORM S02-PUT-RAD-WZ01                                             
016265     PERFORM S03-CLOSE-WZ01                                               
016270*                                                                         
016300     PERFORM S13-RECV-CLOSE                                               
017000     MOVE ZERO TO RETURN-CODE                                             
017100     GOBACK                                                               
017200     .                                                                    
017300     EJECT                                                                
017400 A-INIT SECTION.                                                          
017410     MOVE 'A-INIT'              TO WS-SEKTION                             
017500                                                                          
017600     PERFORM S11-RECV-OPEN                                                
017700     PERFORM S12-RECV-MESSAGE                                             
020200     INITIALIZE                    WU-WF2104I1                            
020210                                                                          
020222                                                                          
020223     ACCEPT DAGENS-DATUM    FROM DATE                                     
020230     ACCEPT WS-TIKLOCK      FROM TIME                                     
020240     MOVE FUNCTION CURRENT-DATE (9:6) TO WS-TTMMSS                        
020300     .                                                                    
020400     EJECT                                                                
020500 B-KOLLA-NYCKLAR SECTION.                                                 
020510     MOVE 'B-KOLLA-NYCKLAR'     TO WS-SEKTION                             
022000                                                                          
022100     MOVE JA                    TO NYCKLAR-SW                             
022200                                                                          
022210     MOVE WF-IDDC               TO WS-IDDC                                
022220                                                                          
022300     MOVE +0         TO W-ANT                                             
022400     INSPECT WF-IDEXCUST(1) TALLYING W-ANT FOR CHARACTERS                 
022410             BEFORE INITIAL ' '                                           
022420     MOVE WF-IDEXCUST(1)(1:W-ANT)   TO WS-IDDISTR-F                       
022450                                                                          
022460     MOVE +0         TO W-ANT                                             
022470     INSPECT WF-IDOPTION(1) TALLYING W-ANT FOR CHARACTERS                 
022480             BEFORE INITIAL ' '                                           
022490     MOVE WF-IDOPTION(1)(1:W-ANT)   TO WS-IDPRODNR-F                      
022491                                                                          
022492     MOVE WS-IDDISTR-F             TO WS-IDDISTR                          
022493     MOVE WS-IDPRODNR-F            TO WS-IDPRODNR                         
022494     MOVE WS-IDPRODNR              TO W-IDPRODNR-ESEQ                     
022500                                                                          
022600     IF NYCKLAR-FEL                                                       
022610       STRING 'NYCKLAR FEL '                                              
022620            DELIMITED BY SIZE INTO FELTEXT                                
023000       CALL FELLOG                                                        
023100                                                                          
023200     END-IF                                                               
023300     .                                                                    
023500     EJECT                                                                
026071 G-SKAPA-SEND-DATA  SECTION.                                              
026072     MOVE 'G-SKAPA-SEND-DATA'   TO WS-SEKTION                             
026073                                                                          
026083     PERFORM GA-RADINFO                                                   
026089     PERFORM S02-PUT-RAD-WZ01                                             
026090     .                                                                    
026091     EJECT                                                                
026092 GA-RADINFO  SECTION.                                                     
026093     MOVE 'GA-RADINFO'          TO WS-SEKTION                             
026094                                                                          
026095*  SKAPA DISTRIKT                                                         
026096     MOVE WS-IDDISTR            TO TEST-IDDISTR                           
026097     IF WS-IDDC NOT = W-IDDC-B6                                           
026098        MOVE WS-IDDC            TO W-IDDC-B6                              
026099        PERFORM IMS-GU-WDB601                                             
026100     END-IF                                                               
026101                                                                          
026102     MOVE 1                     TO UT-REQU-IDMSGVER                       
026103     MOVE SPACE                 TO UT-REQU-KDPGMACT                       
026104     MOVE 'W4063500'            TO UT-REQU-IDUSER                         
026105                                                                          
026116     MOVE WF-IDBUNDLE           TO WU-IDBUNDLE                            
026125                                                                          
026126     MOVE DAGENS-DATUM          TO WS-AAMMDD                              
026136     MOVE 20                    TO WS-SEKEL                               
026138     MOVE WS-DATUM              TO WU-DAEXDAT                             
026144*------------------------                                                 
026145     MOVE WS-DATUM              TO WU-DAFINDOC                            
026147     MOVE WS-TTMMSS             TO WU-TIEXTID                             
026150     MOVE WF-KDVALISO           TO WU-KDVALISO                            
026160     MOVE WF-IDPARTNR           TO WU-IDPARTNR                            
026161     PERFORM IMS-GU-WDE401-ESEQ                                           
026162     IF SEGMENT-SAKNAS                                                    
026163       MOVE ZERO                TO KORD-SUORDV                            
026164     END-IF                                                               
026165     MOVE KORD-IDORDNR5         TO WU-IDFINDOC                            
026166*    ORDERNR BLIR I ST.F. FAKTURANUMMER FRÅN BILL-IT                      
026170     MOVE KORD-SUORDV           TO WU-SUNTO-PART                          
026180     MOVE ZERO                  TO WU-SUVAT-BILLIT-TOT                    
026190     MOVE KORD-SUORDV           TO WU-SUBTO-TOT                           
026200     MOVE WF-IDBUNDLE           TO WU-IDBUNDLE                            
026300     MOVE WF-IDEXCUST(1)        TO WU-IDEXCUST-1                          
026400     MOVE WF-IDEXCUST(2)        TO WU-IDEXCUST-2                          
026500     MOVE WF-IDOPTION(1)        TO WU-IDOPTION-1                          
026600     MOVE WF-IDOPTION(2)        TO WU-IDOPTION-2                          
026610     MOVE WF-IDOPTION(3)        TO WU-IDOPTION-3                          
026620     MOVE WF-IDOPTION(4)        TO WU-IDOPTION-4                          
026630     MOVE WF-IDOPTION(5)        TO WU-IDOPTION-5                          
026640     MOVE WF-IDARTNR-FINANCE    TO WU-IDARTNR-FINANCE                     
026641     MOVE WF-BEART              TO WU-BEART                               
026642     MOVE WF-VKARTNTO           TO WU-VKARTNTO                            
026643     MOVE ZERO                  TO WU-SUBTO                               
026644     MOVE ZERO                  TO WU-SUNTO                               
026645     MOVE ZERO                  TO WU-SUVAT-BILLIT                        
026646     MOVE WF-KDVALISO           TO WU-KDVALISO-BET                        
026647     MOVE 1.00                  TO WU-PRKURS-BET                          
026648     MOVE 1.00                  TO WU-PRKURS                              
026649     MOVE 1.00                  TO WU-PRKURS-FAKBET                       
026650     .                                                                    
026660     EJECT                                                                
027077 S01-OPEN-WZ01 SECTION.                                                   
027078*    POSTER FRÅN 4635 TILL 4634                                           
027079     MOVE 'S01-OPEN-WZ01'            TO WS-SEKTION                        
027080                                                                          
027081     MOVE 'OPEN'                     TO SEND-KDFUNC                       
027082     MOVE 'CARPARTS.PULS.SAVEIT'     TO SEND-ADDISPABS                    
027083     MOVE SPACE                      TO SEND-ADDISPABS-RETURN             
027084                                                                          
027085     CALL WZ01SEND   USING      SEND-CONTROL-AREA                         
027086                                SEND-OPEN-AREA                            
027087     IF SEND-KDRC > 0                                                     
027088       MOVE SEND-KDRC           TO KDRC-DISP                              
027089       STRING 'WZ01SEND-OPEN RC-ERR = ' KDRC-DISP                         
027090            DELIMITED BY SIZE INTO FELTEXT                                
027091       CALL FELLOG                                                        
027092     ELSE                                                                 
027093       MOVE SEND-IDCOM               TO WS-IDCOM                          
027094     END-IF                                                               
027095     .                                                                    
027096     EJECT                                                                
027097 S02-PUT-RAD-WZ01 SECTION.                                                
027098     MOVE 'S02-PUT-RAD-WZ01'         TO WS-SEKTION                        
027099*--------------                                                           
027100     ADD 1 TO ANTAL-SEND                                                  
027101**   DISPLAY 'W4063500 ' ANTAL-SEND ' ' UT-AREA(1:30)                     
027102                                                                          
027103     MOVE 'PUT'                 TO SEND-KDFUNC                            
027104     MOVE LENGTH OF UT-AREA     TO SEND-KVDLEN                            
027105                                                                          
027106     CALL WZ01SEND   USING      SEND-CONTROL-AREA                         
027107                                SEND-KVDLEN                               
027108                                UT-AREA                                   
027109     IF SEND-KDRC > 1                                                     
027110       MOVE SEND-KDRC           TO KDRC-DISP                              
027111       STRING 'WZ01SEND-PUT RC-ERR = ' KDRC-DISP                          
027112            DELIMITED BY SIZE INTO FELTEXT                                
027113       CALL FELLOG                                                        
027114     END-IF                                                               
027116     .                                                                    
027117     EJECT                                                                
027118 S03-CLOSE-WZ01  SECTION.                                                 
027119     MOVE 'S03-CLOSE-WZ01'      TO WS-SEKTION                             
027120                                                                          
027121     MOVE 'CLOSE'               TO SEND-KDFUNC                            
027122                                                                          
027123     CALL WZ01SEND   USING      SEND-CONTROL-AREA                         
027124     IF SEND-KDRC > 0                                                     
027125       MOVE SEND-KDRC           TO KDRC-DISP                              
027126       STRING 'WZ01SEND-CLOSE RC-ERR = ' KDRC-DISP                        
027127            DELIMITED BY SIZE INTO FELTEXT                                
027128       CALL FELLOG                                                        
027129     END-IF                                                               
027130     .                                                                    
027140     EJECT                                                                
027147 S05-NUM-TEXT  SECTION.                                                   
027148     INSPECT WS-REDUIN REPLACING LEADING ZERO BY SPACE                    
027149     CALL W009REDU USING WS-REDUIN WS-REDUUT                              
027150     .                                                                    
027151     EJECT                                                                
027351 S11-RECV-OPEN SECTION.                                                   
027352     MOVE 'S11-RECV-OPEN'          TO WS-SEKTION                          
027353*    POSTER FRÅN W4063700                                                 
027354                                                                          
027355     MOVE 'OPEN'                   TO RECV-KDFUNC                         
027356     MOVE 'CARPARTS.PULS.PASSIT'   TO RECV-ADDISPABS                      
027357                                                                          
027358     CALL WZ01RECV USING           RECV-CONTROL-AREA                      
027359                                   RECV-OPEN-AREA                         
027360                                                                          
027361     IF RECV-KDRC > 0                                                     
027362      MOVE RECV-KDRC               TO KDRC-DISP                           
027363      STRING 'WZ01RECV OPEN ERROR RC= ' KDRC-DISP                         
027364        DELIMITED BY SIZE INTO FELTEXT                                    
027365      CALL FELLOG                                                         
027366     END-IF                                                               
027367     .                                                                    
027370     EJECT                                                                
027400 S12-RECV-MESSAGE SECTION.                                                
027500     MOVE 'S12-RECV-MESSAGE'      TO WS-SEKTION                           
027600                                                                          
027610     MOVE 'GET'                    TO RECV-KDFUNC                         
027620     MOVE LENGTH OF RECV-AREA      TO RECV-KVDLEN                         
027630     CALL WZ01RECV USING RECV-CONTROL-AREA                                
027640                         RECV-KVDLEN                                      
027651                         RECV-AREA                                        
027660*                                                                         
027670     IF RECV-KDRC > 1                                                     
027680       MOVE RECV-KDRC            TO KDRC-DISP                             
027690       STRING 'WZ01RECV GET  ERROR RC= ' KDRC-DISP                        
027691         DELIMITED BY SIZE INTO FELTEXT                                   
027693       CALL FELLOG                                                        
027694     END-IF                                                               
027700     .                                                                    
027800     EJECT                                                                
027900 S13-RECV-CLOSE SECTION.                                                  
028000     MOVE 'S13-RECV-CLOSE'      TO WS-SEKTION                             
028100                                                                          
028110     MOVE 'CLOSE'                TO RECV-KDFUNC                           
028120     CALL WZ01RECV     USING        RECV-CONTROL-AREA                     
028130*                                                                         
028140     IF RECV-KDRC > 0                                                     
028150       MOVE RECV-KDRC            TO KDRC-DISP                             
028160       STRING 'WZ01RECV CLOSE ERROR RC= ' KDRC-DISP                       
028170         DELIMITED BY SIZE INTO FELTEXT                                   
028190       CALL FELLOG                                                        
028191     END-IF                                                               
028200     .                                                                    
028300     EJECT                                                                
030700* --- IMS SEKTIONER ---                                                   
030800     SKIP3                                                                
032924 IMS-GU-WDE401-ESEQ  SECTION.                                             
032925                                                                          
032926     STRING 'WDE401  (WDE4ESEQ =' W-WDE4ESEQ-X ')'                        
032927          DELIMITED BY SIZE INTO SSA1                                     
032928     MOVE '  GE' TO GODK-STATUSKODER                                      
032929     CALL CBLTDLI USING GU WDE4-PCB DLI-IO-WDE401 SSA1                    
032930     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
032931     PERFORM IMS-STATUSKONTROLL                                           
032932     .                                                                    
032940     SKIP3                                                                
033090 IMS-GU-WDB601    SECTION.                                                
033091     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
033092          DELIMITED BY SIZE INTO SSA1                                     
033093     MOVE '    ' TO GODK-STATUSKODER                                      
033094     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
033095     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
033096     PERFORM IMS-STATUSKONTROLL                                           
033097     IF SEGMENT-SAKNAS                                                    
033098         MOVE SPACE TO DCS-KDDC                                           
033099     END-IF                                                               
033100     .                                                                    
033111 IMS-STATUSKONTROLL SECTION.                                              
033120                                                                          
033200     SET STATUS-IX TO 1                                                   
033300     SEARCH GODK-STATUS                                                   
033400       AT END                                                             
033500         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
033600         DELIMITED BY SIZE INTO FELTEXT                                   
033700         CALL FELLOG                                                      
033800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
033900         CONTINUE                                                         
034000     END-SEARCH                                                           
034100     .                                                                    
